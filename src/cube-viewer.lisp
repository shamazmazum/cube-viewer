(in-package :cube-viewer)

(declaim (type (simple-array single-float (#.(* 3 14))) +cube+))
(alex:define-constant +cube+
    (map '(vector single-float)
         #'identity
         '(1.0  1.0  1.0
           -1.0  1.0  1.0
           1.0 -1.0  1.0
           -1.0 -1.0  1.0
           -1.0 -1.0 -1.0
           -1.0  1.0  1.0
           -1.0  1.0 -1.0
           1.0  1.0  1.0
           1.0  1.0 -1.0
           1.0 -1.0  1.0
           1.0 -1.0 -1.0
           -1.0 -1.0 -1.0
           1.0  1.0 -1.0
           -1.0  1.0 -1.0))
  :test #'equalp)

(declaim (type varjo.internals:vertex-stage *vertex-shader*))
(defparameter *vertex-shader*
  (varjo:make-stage
   :vertex
   '((vertex-in :vec3))
   '((transform :mat4)
     (scale     :float))
   '(:450)
   '((let ((coord (* vertex-in scale)))
     (values
      (* transform (vari:vec4 coord 1.0))
      coord)))))

(declaim (type varjo.internals:fragment-stage *fragment-shader*))
(defparameter *fragment-shader*
  (varjo:make-stage
   :fragment
   '((vertex-in :vec3))
   '((smpl      :sampler-3d)
     (threshold :float))
   '(:450)
   '((let* ((coord (+ 0.5 (* 0.5 vertex-in)))
            (density (aref (vari:texture smpl coord) 0)))
       (if (<= density threshold)
           (vari:vec3 0.1)
           (vari:vec3 0.9176 0.7137 0.4627))))))

(defparameter *compiled-shaders*
  (varjo:rolling-translate
   (list *vertex-shader*
         *fragment-shader*)))

(sera:-> array->gl
         ((simple-array single-float (*)))
         (values gl:gl-array &optional))
(defun array->gl (array)
  (let ((gl-array (gl:alloc-gl-array :float (length array))))
    (loop for i from 0 by 1
          for x across array do
          (setf (gl:glaref gl-array i) x))
    gl-array))

(defmacro with-gl-array ((var lisp-array) &rest body)
  `(let ((,var (array->gl ,lisp-array)))
     (unwind-protect
          (progn ,@body)
       (gl:free-gl-array ,var))))

(sera:-> load-data
         ((or pathname string)
          alex:positive-fixnum
          alex:positive-fixnum
          alex:positive-fixnum)
         (values (simple-array (unsigned-byte 8) (* * *)) &optional))
(defun load-data (name w h d)
  (let ((array (make-array (list w h d)
                           :element-type '(unsigned-byte 8))))
    (with-open-file (input name :element-type '(unsigned-byte 8))
      (read-sequence (aops:flatten array) input))
    array))

(defstruct gl-state
  (program       -1 :type fixnum)
  (vao           -1 :type fixnum)
  (vertex-buffer -1 :type fixnum)
  (texture       -1 :type fixnum)
  (transform-loc -1 :type fixnum)
  (sampler-loc   -1 :type fixnum)
  (threshold-loc -1 :type fixnum)
  (scale-loc     -1 :type fixnum)
  (threshold    0d0 :type double-float)
  (scale        1d0 :type double-float))

(defstruct camera
  (position (rtg-math.vector3:make 0.0 0.0 0.0)
            :type rtg-math.types:vec3)
  (fov      90.0 :type single-float)
  (ϕ        0d0  :type double-float)
  (ψ        0d0  :type double-float)
  (dist     3d0  :type double-float))

(sera:-> world->screen
         (camera alex:positive-fixnum alex:positive-fixnum)
         (values rtg-math.types:mat4 &optional))
(defun world->screen (camera width height)
  (rtg-math.matrix4:*
   (rtg-math.projection:perspective
    (float width)
    (float height)
    0.1 10.0
    (camera-fov camera))
   (rtg-math.matrix4:look-at
    (rtg-math.vector3:make 0.0 1.0 0.0)
    (camera-position camera)
    (rtg-math.vector3:make 0.0 0.0 0.0))))

(defun draw-cube (area gl-state camera)
  (declare (type gtk:gtk-gl-area area)
           (type gl-state gl-state)
           (type camera camera))
  (gl:clear-color 1.0 1.0 1.0 0.0)
  (gl:clear :color-buffer-bit)
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :depth-buffer-bit)
  (gl:enable :depth-test :cull-face)

  (gl:use-program (gl-state-program gl-state))
  (gl:uniformi (gl-state-sampler-loc gl-state) 0)
  (gl:uniformf (gl-state-threshold-loc gl-state)
               (float (gl-state-threshold gl-state) 0.0))
  (gl:uniformf (gl-state-scale-loc gl-state)
               (float (gl-state-scale gl-state) 0.0))
  (let ((world->screen
         (let* ((allocation (gtk:gtk-widget-get-allocation area))
                (width  (gdk:gdk-rectangle-width  allocation))
                (height (gdk:gdk-rectangle-height allocation)))
           (world->screen camera width height))))
    (gl:uniform-matrix
     (gl-state-transform-loc gl-state)
     4 (vector world->screen) nil))

  (gl:active-texture :texture0)
  (gl:bind-texture :texture-3d (gl-state-texture gl-state))

  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer (gl-state-vertex-buffer gl-state))
  (gl:vertex-attrib-pointer 0 3 :float nil 0
                            (cffi:null-pointer))
  (gl:draw-arrays :triangle-strip 0 (/ (length +cube+) 3))
  (gl:disable-vertex-attrib-array 0))

(sera:-> prepare-rendering
         (gtk:gtk-gl-area
          gl-state
          (simple-array (unsigned-byte 8) (* * *)))
         (values &optional))
(defun prepare-rendering (area gl-state density)
  (gtk:gtk-gl-area-make-current area)
  (gtk:gtk-gl-area-get-error area)

  (with-accessors ((vao           gl-state-vao)
                   (vertex-buffer gl-state-vertex-buffer)
                   (program       gl-state-program)
                   (texture       gl-state-texture))
      gl-state
    ;; Fill vertices
    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)
    (setf vertex-buffer (gl:gen-buffer))
    (gl:bind-buffer :array-buffer vertex-buffer)
    (with-gl-array (vertex-array +cube+)
      (gl:buffer-data :array-buffer :static-draw
                      vertex-array))

    ;; Create texture
    (setf texture (gl:gen-texture))
    (gl:bind-texture :texture-3d texture)
    (gl:tex-image-3d :texture-3d 0 :red
                     (array-dimension density 0)
                     (array-dimension density 1)
                     (array-dimension density 2)
                     0 :red :unsigned-byte (aops:flatten density))
    (gl:tex-parameter :texture-3d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-3d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-3d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-3d :texture-wrap-t :clamp-to-edge)
    (gl:tex-parameter :texture-3d :texture-wrap-r :clamp-to-edge)

    ;; Create program
    (setf program (gl:create-program))
    (let ((vertex-shader   (gl:create-shader :vertex-shader))
          (fragment-shader (gl:create-shader :fragment-shader)))
      (gl:shader-source vertex-shader
                        (list (varjo:glsl-code (first *compiled-shaders*))))
      (gl:shader-source fragment-shader
                        (list (varjo:glsl-code (second *compiled-shaders*))))
      (gl:compile-shader vertex-shader)
      (gl:compile-shader fragment-shader)
      (gl:attach-shader program vertex-shader)
      (gl:attach-shader program fragment-shader)
      (gl:link-program program)
      (gl:detach-shader  program vertex-shader)
      (gl:detach-shader program fragment-shader)
      (gl:delete-shader vertex-shader)
      (gl:delete-shader fragment-shader))

    ;; Check program status
    (let ((status (gl:get-program program :link-status)))
      (unless status
        (error "Program linkage failure")))

    ;; Set locations of uniforms
    (setf
     (gl-state-transform-loc gl-state)
     (gl:get-uniform-location program "TRANSFORM")
     (gl-state-sampler-loc gl-state)
     (gl:get-uniform-location program "SAMPLER")
     (gl-state-threshold-loc gl-state)
     (gl:get-uniform-location program "THRESHOLD")
     (gl-state-scale-loc gl-state)
     (gl:get-uniform-location program "SCALE")))
  (values))

(defun cleanup (area gl-state)
  (declare (type gtk:gtk-gl-area area)
           (type gl-state gl-state))
  (gtk:gtk-gl-area-make-current area)
  (gtk:gtk-gl-area-get-error area)
  (gl:delete-buffers (list (gl-state-vertex-buffer gl-state)))
  (gl:delete-texture (gl-state-texture gl-state))
  (gl:delete-vertex-arrays (list (gl-state-vao gl-state)))
  (gl:delete-program (gl-state-program gl-state)))

(sera:-> make-scale
         ((member :horizontal :vertical)
          double-float double-float double-float double-float)
         (values gtk:gtk-scale &optional))
(defun make-scale (orientation value start stop step)
  (make-instance 'gtk:gtk-scale
                 :orientation orientation
                 :digits 3
                 :adjustment (make-instance 'gtk:gtk-adjustment
                                            :value          value
                                            :lower          start
                                            :upper          stop
                                            :step-increment step)))

(sera:-> update-camera-position
         (camera)
         (values camera &optional))
(defun update-camera-position (camera)
  (let ((ϕ (camera-ϕ camera))
        (ψ (camera-ψ camera))
        (dist (camera-dist camera)))
    (setf (camera-position camera)
          (rtg-math.vector3:make
           (float (* dist (cos ψ) (cos ϕ)) 0.0)
           (float (* dist (sin ψ)) 0.0)
           (float (* dist (cos ψ) (sin ϕ)) 0.0)))
    camera))

(defun run (name side)
  (declare (type (or pathname string) name)
           (type alex:positive-fixnum side))
  (let ((density-data (load-data name side side side)))
    (gtk:within-main-loop
      (let ((window (gtk:gtk-window-new :toplevel))
            (area (make-instance 'gtk:gtk-gl-area))
            (main-box (make-instance 'gtk:gtk-box :orientation :vertical))
            (control-box (make-instance 'gtk:gtk-box :orientation :horizontal))
            (gl-state (make-gl-state))
            (camera (make-camera))
            (threshold-scale (make-scale :horizontal 0d0 0d0 1d0 5d-2))
            (scale-scale (make-scale :horizontal 1d0 0d0 1d0 5d-2))
            (ϕ-scale (make-scale :horizontal  0d0 0d0 (* 2 pi) 1d-1))
            (ψ-scale (make-scale :horizontal  0d0 (- pi) pi 1d-1)))
        (setf (gl-state-threshold gl-state)
              (gtk:gtk-range-get-value threshold-scale))
        (update-camera-position camera)
        (gobject:g-signal-connect
         window "destroy"
         (lambda (widget)
           (declare (ignore widget))
           (gtk:leave-gtk-main)))
        (gobject:g-signal-connect
         area "render"
         (lambda (area context)
           (declare (ignore context))
           (draw-cube area gl-state camera)
           nil))
        (gobject:g-signal-connect
         area "realize"
         (lambda (widget)
           (declare (ignore widget))
           (prepare-rendering area gl-state density-data)))
        (gobject:g-signal-connect
         area "unrealize"
         (lambda (widget)
           (declare (ignore widget))
           (cleanup area gl-state)))
        (gobject:g-signal-connect
         threshold-scale "value-changed"
         (lambda (scale)
           (setf (gl-state-threshold gl-state)
                 (gtk:gtk-range-get-value scale))
           (gtk:gtk-gl-area-queue-render area)))
        (gobject:g-signal-connect
         scale-scale "value-changed"
         (lambda (scale)
           (setf (gl-state-scale gl-state)
                 (gtk:gtk-range-get-value scale))
           (gtk:gtk-gl-area-queue-render area)))
        (gobject:g-signal-connect
         ϕ-scale "value-changed"
         (lambda (scale)
           (setf (camera-ϕ camera)
                 (gtk:gtk-range-get-value scale))
           (update-camera-position camera)
           (gtk:gtk-gl-area-queue-render area)))
        (gobject:g-signal-connect
         ψ-scale "value-changed"
         (lambda (scale)
           (setf (camera-ψ camera)
                 (gtk:gtk-range-get-value scale))
           (update-camera-position camera)
           (gtk:gtk-gl-area-queue-render area)))
        (gtk:gtk-box-pack-start main-box area)
        (gtk:gtk-box-pack-end main-box control-box :expand nil)
        (let ((misc-box (make-instance 'gtk:gtk-box :orientation :vertical)))
          (gtk:gtk-box-pack-start misc-box (make-instance 'gtk:gtk-label :label "Threshold"))
          (gtk:gtk-box-pack-start misc-box threshold-scale)
          (gtk:gtk-box-pack-start misc-box (make-instance 'gtk:gtk-label :label "Scale"))
          (gtk:gtk-box-pack-start misc-box scale-scale)
          (gtk:gtk-box-pack-start control-box misc-box))
        (let ((position-box (make-instance 'gtk:gtk-box :orientation :vertical)))
          (gtk:gtk-box-pack-start position-box (make-instance 'gtk:gtk-label :label "Φ"))
          (gtk:gtk-box-pack-start position-box ϕ-scale)
          (gtk:gtk-box-pack-start position-box (make-instance 'gtk:gtk-label :label "Ψ"))
          (gtk:gtk-box-pack-start position-box ψ-scale)
          (gtk:gtk-box-pack-start control-box position-box))
        (gtk:gtk-container-add window main-box)
        (gtk:gtk-widget-show-all window)))))
