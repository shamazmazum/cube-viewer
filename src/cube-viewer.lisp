(in-package :cube-viewer)

(define-condition fatal-error (simple-error)
  ()
  (:documentation "Unrecoverable error"))

(declaim (type varjo.internals:vertex-stage *vertex-shader*))
(defparameter *vertex-shader*
  (varjo:make-stage
   :vertex
   '((vertex-in :vec3)
     (normal-in :vec3))
   '((transform :mat4)
     (scale     :float))
   '(:450)
   '((let ((coord (* vertex-in scale)))
     (values
      (* transform (vari:vec4 coord 1.0)) ; gl_Position
      coord                               ; Vertex coordinate (world system)
      normal-in)))))                      ; Normal coordinate (world system)

(declaim (type varjo.internals:fragment-stage *fragment-shader*))
(defparameter *fragment-shader*
  (varjo:make-stage
   :fragment
   '((vertex-in   :vec3)
     (normal-in   :vec3))
   '((smpl        :sampler-3d)
     (threshold   :float)
     (light       :vec3)
     (color-solid :vec3)
     (color-void  :vec3))
   '(:450)
   '((let* ((coord (+ 0.5 (* 0.5 vertex-in)))
            (density (aref (vari:texture smpl coord) 0))
            (color (if (<= density threshold)
                       color-void color-solid))
            (norm-light (vari:normalize light)))
       (+ 0.1 ; Ambient light
          (* 0.9 color ; Diffused light
             (vari:clamp
              (vari:dot norm-light normal-in)
              0.0 1.0)))))))

(defparameter *compiled-shaders*
  (varjo:rolling-translate
   (list *vertex-shader*
         *fragment-shader*)))

(sera:-> array->gl
         ((simple-array single-float (*)))
         (values gl:gl-array &optional))
(defun array->gl (array)
  "Convert one dimensional lisp array of floats to foreign array"
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
  "Load density data from a raw file"
  (let ((array (make-array (list w h d)
                           :element-type '(unsigned-byte 8))))
    (with-open-file (input name :element-type '(unsigned-byte 8))
      (unless (= (file-length input)
                 (* w h d))
        (error 'fatal-error
               :format-control "File size mismatch: expected ~d, got ~d"
               :format-arguments (list (* w h d) (file-length input))))
      (read-sequence (aops:flatten array) input))
    array))

(deftype dvec3 () '(simple-array double-float (3)))
(sera:-> dvec3
         (double-float double-float double-float)
         (values dvec3 &optional))
(defun dvec3 (x y z)
  (make-array 3
              :element-type 'double-float
              :initial-contents (list x y z)))

(defstruct gl-state
  (program       -1 :type fixnum)
  (vao           -1 :type fixnum)
  (vertex-buffer -1 :type fixnum)
  (normal-buffer -1 :type fixnum)
  (texture       -1 :type fixnum)
  (transform-loc -1 :type fixnum)
  (sampler-loc   -1 :type fixnum)
  (threshold-loc -1 :type fixnum)
  (scale-loc     -1 :type fixnum)
  (light-loc     -1 :type fixnum)
  (solid-loc     -1 :type fixnum)
  (void-loc      -1 :type fixnum)
  (solid-color   (dvec3 0.9176d0 0.7137d0 0.4627d0)
                    :type dvec3)
  (void-color    (dvec3 0.1d0 0.1d0 0.1d0)
                    :type dvec3)
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
  "Return world -> screen projection matrix. WIDTH and HEIGHT are
dimensions of the GtkGLArea widget."
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
  ;; Clear buffers
  (gl:clear-color 1.0 1.0 1.0 0.0)
  (gl:clear :color-buffer-bit)
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :depth-buffer-bit)
  (gl:enable :depth-test :cull-face)

  ;; Set uniforms
  (gl:use-program (gl-state-program gl-state))
  (gl:uniformi (gl-state-sampler-loc gl-state) 0)
  (gl:uniformf (gl-state-threshold-loc gl-state)
               (float (gl-state-threshold gl-state) 0.0))
  (gl:uniformf (gl-state-scale-loc gl-state)
               (float (gl-state-scale gl-state) 0.0))
  (apply #'gl:uniformf (gl-state-light-loc gl-state)
         (map 'list #'identity (camera-position camera)))
  (apply #'gl:uniformf (gl-state-solid-loc gl-state)
         (map 'list (alex:rcurry #'float 0.0)
              (gl-state-solid-color gl-state)))
  (apply #'gl:uniformf (gl-state-void-loc gl-state)
         (map 'list (alex:rcurry #'float 0.0)
              (gl-state-void-color gl-state)))
  (let ((world->screen
         (let* ((allocation (gtk:gtk-widget-get-allocation area))
                (width  (gdk:gdk-rectangle-width  allocation))
                (height (gdk:gdk-rectangle-height allocation)))
           (world->screen camera width height))))
    (gl:uniform-matrix
     (gl-state-transform-loc gl-state)
     4 (vector world->screen) nil))

  ;; Bind texture
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-3d (gl-state-texture gl-state))

  ;; Draw
  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer (gl-state-vertex-buffer gl-state))
  (gl:vertex-attrib-pointer 0 3 :float nil 0
                            (cffi:null-pointer))
  (gl:enable-vertex-attrib-array 1)
  (gl:bind-buffer :array-buffer (gl-state-normal-buffer gl-state))
  (gl:vertex-attrib-pointer 1 3 :float nil 0
                            (cffi:null-pointer))
  (gl:draw-arrays :triangles 0 (/ (length +cube-vertices+) 3))
  (gl:disable-vertex-attrib-array 1)
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
                   (normal-buffer gl-state-normal-buffer)
                   (program       gl-state-program)
                   (texture       gl-state-texture))
      gl-state
    ;; Create vertex array
    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)

    ;; Fill vertices
    (setf vertex-buffer (gl:gen-buffer))
    (gl:bind-buffer :array-buffer vertex-buffer)
    (with-gl-array (vertex-array +cube-vertices+)
      (gl:buffer-data :array-buffer :static-draw
                      vertex-array))

    ;; Fill normals
    (setf normal-buffer (gl:gen-buffer))
    (gl:bind-buffer :array-buffer normal-buffer)
    (with-gl-array (vertex-array +cube-normals+)
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
        (error 'fatal-error :format-control "Program linkage failure")))

    ;; Set locations of uniforms
    (setf
     (gl-state-transform-loc gl-state)
     (gl:get-uniform-location program "TRANSFORM")
     (gl-state-sampler-loc gl-state)
     (gl:get-uniform-location program "SAMPLER")
     (gl-state-threshold-loc gl-state)
     (gl:get-uniform-location program "THRESHOLD")
     (gl-state-scale-loc gl-state)
     (gl:get-uniform-location program "SCALE")
     (gl-state-light-loc gl-state)
     (gl:get-uniform-location program "LIGHT")
     (gl-state-solid-loc gl-state)
     (gl:get-uniform-location program "COLOR_SOLID")
     (gl-state-void-loc gl-state)
     (gl:get-uniform-location program "COLOR_VOID")))
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

(sera:-> make-color-button
         ((simple-array double-float (3)))
         (values gtk:gtk-color-button &optional))
(defun make-color-button (color)
  (make-instance 'gtk:gtk-color-button
                 :rgba (gdk:make-gdk-rgba :red   (aref color 0)
                                          :green (aref color 1)
                                          :blue  (aref color 2)
                                          :alpha 1d0)))

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
  "Run cube viewer. NAME is a name of raw file with XCT
densities. SIDE is a side of cubic array stored in that file. Elements
of the array must be 8 bit unsigned values."
  (declare (type (or pathname string) name)
           (type alex:positive-fixnum side))
  (let ((density-data (load-data name side side side))
        (gl-state (make-gl-state))
        (camera (make-camera)))
    (gtk:within-main-loop
      (let ((window (make-instance 'gtk:gtk-window
                                   :type  :toplevel
                                   :title (format nil "cube-viewer: ~a"
                                                  (pathname-name (pathname name)))))
            (area (make-instance 'gtk:gtk-gl-area))
            (main-box (make-instance 'gtk:gtk-box :orientation :vertical))
            (control-box (make-instance 'gtk:gtk-box :orientation :horizontal))
            (threshold-scale (make-scale :horizontal
                                         (gl-state-threshold gl-state)
                                         0d0 1d0 5d-2))
            (scale-scale (make-scale :horizontal
                                     (gl-state-scale gl-state)
                                     0d0 1d0 5d-2))
            (ϕ-scale (make-scale :horizontal
                                 (camera-ϕ camera)
                                 0d0 (* 2 pi) 1d-1))
            (ψ-scale (make-scale :horizontal
                                 (camera-ψ camera)
                                 (- (/ pi 2)) (+ (/ pi 2)) 1d-1))
            (solid-color (make-color-button (gl-state-solid-color gl-state)))
            (void-color  (make-color-button (gl-state-void-color  gl-state))))
        (flet ((scale-handler (scale)
                 (let ((value (gtk:gtk-range-get-value scale)))
                   (cond
                     ((eq scale threshold-scale)
                      (setf (gl-state-threshold gl-state) value))
                     ((eq scale scale-scale)
                      (setf (gl-state-scale gl-state) value))
                     ((eq scale ϕ-scale)
                      (setf (camera-ϕ camera) value)
                      (update-camera-position camera))
                     ((eq scale ψ-scale)
                      (setf (camera-ψ camera) value)
                      (update-camera-position camera))
                     (t (error "This never happens"))))
                 (gtk:gtk-gl-area-queue-render area))
               (color-button-handler (button)
                 (let* ((color (gtk:gtk-color-chooser-get-rgba button))
                        (vector (dvec3 (gdk:gdk-rgba-red   color)
                                       (gdk:gdk-rgba-green color)
                                       (gdk:gdk-rgba-blue  color))))
                   (cond
                     ((eq button solid-color)
                      (setf (gl-state-solid-color gl-state) vector))
                     ((eq button void-color)
                      (setf (gl-state-void-color gl-state) vector))
                     (t (error "This never happens"))))
                 (gtk:gtk-gl-area-queue-render area)))
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
          (mapc
           (alex:rcurry
            #'gobject:g-signal-connect
            "value-changed"
            #'scale-handler)
           (list threshold-scale scale-scale ϕ-scale ψ-scale))
          (mapc
           (alex:rcurry
            #'gobject:g-signal-connect
            "color-set"
            #'color-button-handler)
           (list solid-color void-color)))

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
        (let ((color-box (make-instance 'gtk:gtk-box :orientation :vertical)))
          (gtk:gtk-box-pack-start color-box solid-color)
          (gtk:gtk-box-pack-start color-box void-color)
          (gtk:gtk-box-pack-end control-box color-box :expand nil))
        (gtk:gtk-container-add window main-box)
        (gtk:gtk-widget-show-all window)))))
