(in-package :cube-viewer)

(defstruct gl-state
  (program           -1   :type fixnum)
  (vao               -1   :type fixnum)
  (vertex-buffer     -1   :type fixnum)
  (normal-buffer     -1   :type fixnum)
  (texture           -1   :type fixnum)
  (transform-loc     -1   :type fixnum)
  (sampler-loc       -1   :type fixnum)
  (threshold-loc     -1   :type fixnum)
  (scale-loc         -1   :type fixnum)
  (light-loc         -1   :type fixnum)
  (solid-loc         -1   :type fixnum)
  (void-loc          -1   :type fixnum)
  (use-threshold-loc -1   :type fixnum)
  (solid-color        (dvec3 0.9176d0 0.7137d0 0.4627d0)
                          :type dvec3)
  (void-color         (dvec3 0.1d0 0.1d0 0.1d0)
                          :type dvec3)
  (use-threshold      t   :type boolean)
  (threshold          0d0 :type double-float)
  (scale              1d0 :type double-float))

(defstruct camera
  (position (rtg-math.vector3:make 0.0 0.0 0.0)
            :type rtg-math.types:vec3)
  (fov      75.0 :type single-float)
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
  (gl:uniformi (gl-state-use-threshold-loc gl-state)
               (if (gl-state-use-threshold gl-state) 1 0))
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
     (gl:get-uniform-location program "COLOR_VOID")
     (gl-state-use-threshold-loc gl-state)
     (gl:get-uniform-location program "USE_THRESHOLD")))
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
