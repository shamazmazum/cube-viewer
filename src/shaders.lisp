(in-package :cube-viewer)

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
