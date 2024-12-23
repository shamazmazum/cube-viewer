(defsystem :cube-viewer
    :name :cube-viewer
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Porous media visualization application"
    :license "2-clause BSD"
    :serial t
    :pathname "src/"
    :components ((:file "package")
                 (:file "cube")
                 (:file "shaders")
                 (:file "utilities")
                 (:file "draw")
                 (:file "cube-viewer-window")
                 (:file "save-image")
                 (:file "cube-viewer"))
    :depends-on (:alexandria
                 :serapeum
                 :cl-cffi-gtk
                 :varjo
                 :cl-opengl
                 :rtg-math
                 :array-operations
                 :opticl))

(defsystem :cube-viewer/application
    :name :cube-viewer/application
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Porous media visualization application"
    :license "2-clause BSD"
    :serial t
    :pathname "app/"
    :components ((:file "package")
                 (:file "application"))
    :depends-on (:cube-viewer)
    :build-operation program-op
    :build-pathname "cube-viewer"
    :entry-point "cube-viewer-application:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression -1))
