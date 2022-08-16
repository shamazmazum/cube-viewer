(defsystem :cube-viewer
    :name :cube-viewer
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Porous media visualization application"
    :license "2-clause BSD"
    :serial t
    :pathname "src/"
    :components ((:file "package")
                 (:file "cube-viewer"))
    :depends-on (:alexandria
                 :serapeum
                 :cl-cffi-gtk
                 :varjo
                 :cl-opengl
                 :rtg-math
                 :array-operations))
