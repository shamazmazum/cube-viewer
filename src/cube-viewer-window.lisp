(in-package :cube-viewer)

(defclass cube-viewer-window (gtk:gtk-window)
  ((cube-name :initarg  :cube-name
              :initform (error "Specify CUBE-NAME")
              :reader   cube-name
              :type     string))
  (:metaclass gobject:gobject-class))

(defmethod initialize-instance :after ((window cube-viewer-window) &rest initargs)
  (declare (ignore initargs))
  (setf (gtk:gtk-window-title window)
        (format nil "cube-viewer: ~a"
                (cube-name window))))
