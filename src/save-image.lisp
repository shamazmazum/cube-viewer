(in-package :cube-viewer)

(defun show-error-message (window condition)
  (let ((dialog (gtk:gtk-message-dialog-new
                 window '(:destroy-with-parent)
                 :error :close
                 (with-output-to-string (output)
                   (princ condition output)))))
    (gtk:gtk-dialog-run dialog)
    (gtk:gtk-widget-destroy dialog)))

(sera:-> save-image
         (gtk:gtk-gl-area string)
         (values &optional))
(defun save-image (area path)
  (let ((type (pathname-type (pathname path))))
    (unless (or (string= type "png")
                (string= type "jpg"))
      (error "Unsupported image type: ~a" type)))
  (gtk:gtk-gl-area-make-current area)
  (let* ((allocation (gtk:gtk-widget-get-allocation area))
         (width  (gdk:gdk-rectangle-width  allocation))
         (height (gdk:gdk-rectangle-height allocation))
         (image (make-array (list height width 3)
                            :element-type '(unsigned-byte 8))))
    ;; We need to recopy pixels because Opticl works only with
    ;; SIMPLE-ARRAYs
    (map-into (aops:flatten image) #'identity
              ;; GL:READ-PIXELS is too damn slow but can be called only
              ;; in GTK thread which can cause a small UI hang. Can this
              ;; be fixed somehow?
              (gl:read-pixels 0 0 width height :rgb :unsigned-byte))
    (opticl:write-image-file
     path (opticl:vertical-flip-image image)))
  (values))

(sera:-> image-save-dialog
         (cube-viewer-window
          (sera:-> (string) (values &optional)))
         (values &optional))
(defun image-save-dialog (window callback)
  (let ((dialog (gtk:gtk-file-chooser-dialog-new
                 "Save image" window
                 :save
                 "gtk-cancel" :cancel
                 "gtk-save"   :accept)))
    (gtk:gtk-file-chooser-set-do-overwrite-confirmation dialog t)
    (gtk:gtk-file-chooser-set-current-name
     dialog (concatenate 'string (cube-name window) ".png"))
    (gobject:g-signal-connect
     dialog "response"
     (lambda (widget response-id)
       (declare (ignore widget))
       (when (eql response-id (cffi:foreign-enum-value 'gtk:gtk-response-type :accept))
         (handler-case
             (funcall callback (gtk:gtk-file-chooser-get-filename dialog))
           (error (c) (show-error-message window c))))
       (gtk:gtk-widget-destroy dialog)))
    (gtk:gtk-widget-show dialog))
  (values))
