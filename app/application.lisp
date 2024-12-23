(in-package :cube-viewer-application)

(defun print-usage-and-exit ()
  (format *error-output* "Usage: cube-viewer filename cube-side~%")
  (uiop:quit 1))

(defun parse-integer-or-exit (string)
  (handler-case
      (parse-integer string)
    (error ()
      (print-usage-and-exit))))

(defun main ()
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil)
  (let ((arguments (uiop:command-line-arguments)))
    (unless (= (length arguments) 2)
      (print-usage-and-exit))
    (let ((name (first arguments))
          (side (parse-integer-or-exit (second arguments))))
      (handler-case
          (progn
            (cube-viewer:run name side)
            (gtk:join-gtk-main))
        (cube-viewer:fatal-error (c)
          (princ c *error-output*)
          (terpri *error-output*)
          (uiop:quit 1))))))
