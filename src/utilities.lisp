(in-package :cube-viewer)

(define-condition fatal-error (simple-error)
  ()
  (:documentation "Unrecoverable error"))

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
