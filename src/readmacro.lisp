(in-package :cl-markup)

(defun markup-reader (stream char arg)
  (declare (ignore char arg))
  `(markup ,(read stream t nil t)))

(defun %enable-markup-syntax ()
  (setf *readtable* (copy-readtable))
  (set-dispatch-macro-character #\# #\M #'markup-reader))

(defmacro enable-markup-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-markup-syntax)))
