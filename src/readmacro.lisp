(in-package :cl-markup)

(defun markup-reader (stream char arg)
  (declare (ignore char arg))
  `(markup ,(read stream t nil t)))

(defun enable-markup-syntax ()
  (set-dispatch-macro-character #\# #\M #'markup-reader))
