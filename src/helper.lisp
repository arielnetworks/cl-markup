(in-package :cl-markup)

(defmacro raw (&rest forms)
  `(list ,@forms))

(defmacro esc (&rest forms)
  `(loop for form in forms
         collect (%escape-string-form form)))
