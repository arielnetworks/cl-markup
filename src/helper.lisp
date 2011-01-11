(in-package :cl-markup)

(defmacro raw (&rest forms)
  `(let (*auto-escape*) ,@forms))

(defmacro esc (&rest forms)
  `(let ((*auto-escape* t))
     ,@(loop for form in forms
             collect (%escape-string-form form))))
