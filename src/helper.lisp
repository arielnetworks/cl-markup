(in-package :cl-markup)

(defmacro raw (&rest forms)
  `(let (*auto-escape*) ,@forms))

(defmacro esc (&rest forms)
  `(let ((*auto-escape* t))
     ,@(loop for form in forms
             collect (%escape-string-form form))))

(defmacro i18n (val)
  (let ((dict (gensym)))
    `(let ((,dict (gethash *locale* *i18n-dictionary*)))
       (if ,dict
           (gethash ,val ,dict)
           ,val))))
