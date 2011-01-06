(in-package :cl-markup)

(defun escape-string (string)
  (regex-replace-all (create-scanner "[&<>'\"]") string
                     #'(lambda (match)
                         (case (aref match 0)
                           (#\& "&amp;")
                           (#\< "&lt;")
                           (#\> "&gt;")
                           (#\' "&#039;")
                           (#\" "&quot;")
                           (t match)))
                     :simple-calls t))

(defun attr (attr-plist)
  (and (consp attr-plist)
       (format nil
               "~{~A~^ ~}"
               (loop for (key val) on attr-plist by #'cddr
                     collect (format nil "~(~A~)=\"~A\"" key (escape-string val))))))

(defun tagp (form)
  (and (consp form)
       (keywordp (car form))))

(defmacro tag (name &optional attr-plist (body nil body-supplied-p))
  (let ((res (gensym)))
    (if body-supplied-p
        `(format nil "<~(~A~)~@[ ~A~]>~@[~A~]</~(~A~)>"
                 ,name (attr ',attr-plist)
                 ,(cond
                    ((tagp body) `(html ,body))
                    ((consp body) `(let ((,res ,body))
                                     (if (listp ,res) (apply #'concatenate 'string ,res)
                                         ,res)))
                    (t `(let ((,res ,body))
                          (and ,res
                               (escape-string (format nil "~A" ,res))))))
                 ,name)
        `(format nil "<~(~A~) />" ,name))))

(defmacro html (form)
  (let ((tagname (pop form))
        (attr-plist (apply #'append
                           (loop while form
                                 with res
                                 do (cond
                                      ((keywordp (car form))
                                       (push (list (pop form) (pop form)) res))
                                      ((and (consp (car form)) (symbolp (caar form)) (string= "@" (symbol-name (caar form))))
                                       (push (cdr (pop form)) res))
                                      (t (return res)))
                                 finally (return res)))))
    `(tag ,tagname ,attr-plist ,@form)))
