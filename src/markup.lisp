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

(defmacro attr (attr-plist)
  (and (consp attr-plist)
       `(format nil
                "~{~(~A~)=\"~A\"~^ ~}"
                (list ,@(loop for (key val) on attr-plist by #'cddr
                              append (list key `(escape-string ,val)))))))

(defun tagp (form)
  (and (consp form)
       (keywordp (car form))))

(defmacro tag (name attr-plist &rest body)
  (let ((res (gensym)))
    (if (= 0 (length body))
        `(format nil "<~(~A~) />" ,name)
        `(format nil "<~(~A~)~@[ ~A~]>~{~@[~A~]~}</~(~A~)>"
                 ,name (attr ,attr-plist)
                 (list ,@(loop for b in body
                               collect (cond
                                         ((tagp b) `(html ,b))
                                         ((consp b) `(let ((,res ,b))
                                                       (if (listp ,res) (apply #'concatenate 'string ,res)
                                                           ,res)))
                                         (t `(let ((,res ,b))
                                               (and ,res
                                                    (escape-string (format nil "~A" ,res))))))))
                 ,name))))

(defmacro html (form)
  (let ((tagname (pop form))
        (attr-plist (loop while (and form (keywordp (car form)))
                          collect (pop form)
                          collect (pop form))))
    `(tag ,tagname ,attr-plist ,@form)))
