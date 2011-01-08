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

(defmacro %write-strings (&rest strings)
  (let ((s (gensym)))
    `(if *output-stream*
         (progn
           ,@(loop for str in strings
                   collect `(write-string ,str *output-stream*)))
         (with-output-to-string (,s)
           ,@(loop for str in strings
                   collect `(write-string ,str ,s))))))

(defmacro attr (attr-plist)
  (and (consp attr-plist)
       `(%write-strings
         ,@(butlast
            (loop for (key val) on attr-plist by #'cddr
                  append `(,(concatenate 'string
                                          (string-downcase key)
                                         "=\"")
                           (escape-string ,val)
                           "\""
                           " "))))))

(defun tagp (form)
  (and (consp form)
       (keywordp (car form))))

(defmacro tag (name attr-plist &rest body)
  (let ((res (gensym)))
    (if (= 0 (length body))
        `(%write-strings ,(format nil "<~(~A~) />" name))
        `(%write-strings
          ,(format nil "<~(~A~)~@[ ~]" name attr-plist)
          ,(if attr-plist `(attr ,attr-plist) "")
          ">"
          ,@(loop for b in body
                  collect (cond
                            ((tagp b) `(html ,b))
                            ((consp b) `(let ((,res ,b))
                                          (if (listp ,res) (apply #'concatenate 'string ,res)
                                              ,res)))
                            ((null b) "")
                            ((stringp b) `(escape-string ,b))
                            (t `(let ((,res ,b))
                                  (if ,res
                                      (escape-string (format nil "~A" ,res))
                                      "")))))
          ,(format nil "</~(~A~)>" name)))))

(defmacro html (form)
  (let ((tagname (pop form))
        (attr-plist (loop while (and form (keywordp (car form)))
                          collect (pop form)
                          collect (pop form))))
    `(tag ,tagname ,attr-plist ,@form)))
