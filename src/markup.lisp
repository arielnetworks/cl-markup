(in-package :cl-markup)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun map-group-if (pred list fn)
    (loop
      while list
      for cur = (pop list)
      for cur-res = (funcall pred cur)
      append
      (let ((res (nreverse
                  (loop with acc = (list cur)
                        while list
                        for x = (pop list)
                        if (eq cur-res (funcall pred x))
                          do (push x acc)
                        else do (progn (push x list)
                                       (return acc))
                        finally (return acc)))))
        (if cur-res (list (apply fn res)) res)))))

(defun escape-string (string)
  (if *auto-escape*
      (regex-replace-all (create-scanner "[&<>'\"]") string
                         #'(lambda (match)
                             (case (aref match 0)
                               (#\& "&amp;")
                               (#\< "&lt;")
                               (#\> "&gt;")
                               (#\' "&#039;")
                               (#\" "&quot;")
                               (t match)))
                         :simple-calls t)))

(defmacro raw (&body body)
  `(let (*auto-escape*) ,@body))

(defmacro esc (&body body)
  `(let ((*auto-escape* t)) ,@body))

(defmacro %write-strings (&rest strings)
  (let ((s (gensym)))
    (flet ((conv (strs) (map-group-if #'stringp strs
                                      (lambda (&rest args)
                                        (apply #'concatenate 'string args)))))
      `(if *output-stream*
           (progn
             ,@(loop for str in (conv strings)
                     collect `(write-string ,str *output-stream*)))
           (with-output-to-string (,s)
             ,@(loop for str in (conv strings)
                     collect `(write-string ,str ,s)))))))

(defun tagp (form)
  (and (consp form)
       (keywordp (car form))))

(defun parse-tag (tag)
  (values
   (pop tag)
   (loop while (and tag (keywordp (car tag)))
         collect (pop tag)
         collect (pop tag))
   tag))

(defun attributes->string (attr-plist)
  (and (consp attr-plist)
       (butlast
        (loop for (key val) on attr-plist by #'cddr
              append `(,(concatenate 'string
                                     (string-downcase key)
                                     "=\"")
                       (escape-string ,val)
                       "\""
                       " ")))))

(defun tag->string (tag)
  (multiple-value-bind (name attr-plist body)
      (parse-tag tag)
    (nconc
     (list (format nil "<~(~A~)" name))
     (let ((attr-str (attributes->string attr-plist)))
       (if attr-str (cons " " attr-str)))
     (if body
         (nconc (list ">")
                (loop for elem in body
                      if (tagp elem)
                       append (tag->string elem)
                     else
                       collect (cond
                                 ((consp elem) (let ((res (gensym)))
                                                 `(let ((,res ,elem))
                                                    (if (listp ,res) (format nil "~{~A~}" ,res)
                                                        ,res))))
                                 ((null elem) "")
                                 ((stringp elem) `(escape-string ,elem))
                                 (t `(escape-string (format nil "~A" ,elem)))))
                (list (format nil "</~(~A~)>" name)))
         (list " />")))))

(defmacro render-tag (tag)
  `(%write-strings
    ,@(tag->string tag)))

(defmacro markup (&rest tags)
  `(if *output-stream*
       (progn
         ,@(loop for tag in tags
                 collect `(render-tag ,tag))
         *output-stream*)
       (concatenate 'string
                    ,@(loop for tag in tags
                            collect `(render-tag ,tag)))))

(defun doctype ()
  (%write-strings
   (case *markup-language*
     (:xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
     (:html "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
     (t "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"))))

(defmacro with-doctype (lang &body body)
  `(let ((*markup-language* ,lang))
     (if *output-stream*
         (progn (doctype) ,@body)
         (concatenate 'string
                      (doctype) ,@body))))

(defmacro html (&rest tags)
  `(with-doctype :html
     (markup (:html ,@tags))))

(defmacro xhtml (&rest tags)
  `(with-doctype :xhtml
     (markup (:html ,@tags))))

(defmacro xml (&rest tags)
  `(with-doctype :xml
     (markup ,@tags)))
