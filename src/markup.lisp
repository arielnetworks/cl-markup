(in-package :cl-markup)

(defmacro %escape-string-form (val)
  (let ((val2 (gensym)))
    `(let ((,val2 ,val))
       (if (should-escape-p ,val2)
           `(escape-string ,,val2)
           ,val2))))

(defun %dirty-string-form (form)
  (cond
    ((consp form) (let ((res (gensym))
                        (s (gensym))
                        (r (gensym)))
                    `(let ((,res ,form))
                       (if (listp ,res)
                           (with-output-to-string (,s)
                             (dolist (,r ,res)
                               (if ,r (write-string ,r ,s))))
                           (escape-string ,res)))))
    ((null form) "")
    ((stringp form) (%escape-string-form form))
    ((symbolp form) `(escape-string (ensure-string ,form)))
    (t (%escape-string-form (format nil "~A" form)))))

(defmacro %write-strings (&rest strings)
  (let ((s (gensym))
        (strings (map-group-if #'stringp strings
                               (lambda (&rest args)
                                 (apply #'concatenate 'string args)))))
    `(if *output-stream*
         (progn ,@(loop for str in strings
                        collect `(write-string ,str *output-stream*)))
         (with-output-to-string (,s)
           ,@(loop for str in strings
                   collect `(write-string ,str ,s))))))

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
                       ,(%dirty-string-form val)
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
                        collect (%dirty-string-form elem))
                (list (format nil "</~(~A~)>" name)))
         (if (or (eq *markup-language* :html)
                 (eq *markup-language* :html5))
             (list ">")
             (list " />"))))))

(defun doctype (lang)
  (case lang
    (:xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
    (:html "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
    (:html5 "<!DOCTYPE html>")
    (:xhtml "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
    (t "")))

(defmacro with-doctype (lang &body body)
  `(prog2
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setq *markup-language* ,lang))
     (%write-strings
      ,(doctype lang)
      ,@(let ((*markup-language* lang))
          (loop for b in body
                append (eval b))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setq *markup-language* :xhtml))))

(defmacro markup (&rest tags)
  `(%write-strings
    ,@(loop for tag in tags
            append (tag->string tag))))

(defun markup* (&rest tags)
  (eval `(markup ,@tags)))

(defmacro html5 (&rest tags)
  `(with-doctype :html5
     (tag->string (cons :html ',tags))))

(defmacro html (&rest tags)
  `(with-doctype :html
     (tag->string (cons :html ',tags))))

(defmacro xhtml (&rest tags)
  `(with-doctype :xhtml
     (tag->string (cons :html ',tags))))

(defmacro xml (&rest tags)
  `(with-doctype :xml
     (loop for tag in ',tags
           append (tag->string tag))))
