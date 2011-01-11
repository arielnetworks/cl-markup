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

(defun substitute-string-by (fn string)
  (declare (optimize speed))
  (let ((first-pos (position-if fn string)))
    (if first-pos
        (with-output-to-string (s)
            (loop with len = (length string)
                for old-pos = 0 then (1+ pos)
                for pos = first-pos
                  then (position-if fn string :start old-pos)
                for char =  (and pos (aref string pos))
                while pos
                do (write-sequence string s :start old-pos :end pos)
                   (write-string (funcall fn char) s)
                while (< (1+ pos) len)
                finally (unless pos
                          (write-sequence string s :start old-pos)))
            s)
        string)))

(defun escape-string (string)
  (if *auto-escape*
      (substitute-string-by
       (lambda (char)
         (case char
           (#\& "&amp;")
           (#\< "&lt;")
           (#\> "&gt;")
           (#\' "&#039;")
           (#\" "&quot;")
           (t (string char))))
       string)
      string))

(defun ensure-string (val)
  (if (stringp val)
      val
      (format nil "~A" val)))

(defun should-escape-p (val)
  (not (and (stringp val)
            (string= val (escape-string val)))))

(defmacro %escape-string-form (val)
  (let ((val2 (gensym)))
    `(let ((,val2 ,val))
       (if (should-escape-p ,val2)
           `(escape-string ,,val2)
           ,val2))))

(defun %dirty-string-form (form)
  (cond
    ((consp form) (let ((res (gensym)))
                    `(let ((,res ,form))
                       (if (consp ,res)
                           (apply #'write-strings ,res)
                           ,res))))
    ((null form) "")
    ((stringp form) (%escape-string-form form))
    ((symbolp form) `(escape-string (ensure-string ,form)))
    (t (%escape-string-form (format nil "~A" form)))))

(defmacro raw (&rest forms)
  `(let (*auto-escape*) ,@forms))

(defmacro esc (&rest forms)
  `(let ((*auto-escape* t))
     ,@(loop for form in forms
             collect (%escape-string-form form))))

(defun write-strings (&rest strings)
  (if *output-stream*
      (loop for str in strings
            do (write-string str *output-stream*))
      (with-output-to-string (s)
        (loop for str in strings
              do (write-string str s)))))

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
         (if (eq *markup-language* :html)
             (list ">")
             (list " />"))))))

(defmacro render-tag (tag)
  `(%write-strings
    ,@(tag->string tag)))

(defun doctype (lang)
  (case lang
    (:xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
    (:html "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
    (:xhtml "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
    (t "")))

(defmacro with-doctype (lang &body body)
  `(%write-strings
    ,(doctype lang)
    ,@(let ((*markup-language* lang))
        (loop for b in body
              append (eval b)))))

(defmacro markup (&rest tags)
  `(%write-strings
    ,@(loop for tag in tags
            append (tag->string tag))))

(defun markup* (&rest tags)
  (eval `(markup ,@tags)))

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
