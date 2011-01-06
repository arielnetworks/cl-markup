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

(defun %tag (name attr-plist &optional (body nil body-supplied-p))
  (if body-supplied-p
      (format nil "<~(~A~)~@[ ~A~]>~@[~A~]</~(~A~)>"
              name (attr attr-plist) body name)
      (format nil "<~(~A~) />" name)))

(defmacro tag (name &optional attr-plist (body nil body-supplied-p))
  (if body-supplied-p
      `(%tag ,name ',attr-plist
             ,(if (tagp body)
                  `(html ,body)
                  `(escape-string ,body)))
      `(%tag ,name ',attr-plist)))

(defmacro html (form)
  (if (= 2 (length form))
      `(tag ,(car form) nil ,@(cdr form))
      `(tag ,@form)))
