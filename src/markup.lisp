(in-package :cl-markup)

(defun attr (attr-plist)
  ;; TODO: escape each strings in attributes.
  (and (consp attr-plist)
       (format nil "~{~(~A~)=\"~A\"~}" attr-plist)))

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
      `(%tag ,name ',attr-plist ,(if (tagp body) `(html ,body) body))
      `(%tag ,name ',attr-plist)))

(defmacro html (form)
  (if (= 2 (length form))
      `(tag ,(car form) nil ,@(cdr form))
      `(tag ,@form)))
