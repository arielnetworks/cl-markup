(in-package :cl-markup)

(defvar *auto-escape* t)

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
      (if cur-res (list (apply fn res)) res))))

(defun substitute-string-by (fn string)
  (declare (optimize (speed 3)))
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
