(in-package :cl-user)

(defpackage cl-markup
  (:use :cl :cl-ppcre)
  (:export :markup :tag :escape-string :raw :esc
           :*auto-escape*
           :*output-stream*))
