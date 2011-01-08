(in-package :cl-user)

(defpackage cl-markup
  (:use :cl :cl-ppcre)
  (:export :html :escape-string :raw :esc
           :*auto-escape*
           :*output-stream*))
