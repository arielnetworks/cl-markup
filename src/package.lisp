(in-package :cl-user)

(defpackage cl-markup
  (:use :cl :cl-ppcre)
  (:export :markup :html :xhtml :xml :tag
           :escape-string :raw :esc
           :*auto-escape*
           :*output-stream*
           :*markup-language*))
