(in-package :cl-user)

(defpackage cl-markup
  (:use :cl)
  (:export :markup :html :xhtml :xml
           :markup*
           :doctype
           :escape-string :raw :esc
           :enable-markup-syntax
           :*auto-escape*
           :*output-stream*
           :*markup-language*))
