(in-package :cl-user)

(defpackage cl-markup
  (:use :cl)
  (:export :markup :html :xhtml :xml
           :markup*
           :doctype
           :escape-string :raw :esc
           :i18n
           :enable-markup-syntax
           :*auto-escape*
           :*output-stream*
           :*markup-language*
           :*i18n-dictionary*
           :*locale*))
