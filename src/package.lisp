(in-package :cl-user)

(defpackage cl-markup
  (:nicknames :markup)
  (:use :cl)
  (:export :markup :html :html5 :xhtml :xml
           :markup*
           :doctype
           :escape-string :raw :esc
           :enable-markup-syntax
           :*output-stream*
	   :*auto-escape*
           :*markup-language*))
