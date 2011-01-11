(in-package :cl-user)

(defpackage cl-markup-asd
  (:use :cl :asdf))

(in-package :cl-markup-asd)

(defsystem cl-markup
  :version "0.1"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "special")
                             (:file "markup")
                             (:file "helper")
                             (:file "readmacro")))))
