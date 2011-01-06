(in-package :cl-user)

(defpackage cl-markup-asd
  (:use :cl :asdf))

(in-package :cl-markup-asd)

(defsystem cl-markup
  :version "0.1"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:cl-ppcre)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "markup")))))
