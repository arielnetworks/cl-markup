(in-package :cl-user)

(defpackage :cl-markup-test-asd
  (:use :cl :asdf))

(in-package :cl-markup-test-asd)

(defsystem cl-markup-test
  :depends-on (:cl-test-more :cl-markup)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "markup")))))
