(in-package :cl-markup-test)

(plan 9)

(deftest html-expansion
    (setf cl-test-more:*default-test-function* #'equal)
  (is-expand (html (:p "hoge"))
             (cl-markup::tag :p nil "hoge"))
  (is-expand (html (:p (:id "title") "hoge"))
             (cl-markup::tag :p (:id "title") "hoge"))
  (is-expand (html (:p (:id "title") (:div "hoge")))
             (cl-markup::tag :p (:id "title") (:div "hoge")))
  (is-expand (cl-markup::tag :p (:id "title") (:div "hoge"))
             (cl-markup::%tag :p '(:id "title") (html (:div "hoge"))))
  )

(deftest html
    (setf cl-test-more:*default-test-function* #'string=)
  (is (html (:p "hoge")) "<p>hoge</p>" "normal 'p' tag.")
  (is (html (:br)) "<br />" "'br' tag")
  (is (html (:p nil)) "<p></p>" "empty tag")
  (is (html (:p (:id "title") "Hello, World!")) "<p id=\"title\">Hello, World!</p>" "with attributes.")
  (is (html (:p (:div (:style "padding: 10px;") "fuga"))) "<p><div style=\"padding: 10px;\">fuga</div></p>" "nested tag.")
  )

(run-test-all)
