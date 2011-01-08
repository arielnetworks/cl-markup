(in-package :cl-markup-test)

(plan 15)

(deftest escape
    (is (escape-string "<script type=\"text/javascript\">alert();</script>")
        "&lt;script type=&quot;text/javascript&quot;&gt;alert();&lt;/script&gt;"))

(deftest html-expansion
    (setf cl-test-more:*default-test-function* #'equal)
  (is-expand (cl-markup::attr (:id "title"))
             (cl-markup::%write-strings "id=\"" (escape-string "title") "\""))
  (is-expand (html (:p "hoge"))
             (cl-markup::tag :p nil "hoge"))
  (is-expand (html (:p :id "title" "hoge"))
             (cl-markup::tag :p (:id "title") "hoge"))
  (is-expand (cl-markup::tag :p (:id "title") (:div "hoge"))
             (cl-markup::%write-strings "<p " (cl-markup::attr (:id "title")) ">" (html (:div "hoge")) "</p>"))
  (is-expand (cl-markup::tag :p nil nil)
             (cl-markup::%write-strings "<p" "" ">" "" "</p>")
             "expands nil to empty string")
  (is-expand (cl-markup::tag :p nil "hoge")
             (cl-markup::%write-strings "<p" "" ">" (escape-string "hoge") "</p>")
             "expands strings as a string")
  )

(deftest html
    (setf cl-test-more:*default-test-function* #'string=)
  (is (html (:p "hoge")) "<p>hoge</p>" "normal 'p' tag.")
  (is (html (:ul (:li "one") (:li "two"))) "<ul><li>one</li><li>two</li></ul>" "multiple items")
  (is (html (:br)) "<br />" "'br' tag")
  (is (html (:p nil)) "<p></p>" "empty tag")
  (is (html (:p :id "title" :class "important" "Hello, World!")) "<p id=\"title\" class=\"important\">Hello, World!</p>" "with attributes.")
  (is (html (:p (:div :style "padding: 10px;" "fuga"))) "<p><div style=\"padding: 10px;\">fuga</div></p>" "nested tag.")
  (is (html (:div :name "<script type=\"text/javascript\">alert();</script>"
                  "<hoge>"))
      "<div name=\"&lt;script type=&quot;text/javascript&quot;&gt;alert();&lt;/script&gt;\">&lt;hoge&gt;</div>" "escape body and attribute values")
  (is (html (:ul (loop for v in '("a" "b" "c") collect (html (:li v)))))
      "<ul><li>a</li><li>b</li><li>c</li></ul>")
  )

(run-test-all)
