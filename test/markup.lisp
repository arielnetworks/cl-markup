(in-package :cl-markup-test)

(plan 21)

(deftest escape
    (is (escape-string "<script type=\"text/javascript\">alert();</script>")
        "&lt;script type=&quot;text/javascript&quot;&gt;alert();&lt;/script&gt;"))

(deftest html-expansion
    (setf cl-test-more:*default-test-function* #'equal)
  (is-expand (cl-markup::render-tag (:p :id "title" (:div "<hoge>")))
             (cl-markup::%write-strings "<p" " " "id=\"" "title" "\"" ">" "<div" ">" (escape-string "<hoge>") "</div>" "</p>"))
  (is-expand (cl-markup::render-tag (:p nil))
             (cl-markup::%write-strings "<p" ">" "" "</p>")
             "expands nil to empty string")
  (is-expand (cl-markup::render-tag (:p nil "hoge"))
             (cl-markup::%write-strings "<p" ">" "" "hoge" "</p>")
             "expands strings as a string")
  )

(deftest markup
    (setf cl-test-more:*default-test-function* #'string=)
  (is (markup (:p "hoge")) "<p>hoge</p>" "normal 'p' tag.")
  (is (markup (:ul (:li "one") (:li "two"))) "<ul><li>one</li><li>two</li></ul>" "multiple items")
  (is (markup (:br)) "<br />" "'br' tag")
  (is (markup (:br :style "clear: both;")) "<br style=\"clear: both;\" />" "'br' tag with an attribute")
  (is (html (:br)) "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\"><html><br></html>" "'br' tag")
  (is (markup (:p nil)) "<p></p>" "empty tag")
  (is (markup (:p :id "title" :class "important" "Hello, World!")) "<p id=\"title\" class=\"important\">Hello, World!</p>" "with attributes.")
  (is (markup (:p (:div :style "padding: 10px;" "fuga"))) "<p><div style=\"padding: 10px;\">fuga</div></p>" "nested tag.")
  (is (markup (:div :name "<script type=\"text/javascript\">alert();</script>"
                  "<hoge>"))
      "<div name=\"&lt;script type=&quot;text/javascript&quot;&gt;alert();&lt;/script&gt;\">&lt;hoge&gt;</div>" "escape body and attribute values")
  (is (markup (:ul (loop for v in '("a" "b" "c") collect (markup (:li v)))))
      "<ul><li>a</li><li>b</li><li>c</li></ul>"
      "embed expressions")
  (is (html (:body (:table (loop for x from 0 to 2 collect (markup (:tr (:td x)))))))
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\"><html><body><table><tr><td>0</td></tr><tr><td>1</td></tr><tr><td>2</td></tr></table></body></html>"
      "embed expressions")
  )

(deftest raw-and-esc
    (setf cl-markup::*auto-escape* t)
  (is (markup (:p "<hoge>"))
      "<p>&lt;hoge&gt;</p>"
      "normal case")
  (is (markup (:p (esc "<hoge>")))
      "<p>&lt;hoge&gt;</p>"
      "esc")
  (is (markup (:p (esc (concatenate 'string "<" "hage" ">"))))
      "<p>&lt;hage&gt;</p>"
      "esc with an expression")
  (is (markup (:p "<hoge>" (:div (raw "Tiffany & Co."))))
      "<p>&lt;hoge&gt;<div>Tiffany & Co.</div></p>"
      "raw")
  (is (markup (:p "<hoge>" (:div (raw (concatenate 'string "Tiffany" " & " "Co.")))))
      "<p>&lt;hoge&gt;<div>Tiffany & Co.</div></p>"
      "raw with an expression")
  (is (let (*auto-escape*) (markup (:p "<hoge>")))
      "<p><hoge></p>"
      "*auto-escape* is nil")
)

(run-test-all)
