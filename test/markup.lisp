(in-package :cl-markup-test)

(plan 5)

(deftest escape
    (is (escape-string "<script type=\"text/javascript\">alert();</script>")
        "&lt;script type=&quot;text/javascript&quot;&gt;alert();&lt;/script&gt;"))

(deftest markup
    (setf cl-test-more:*default-test-function* #'string=)
  (is (markup (:p "hoge")) "<p>hoge</p>" "normal 'p' tag.")
  (is (markup (:ul (:li "one") (:li "two"))) "<ul><li>one</li><li>two</li></ul>" "multiple items")
  (is (markup (:br)) "<br />" "'br' tag")
  (is (markup (:br :style "clear: both;")) "<br style=\"clear: both;\" />" "'br' tag with an attribute")
  (is (markup (:p nil)) "<p></p>" "empty tag")
  (is (markup (:p :id "title" :class "important" "Hello, World!")) "<p id=\"title\" class=\"important\">Hello, World!</p>" "with attributes.")
  (is (markup (:p (:div :style "padding: 10px;" "fuga"))) "<p><div style=\"padding: 10px;\">fuga</div></p>" "nested tag.")
  (is (markup (:div :name "<script type=\"text/javascript\">alert();</script>"
                  "<hoge>"))
      "<div name=\"&lt;script type=&quot;text/javascript&quot;&gt;alert();&lt;/script&gt;\">&lt;hoge&gt;</div>" "escape body and attribute values")
  (is (markup (:ul (loop for v in '("a" "b" "c") collect (markup (:li v)))))
      "<ul><li>a</li><li>b</li><li>c</li></ul>"
      "embed expressions")
  (is (markup (:ul nil))
      "<ul></ul>"
      "nil body")
  (is (markup (:div (let ((s "<a>Foo</a>")) s))) "<div>&lt;a&gt;Foo&lt;/a&gt;</div>"
      "auto escape")
  (let ((*auto-escape* t))
    (is (markup (:p "Tiffany & Co."))
        "<p>Tiffany &amp; Co.</p>"
        "*auto-escape* is T"))
  (let ((*auto-escape* nil))
    (is (markup (:p "Tiffany & Co."))
        "<p>Tiffany & Co.</p>"
        "*auto-escape* is NIL"))
  )

(deftest html
  (is (html (:br)) "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\"><html><br></html>")
  (is (html (:ul (loop for v in '("a" "b" "c") collect (markup* `(:li ,v (:br))))))
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\"><html><ul><li>a<br></li><li>b<br></li><li>c<br></li></ul></html>"))

(deftest html5
  (is (html5 (:br)) "<!DOCTYPE html><html><br></html>")
  (is (html5 (:ul (loop for v in '("a" "b" "c") collect (markup* `(:li ,v (:br))))))
      "<!DOCTYPE html><html><ul><li>a<br></li><li>b<br></li><li>c<br></li></ul></html>"))

(deftest raw-and-esc
  (is (markup (:p "<hoge>"))
      "<p>&lt;hoge&gt;</p>"
      "normal case")
  ;; FIXME: double escaping.
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
  (is (markup (:p (raw (let (x) x))))
      "<p></p>"
      "raw with nil")
)

(run-test-all)
