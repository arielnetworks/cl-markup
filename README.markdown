# CL-MARKUP - Modern markup generation library for Common Lisp

## About

* Fast (only if you compile it)
* Support multiple document types (markup, xml, html, xhtml)
* Output to stream directly

## Usage

    (html
     (:body
      (:p :id "title" "aiueo")))
    ;=> <?xml version=\"1.0\" encoding=\"UTF-8\"?>
    ;   <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">
    ;   <html><body><p id="title">aiueo</p></body></html>

## Markup language

<code>markup</code> parse forms and write the result simply.

    (markup (:p "あいうえお"))
    ;=> "<p>あいうえお</p>"

Other macros <code>html<code>, <code>xhtml</code> and <code>xml</code>, output DOCTYPE before <code>markup</code>.

    (html (:p "あいうえお"))
    ;=> "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\"><html><p>あいうえお</p></html>"

## Escape

    (markup (:p "Tiffany & Co."))
    ;=> "<p>Tiffany &amp; Co.</p>"

    (markup (:p (raw "Tiffany & Co.")))
    ;=> "<p>Tiffany & Co.</p>"

## Output to stream directly

Markup macros output html tags into \*output-stream\*. The default is <code>NIL</code> and returns as a string. If it is <code>T</code>, output it to \*standard-output*.

    (let (*output-stream*)
      (markup (:p "hoge"))
    ;=> "<p>hoge</p>"
    
    (let ((*output-stream* t))
      (markup (:p "hoge")))
    ;;=> <p>hoge</p>
    ;=> T

## Markup syntax

You can embeded Lisp code in a tag body.

    (markup (:ul (loop for item in '(1 2 3) collect (markup (:li item)))))

But, <code>markup</code> is too long to embed. cl-markup provides an usefull syntax to write more shortly.

    (enable-markup-syntax)
    #M(:ul (loop for item in '(1 2 3) collect #M(:li item))))

## License

Copyright (c) 2011 Eitarow Fukamachi.  
Licensed under the LLGPL License.
