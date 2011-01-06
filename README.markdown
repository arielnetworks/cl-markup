# CL-MARKUP - Modern markup generation library for Common Lisp

## About

* document type (markup, xml, html, xhtml)
* output stream
* enable-markup-syntax

## Usage

    (html
     (:body
      (:p (:id "title") "aiueo")))
    ;;=> "&lt;html&gt;&lt;body&gt;&lt;p id=\"title\"&gt;aiueo&lt;/p&gt;&lt;/body&gt;&lt;/html&gt;"

## License

Copyright (c) 2011 Eitarow Fukamachi.  
Licensed under the LLGPL License.
