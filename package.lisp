(defpackage simple-doc
  (:use #:cl #:docstring-parser #:alexandria #:cl-who)
  (:export
   #:generate-html-doc
   #:generate-html-bootstrap-doc
   #:generate-markdown-doc)
  (:documentation "Simple documentation generator for Common Lisp"))
