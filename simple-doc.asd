;;;; simple-doc.asd

(asdf:defsystem #:simple-doc
  :serial t
  :description "Simple documentation generator for Common Lisp"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-who
               #:docstring-parser
	       #:trivial-shell
	       #:closer-mop)
  :components ((:file "package")
	       (:file "util")
               (:file "simple-doc")
	       (:file "markdown")
	       (:file "html-bootstrap"))
  :serial t)
