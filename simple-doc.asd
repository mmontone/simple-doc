;;;; simple-doc.asd

(asdf:defsystem #:simple-doc
  :serial t
  :description "Describe simple-doc here"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-who
               #:docstring-parser)
  :components ((:file "package")
	       (:file "util")
               (:file "simple-doc")))

