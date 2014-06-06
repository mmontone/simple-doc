(defpackage simple-doc
  (:use #:cl #:docstring-parser #:alexandria #:cl-who)
  (:export #:generate-html-doc
	   #:generate-markdown-doc))
