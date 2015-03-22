(in-package :simple-doc)

(defun generate-markdown-doc (output-filename package &key (output-undocumented *output-undocumented*))
    "Generates Markdown doc for a package

   Args: - output-filename: A pathname or string. The documentation is written to that file.
         - package (package): The package for which to generate the documentation
         - output-undocumented (boolean): If T, enums undocumented things in generated doc."

  (with-open-file (stream output-filename
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
    (format stream "# ~A~%" (package-name package))
    (format stream "~%```~%~A~%```~%"
	    (readme-text (alexandria:make-keyword (package-name package))))
    (loop for category in *categories*
       do
	 (format stream "## ~A~%"
		 (pluralization (string-capitalize (symbol-name category))))
	 (loop for name in (names package category)
	    do
	      (render-category-element-md category name stream :output-undocumented output-undocumented)))))

(defun md-escape (string)
  (setf string (ppcre:regex-replace-all "\\*" string "\\*"))
  (ppcre:regex-replace-all "#" string "\\#"))

(defmethod render-category-element-md :around (category thing stream &key (output-undocumented *output-undocumented*))
  (when (or output-undocumented
	    (docs-for thing category))
    (let ((*print-pretty* nil)
	  (*print-case* :downcase))
      (call-next-method))))

(defmethod render-category-element-md ((category (eql :function)) function stream &key)
  (let ((lambda-list (sb-introspect:function-lambda-list function)))
    (format stream "### ~A ~A~%"
	    (md-escape (princ-to-string function))
	    (md-escape (princ-to-string lambda-list)))
    (render-function-md function stream)))

(defmethod render-category-element-md (category thing stream &key)
  (format stream "### ~A~%" (md-escape (princ-to-string thing)))
  (format stream "~A~%" (md-escape (docs-for thing category))))

(defun render-function-md (function stream)
  (when (docs-for function :function)
    (render-function-docstring-md
     (parse-function-docstring
      (docs-for function :function))
     stream)))
	   
(defun render-function-docstring-md (docstring stream)
  (render-docstring-markup-md
   (function-docstring-short-description docstring)
   stream)
  (terpri stream)
  (terpri stream)
  (when (function-docstring-args docstring)
    (format stream "Arguments:~%~%")
    (loop for arg in (args-element-args (function-docstring-args docstring))
       do
	 (format stream "- **~A**: " (arg-element-name arg))
	 (when (arg-element-type arg)
	   (format stream "(~A) " (arg-element-type arg)))
	 (render-docstring-markup-md (arg-element-description arg)
				     stream)
	 (terpri stream)))
  (terpri stream)
  (terpri stream)
  (when (function-docstring-returns docstring)
    (format stream "**Returns**: ")
    (render-docstring-markup-md
     (returns-element-returns
      (function-docstring-returns docstring))
     stream)
    (terpri stream)
    (terpri stream))
  (when (function-docstring-long-description docstring)
    (render-docstring-markup-md
     (function-docstring-long-description docstring)
     stream))
  (when (function-docstring-metadata docstring)
    (loop for metadata in (docstring-metadata-metadata
			   (function-docstring-metadata docstring))
       do (render-docstring-metadata-md metadata stream))))

(defmethod render-docstring-markup-md ((markup string) stream)
  (write-string markup stream))

(defmethod render-docstring-markup-md ((markup cons) stream)
  (loop for elem in markup
     do
       (render-docstring-markup-md elem stream)))

(defmethod render-docstring-markup-md ((markup code-element) stream)
  (format stream "```~%~A~%```" (code-element-text markup)))

(defmethod render-docstring-markup-md ((markup bold-element) stream)
  (format stream "**~A**" (bold-element-text markup)))

(defmethod render-docstring-markup-md ((markup italic-element) stream)
  (format stream "*~A*" (italic-element-text markup)))

(defmethod render-docstring-markup-md ((markup ref-element) stream)
  (write-string stream (ref-element-name markup)))

(defmethod render-docstring-markup-md ((markup email-element) stream)
  (write-string stream (email-element-email markup)))

(defmethod render-docstring-markup-md ((markup link-element) stream)
  (format stream "[~A](~A)"
	  (or (link-element-title markup)
	      (link-element-url markup))
	  (link-element-url markup)))

(defmethod render-docstring-metadata-md ((version docstring-version) stream)
  (format stream "**Version**: ~A" (docstring-version-version version)))

(defmethod render-docstring-metadata-md ((todo docstring-todo) stream)
  (write-string "**TODO**: " stream)
  (render-docstring-markup-md (docstring-todo-todo todo) stream))

(defmethod render-docstring-metadata-md ((see docstring-see) stream)
  (write-string "**See**: " stream)
  (loop for reference in (docstring-see-references see)
     do
       (render-docstring-markup-md reference stream)))

(defmethod render-docstring-metadata-md ((date docstring-date) stream)
  (write-string "**Date**: " stream)
  (render-docstring-markup-md (docstring-date-date date) stream))

(defmethod render-docstring-metadata-md ((author docstring-author) stream)
  (write-string "**Author**: " stream)
  (render-docstring-markup (docstring-author-author author) stream))

(defmethod render-docstring-metadata-md ((tags docstring-tags) stream)
  (format stream "**Tags**: ~{~A~^, ~}" (docstring-tags-tags tags)))

(defmethod render-docstring-metadata-md ((categories docstring-categories) stream)
  (format stream "**Categories**: ~{~A~^, ~}" (docstring-categories-categories categories)))
