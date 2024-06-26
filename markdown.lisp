(in-package :simple-doc)

(defun generate-markdown-doc (destination package &key
                                                    (output-undocumented *output-undocumented*)
                                                    (use-readme *use-readme*)
                                                    (kind-of-symbols :external)
                                                    (include '(:package :package-documentation))
                                                    (categories *categories*))
  "Generates Markdown doc for a package

   Args: - DESTINATION: (OR PATHNAME STRING STREAM NIL T) The documentation is written a stream created from the type of DESTINATION. See WITH-DESTINATION-STREAM.
         - PACKAGE: (PACKAGE-DESIGNATOR) The package for which to generate the documentation
         - OUTPUT-UNDOCUMENTED: (BOOLEAN) If T, enums undocumented things in generated doc.
         - KIND-OF-SYMBOLS: Kind of symbols to appear in the doc. One of :EXTERNAL, :PRESENT or :ACCESSIBLE.
         - CATEGORIES: The list of definition categories that should appear in the docs. Default is *CATEGORIES*, that includes all definition categories: (:FUNCTION :MACRO :GENERIC-FUNCTION :SLOT-ACCESSOR :VARIABLE :CLASS :CONDITION
 :CONSTANT)
         - INCLUDE: Controls what gets included in the output. Default is '(:PACKAGE :PACKAGE-DOCUMENTATION). If :PACKAGE appears in the INCLUDE list, the document will start with the PACKAGE name. If :PACKAGE-DOCUMENTATION appears in the INCLUDE list, then the PACKAGE docstring is added to the document."
  (let ((*package* (find-package package))
        (package (find-package package)))
    (with-output-to-destination (stream destination
                                        :if-does-not-exist :create
                                        :if-exists :supersede)
      (let ((*print-pretty* t)
            (*print-case* :downcase))
        (when (member :package include)
          (format stream "# ~A~%~%" (package-name package))
          (if use-readme
              (format stream "```~%~A~%```~%~%"
                      (readme-text (alexandria:make-keyword (package-name package))))
              (when (and (member :package-documentation include)
                         (documentation package t))
                (format stream "~A~%~%"
                        (documentation package t)))))
        (loop for category in categories
              do
                 (let ((names (names package category kind-of-symbols)))
                   (when names
                     (format stream "## ~A~%"
                             (string-capitalize (pluralization category)))
                     (loop for name in names
                           do
                              (render-category-element-md category name stream
                                                          :output-undocumented output-undocumented)))))))))

(defun md-escape (string)
  (setf string (ppcre:regex-replace-all "\\*" string "\\*"))
  (setf string (ppcre:regex-replace-all "#" string "\\#"))
  string)

(defmethod render-category-element-md :around (category thing stream &key (output-undocumented *output-undocumented*))
  (when (or output-undocumented
            (docs-for thing category))
    (call-next-method)))

(defmethod render-category-element-md ((category (eql :function)) function stream &key)
  (let ((lambda-list (sb-introspect:function-lambda-list function)))
    (format stream "### ~A~%~%"
            (md-escape (princ-to-string function)))
    (format stream "```lisp~%~A~%```~%~%"
            (if lambda-list (prin1-to-string lambda-list)
                "()"))
    (render-function-md function stream)
    (terpri stream)))

(defmethod render-category-element-md ((category (eql :generic-function)) function stream &key)
  (let ((lambda-list (sb-introspect:function-lambda-list function)))
    (format stream "### ~A~%~%"
            (md-escape (princ-to-string function)))
    (format stream "```lisp~%~A~%```~%~%"
            (if lambda-list (prin1-to-string lambda-list)
                "()"))
    (render-function-md function stream)
    (terpri stream)))

(defmethod render-category-element-md ((category (eql :macro)) macro stream &key)
  (let ((lambda-list (sb-introspect:function-lambda-list macro)))
    (format stream "### ~A~%~%"
            (md-escape (princ-to-string macro)))
    (format stream "```lisp~%~A~%```~%~%"
            (if lambda-list (prin1-to-string lambda-list)
                "()"))
    (render-function-md macro stream)
    (terpri stream)
    (terpri stream)))

(defmethod render-category-element-md (category thing stream &key)
  (format stream "### ~A~%" (md-escape (princ-to-string thing)))
  (when (docs-for thing category)
    (format stream "~A~%~%" (md-escape (docs-for thing category)))))

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
          do (render-docstring-metadata-md metadata stream)
             (terpri stream))))

(defmethod render-docstring-markup-md ((markup string) stream)
  (write-string markup stream))

(defmethod render-docstring-markup-md ((markup cons) stream)
  (loop for elem in markup
        do
           (render-docstring-markup-md elem stream)))

(defmethod render-docstring-markup-md ((markup code-element) stream)
  (format stream "```lisp~%~A~%```~%" (code-element-text markup)))

(defmethod render-docstring-markup-md ((markup list-element) stream)
  (loop for item in (list-element-items markup)
        do (render-docstring-markup-md item stream)
           (terpri stream)))

(defmethod render-docstring-markup-md ((markup list-item-element) stream)
  (format stream "* ~A" (list-item-element-text markup)))

(defmethod render-docstring-markup-md ((markup bold-element) stream)
  (format stream "**~A**" (bold-element-text markup)))

(defmethod render-docstring-markup-md ((markup italic-element) stream)
  (format stream "*~A*" (italic-element-text markup)))

(defmethod render-docstring-markup-md ((markup ref-element) stream)
  (write-string (ref-element-name markup) stream))

(defmethod render-docstring-markup-md ((markup email-element) stream)
  (write-string (email-element-email markup) stream))

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
