(in-package :simple-doc)

(defparameter *default-css*
  (asdf:system-relative-pathname :simple-doc "simple-doc.css"))

(defparameter *output-undocumented* nil "If T, undocumented things appear on documentation.")

(defparameter *use-readme* nil "If T, reads the readme file and appends it to docs")

(defun generate-html-doc (output-filename package 
			  &key (css *default-css*)
			    (output-undocumented *output-undocumented*)
			    (use-readme *use-readme*))
  "Generates HTML doc for a package

   Args: - output-filename: A pathname or string. The documentation is written to that file.
         - package (package): The package for which to generate the documentation
         - css: The css stylesheet.
         - output-undocumented: If T, undocumented things appear on documentation.
         - use-readme: If T, reads the readme file and appends it to docs"
  (with-open-file (stream output-filename
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
    (let ((css-string (read-file-to-string css))
	  (*print-pretty* nil)
	  (*print-case* :downcase))
      (with-html-output (html stream)
	(htm
	 (:html
	  (:head
	   (:title (fmt "Simple doc for ~A" package))
	   (:style (str css-string)))
	  (:body
	   (:h1 (str (package-name package)))
	   (if use-readme
	       (htm (:pre
		     (str (readme-text (alexandria:make-keyword (package-name package))))))
	       (htm (:p (str (documentation package t)))))
	   (loop for category in *categories*
	      do
		(htm
		 (:h2 (str (pluralization (string-capitalize (symbol-name category)))))
		 (loop for name in (names package category)
		    do
		      (render-category-element category name stream 
					       :output-undocumented output-undocumented)))))))))))

(defmethod render-category-element :around (category thing stream &key (output-undocumented *output-undocumented*))
  (when (or output-undocumented
	    (docs-for thing category))
    (call-next-method)))

(defmethod render-category-element ((category (eql :function)) function stream &key)
  (with-html-output (html stream)
    (let ((lambda-list (sb-introspect:function-lambda-list function)))
      (htm (:div :id (make-unique-name function category)
		 (:h3 (fmt "~A ~A" function lambda-list))
		 (render-function function stream))))))

(defmethod render-category-element (category thing stream &key)
  (with-html-output (html stream)
    (htm (:div :id (make-unique-name thing category)
	       (:h3 (str thing))
	       (:p (str (docs-for thing category)))))))

(defun render-function (function stream)
  (with-html-output (html stream)
    (htm
     (:div :class "function"
	   ;(:p :class "name" (str function))
	   (when (docs-for function :function)
	     (render-function-docstring
	      (parse-function-docstring
	       (docs-for function :function))
	      stream))))))
	   
(defun render-function-docstring (docstring stream)
  (with-html-output (html stream)
    (htm
     (:div :class "short-description"
	   (render-docstring-markup
	    (function-docstring-short-description docstring)
	    stream))
     (when (function-docstring-args docstring)
       (htm
	(:div :class "arguments"
	      (:p (:b (str "Arguments:")))
	      (:ul
	       (loop for arg in (args-element-args (function-docstring-args docstring))
		  do
		    (htm
		     (:li (:b (str (arg-element-name arg)))
			  (str ": ")
			  (when (arg-element-type arg)
			    (htm (:i (fmt "(~A) " (arg-element-type arg)))))
			  (render-docstring-markup (arg-element-description arg)
						   stream))))))))
     (when (function-docstring-returns docstring)
       (htm
	(:div :class "returns"
	      (:b (str "Returns: "))
	      (render-docstring-markup
	       (returns-element-returns
	       (function-docstring-returns docstring))
	       stream))))
     (when (function-docstring-long-description docstring)
       (htm
	(:div :class "long-description"
	      (render-docstring-markup
	       (function-docstring-long-description docstring)
	       stream))))
     (when (function-docstring-metadata docstring)
       (htm
	(:div :class "metadata"
	      (loop for metadata in (docstring-metadata-metadata
				     (function-docstring-metadata docstring))
		 do (render-docstring-metadata metadata stream))))))))

(defmethod render-docstring-markup ((markup string) stream)
  (with-html-output (html stream)
    (str markup)))

(defmethod render-docstring-markup ((markup cons) stream)
  (loop for elem in markup
       do
       (render-docstring-markup elem stream)))

(defmethod render-docstring-markup ((markup code-element) stream)
  (with-html-output (html stream)
    (:code (str (code-element-text markup)))))

(defmethod render-docstring-markup ((markup bold-element) stream)
  (with-html-output (html stream)
    (:b (str (bold-element-text markup)))))

(defmethod render-docstring-markup ((markup italic-element) stream)
  (with-html-output (html stream)
    (:it (str (italic-element-text markup)))))

(defmethod render-docstring-markup ((markup ref-element) stream)
  
  (with-html-output (html stream)
    (let ((href (format nil "#~A~@[-~A~]"
			(ref-element-name markup)
			(ref-element-type markup))))
    (htm
     (:a :class "reference"
	 :href href
	 (str (ref-element-name markup)))))))

(defmethod render-docstring-markup ((markup email-element) stream)
  (with-html-output (html stream)
    (:a :href (format nil "mailto:~A" (email-element-email markup))
	(str (email-element-email markup)))))

(defmethod render-docstring-markup ((markup link-element) stream)
  (with-html-output (html stream)
    (:a :href (link-element-url markup)
	(str (or (link-element-title markup)
		 (link-element-url markup))))))

(defmethod render-docstring-metadata ((version docstring-version) stream)
  (with-html-output (html stream)
    (htm
     (:div :class "version"
	   (:b "Version: ")
	   (str (docstring-version-version version))))))

(defmethod render-docstring-metadata ((todo docstring-todo) stream)
  (with-html-output (html stream)
    (htm
     (:div :class "todo"
	   (:b "TODO: ")
	   (render-docstring-markup (docstring-todo-todo todo) stream)))))

(defmethod render-docstring-metadata ((see docstring-see) stream)
  (with-html-output (html stream)
    (htm
     (:div :class "see"
	   (:b "See: ")
	   (loop for reference in (docstring-see-references see)
		do
		(render-docstring-markup reference stream))))))

(defmethod render-docstring-metadata ((date docstring-date) stream)
  (with-html-output (html stream)
    (htm
     (:div :class "date"
	   (:b "Date: ")
	   (render-docstring-markup (docstring-date-date date) stream)))))

(defmethod render-docstring-metadata ((author docstring-author) stream)
  (with-html-output (html stream)
    (htm
     (:div :class "author"
	   (:b "Author: ")
	   (render-docstring-markup (docstring-author-author author) stream)))))

(defmethod render-docstring-metadata ((tags docstring-tags) stream)
  (with-html-output (html stream)
    (htm
     (:ul :class "tags"
	  (loop for tag in (docstring-tags-tags tags)
		do
		   (let ((href (format nil "#~A" tag)))
		     (htm
		      (:li 
		       (:a :class "tag"
			   :href href
			   (str tag))))))))))

(defmethod render-docstring-metadata ((categories docstring-categories) stream)
  (with-html-output (html stream)
    (htm
     (:div :class "categories"
	   (:b "Categories: ")
	   (let ((category (first (docstring-categories-categories categories))))
	     (let ((href (format nil "#~A" category)))
	       (htm
		(:a :href href
		    (str category)))))
	   (loop for category in (cdr (docstring-categories-categories categories))
		do
		(let ((href (format nil "#~A" category)))
		  (htm
		   (str ", ")
		   (:a :href href
		       (str category)))))))))
