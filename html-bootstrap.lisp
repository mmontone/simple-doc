(in-package :simple-doc)

(defmacro with-bs-page (stream &body body)
  `(with-html-output (html ,stream)
     (:html
       (:head
	(:title (fmt "Simple doc for ~A" package))
	(:link :rel "stylesheet" :type "text/css" :href "bootstrap/css/bootstrap.css"))
       (:body
	,@body))))

(defun generate-html-bootstrap-doc (output-folder package)
  ;; Ensure the output directory exists
  (ensure-directories-exist output-folder)

  ;; Copy Twitter html Bootstrap stuff
  (trivial-shell:shell-command
   (format nil "cp -r ~A ~A"
	   (asdf:system-relative-pathname :simple-doc "bootstrap/")
	   output-folder))

  ;; Generate index page
  (bs-generate-main-page output-folder package)

  ;; Generate category pages
  (loop for category in *categories*
     do
       (bs-generate-category-page output-folder package category)))

(defun bs-generate-category-page (output-folder package category)
  (let ((output-filename (merge-pathnames (format nil "~A.html" (string-downcase (symbol-name category)))
					  output-folder)))
    (with-open-file (stream output-filename
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (with-bs-page stream
	(bs-navbar package stream category)
	(:div :class "content"
	      (:div :class "page-header"
		    (:h1 (str (pluralization (string-capitalize (symbol-name category))))))
	      (loop for name in (names package category)
		 do
		   (bs-render-category-element category name stream)))))))

(defun bs-generate-main-page (output-folder package)
  ;; Generate index.html  
  (let ((output-filename (merge-pathnames "index.html" output-folder)))
    (with-open-file (stream output-filename
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (with-bs-page stream
	(bs-navbar package stream)
	(:div :class "content"
	      (:div :class "page-header"
		    (:h1 (str (package-name package))))
	      (:pre
	       (str (readme-text (alexandria:make-keyword (package-name package))))))))))

(defun bs-navbar (package stream &optional active-category)
  (with-html-output (html stream)
    (:nav :class "navbar navbar-default"
	  :role "navigation"
	  (:div :class "container-fluid"
		(:div :class "navbar-header"
		      (:button :type "button" :class "navbar-toggle"
			       :data-toggle "collapse" :data-target "#bs-example-navbar-collapse-1"
			       (:span :class "sr-only" (str "Toggle navigation"))
			       (:span :class "icon-bar")
			       (:span :class "icon-bar")
			       (:span :class "icon-bar"))
      
		      (:a :class "navbar-brand" :href "index.html" (str (package-name package))))
		
		(:div :class "collapse navbar-collapse"
		      :id "bs-example-navbar-collapse-1"
		      (:ul :class "nav navbar-nav"
			   (loop for category in *categories*
			      do
				(htm
				 (:li :class (if (equalp category active-category)
						 "active"
						 "")
				      (:a :href (format nil "~A.html" (string-downcase (symbol-name category)))
					  (str (pluralization (string-capitalize (symbol-name category))))))))
			   (:form :class "navbar-form navbar-left" :role "search"
				  (:div :class "form-group"
					(:input :type "text" :class "form-control" :placeholder "Search"))
				  (:button :type"submit" :class "btn btn-default"
					   (str "Search")))))))))

(defmethod bs-render-category-element ((category (eql :function)) function stream)
  (with-html-output (html stream)
    (let ((lambda-list (sb-introspect:function-lambda-list function)))
      (htm
       (:div :class "panel panel-default"
	     :id (make-unique-name function category)
	     (:div :class "panel-heading"
		   (fmt "~A ~A" function lambda-list))
	     (:div :class "panel-body"
		   (render-function function stream)))))))

(defmethod bs-render-category-element (category thing stream)
  (with-html-output (html stream)
    (:div :class "panel panel-default"
	     :id :id (make-unique-name thing category)
	     (:div :class "panel-heading"
		   (str thing))
	     (:div :class "panel-body"
		   (:p (str (docs-for thing category)))))))
