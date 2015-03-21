(in-package :simple-doc)

(defun read-file-to-string (file)
  (with-open-file (stream file)
    (let ((seq (make-string (file-length stream))))
      (read-sequence seq stream)
      seq)))

(defparameter *categories* '(:function :macro :generic-function :slot-accessor :variable :class :condition :constant))

(defun exported-p (sym)
  (cond
    ((consp sym)
     (assert (eql (first sym) 'setf))
     (exported-p (second sym)))
    (t
     (eql (nth-value 1 (find-symbol (symbol-name sym)
				    (symbol-package sym)))
	  :external))))

(defun public-packages ()
  (loop for p in (list-all-packages)
     when (and (has-exported-symbols-p p) (not (eql p (find-package :keyword))))
     collect p))

(defun has-exported-symbols-p (package)
  (do-external-symbols (sym package)
    (declare (ignore sym))
    (return-from has-exported-symbols-p t))
  nil)

(defun needs-documentation (package)
  (loop for what in *categories*
     for names = (names package what)
     when names nconc
       (loop for sym in names unless (docs-for sym what) collect (list sym what))))

(defun names (package what)
  (sort
   (loop for sym being the present-symbols of package
      when (is sym what) collect sym
      when (is `(setf ,sym) what) collect `(setf ,sym))
   #'name<))

(defun name< (n1 n2)
  (cond
    ((and (symbolp n1) (symbolp n2))
     (string< n1 n2))
    ((and (symbolp n1) (listp n2))
     (cond
       ((string< n1 (second n2)) t)
       ((string< (second n2) n1) nil)
       (t t)))
    ((and (listp n1) (symbolp n2))
     (cond
       ((string< (second n1) n2) t)
       ((string< n2 (second n1)) nil)
       (t nil)))
    ((and (listp n1) (listp n2))
     (string< (second n1) (second n2)))))

(defgeneric is (symbol what))
(defgeneric docs-for (symbol what))
(defgeneric pluralization (what))

(defmethod pluralization (what) (format nil "~as" what))

(defmacro define-category (name (symbol what) &body body)
  (let ((is-test (cdr (assoc :is body)))
        (get-docs (cdr (assoc :docs body)))
        (pluralization (cdr (assoc :pluralization body))))
    `(progn
       (defmethod is (,symbol (,what (eql ',name))) ,@is-test)
       (defmethod docs-for (,symbol (,what (eql ',name))) ,@get-docs)
       ,@(when pluralization
               `((defmethod pluralization ((,what (eql ',name)))
                   ,@pluralization))))))

(defun function-p (name)
  (ignore-errors (fdefinition name)))

(defun macro-p (name)
  (and (symbolp name) (macro-function name)))

(defun generic-function-p (name)
  (and (function-p name)
       (typep (fdefinition name) 'generic-function)))

(defun variable-p (name)
  (ignore-errors (boundp name)))

(defun automatic-p (docstring)
  (member docstring '("automatically generated reader method" "automatically generated writer method") :test #'string-equal))

(defun gf-docs (name)
  (let ((simple (documentation (fdefinition name) t))
        (from-setf (and (consp name) (documentation (fdefinition (second name)) t))))

    (or
     (and simple (not (automatic-p simple)) (format nil "The ~a" simple))
     (and from-setf (not (automatic-p from-setf)) (format nil "Set the ~a" from-setf))
     (first (remove-if #'automatic-p (remove nil (mapcar
                         (lambda (m) (documentation m t))
                         (closer-mop:generic-function-methods (fdefinition name)))))))))

(define-category :function (symbol what)
  (:is (and (function-p symbol)
            (not (or (is symbol :macro)
                     (is symbol :generic-function)
                     (is symbol :slot-accessor)))))
  (:docs (documentation symbol 'function)))

(define-category :macro (symbol what)
  (:is (macro-p symbol))
  (:docs (documentation symbol 'function)))

(define-category :generic-function (symbol what)
  (:is (and (generic-function-p symbol)
            (not (is symbol :slot-accessor))))
  (:docs (documentation symbol 'function)))

(define-category :class (symbol what)
  (:is (and (find-class symbol nil) (not (is symbol :condition))))
  (:docs (documentation (find-class symbol) t))
  (:pluralization (format nil "~aes" what)))

(define-category :condition (symbol what)
  (:is (and (find-class symbol nil) (subtypep (find-class symbol nil) 'condition)))
  (:docs (documentation (find-class symbol) t)))

(define-category :variable (symbol what)
  (:is (and (variable-p symbol) (not (is symbol :constant))))
  (:docs   (documentation symbol 'variable)))

(define-category :constant (symbol what)
  (:is (and (variable-p symbol) (constantp symbol)))
  (:docs (documentation symbol 'variable)))

(define-category :slot-accessor (symbol what)
  (:is (and (generic-function-p symbol)
            (some (lambda (m)
                    (or (eql (class-of m) (find-class 'closer-mop:standard-reader-method))
                        (eql (class-of m) (find-class 'closer-mop:standard-writer-method))))
                  (closer-mop:generic-function-methods (fdefinition symbol)))))
  (:docs (gf-docs symbol)))

(defun make-unique-name (thing what)
  (format nil "~A-~A" thing what))

(defparameter *possible-readme-types* '(nil "txt" "md" "TXT"))

(defun readme-text (package-name)
  (when-let ((file (find-readme package-name)))
    (with-open-file (in file :if-does-not-exist nil)
      (when in
        (with-output-to-string (s)
          (loop for line = (read-line in nil nil)
             while line do (write-line line s)))))))

(defun find-readme (package-name)
  (let ((dir (ignore-errors (asdf:system-relative-pathname package-name nil))))
    (when dir
      (loop for type in *possible-readme-types*
         when (probe-file (merge-pathnames (make-pathname :name "README" :type type) dir))
         return it))))
