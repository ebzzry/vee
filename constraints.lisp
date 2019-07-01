;;;; constraints.lisp

(in-package #:muso/core)

(defgeneric full (object)
  (:documentation "Return the full, unsectioned version of OBJECT."))
(defmethod full ((object list)) object)
(defmethod full ((object entry)) object)

(defgeneric apply-selectors (query selectors)
  (:documentation "Apply items in selectors to create a new value."))
(defmethod apply-selectors ((query list) selectors)
  (loop :for fn :in selectors :collect (funcall fn query)))
(defmethod apply-selectors ((query entry) selectors)
  (apply-selectors (value query) selectors))

(defmacro build-selectors (indexes)
  "Define selectors."
  `(progn
     ,@(loop :for index :in indexes
             :for name = (read-from-string (mof:cat "select-" (write-to-string index)))
             :collect `(defun ,name (list) (elt list ,(1- index))))))

(defun field-index (field header)
  "Return the index of FIELD in HEADER."
  (loop :for h :in header
        :for count = 0 :then (1+ count)
        :when (string-equal field h)
        :return count))

(defun function-integer-p (object)
  "Return true if OBJECT is either a function or integer."
  (when (or (functionp object)
            (integerp object))
    t))

(defun get-field (field entry registry &key (use-header t) (fallback ""))
  "Return field from ENTRY within REGISTRY, where FIELD is either a string, function, or integer."
  (let* ((value (if (listp entry) entry (value entry)))
         (header (when use-header
                   (header (find-volume (vid entry) registry))))
         (index (cond ((function-integer-p field) field)
                      (header (field-index field header)))))
    (etypecase index
      (function (funcall index value))
      (integer (nth index value))
      (t fallback))))

(defun apply-constraints (constraints entry registry &key (use-header t))
  "Apply CONSTRAINTS to ENTRY within REGISTRY."
  (let ((constraints (ensure-list constraints)))
    (loop :for constraint :in constraints
          :collect (get-field constraint entry registry :use-header use-header))))

(defun find-fields-by-head (head entry &key (test *field-test*))
  "Return field from from ENTRY that satisfies HEAD."
  (loop :for field :in (value entry)
        :when (funcall test head (head field))
          :collect field))

(defun find-fields-by-value (value entry &key (test *field-test*))
  "Return field from from ENTRY that satisfies HEAD."
  (loop :for field :in (value entry)
        :when (funcall test value (value field))
          :collect field))

(defun find-fields-by-heads (heads entry &key (test *field-test*))
  "Return fields from ENTRY that satisfy HEADS."
  (loop :for field :in (value entry)
        :for head :in heads
        :nconc (find-fields-by-head head entry :test test)))

(defun find-fields-by-values (values entry &key (test *field-test*))
  "Return fields from ENTRY that satisfy VALUES."
  (loop :for field :in (value entry)
        :for value :in values
        :nconc (find-fields-by-value value entry :test test)))

(defun search-field (volume specifier)
  ""
  nil)
