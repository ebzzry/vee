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
  (apply-selectors (mapcar #'value (fields query)) selectors))

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

;;; Note: REGISTRY may no longer be needed because the header is already part of the field
;;; Note: FIELD-INDEX may also no longer be needed
(defun get-field (field entry registry &key (use-header t) (fallback ""))
  "Return field from ENTRY within REGISTRY, where FIELD is either a string, function, or integer."
  (let* ((value (if (listp entry) entry (fields entry)))
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

(defun make-column (value)
  "Return a column object."
  (make-instance 'column :value value))

(defun extract-column (index volume)
  "Return a column object from VOLUME specified by 1-indexed INDEX."
  (flet ((fn (entry)
           (nth index (fields entry))))
    (let* ((entries (walk-down volume :skip #'unitp))
           (value (mapcar #'fn entries)))
      (make-column value))))

(defun view-column (index volume)
  "Return a column from volume in a readable form."
  (let ((value (mapcar #'value (fields (extract-column index volume)))))
    (format t "~{~S~^ ~}" value)))

(defun print-matches (entry)
  "Print the chain of matches from entry."
  (let ((matches (matches entry)))
    (when matches
      (format t "~{~S~^ ~}" (mapcar #'value matches)))))

(defun extract-fields (constraints volume &key (test *field-test*))
  "Extract the fields from VOLUME that satisfy CONSTRAINTS."
  (loop :for entry :in (walk-down volume :skip #'unitp)
        :nconc (find-fields constraints entry :type :head :test test)))

;;; Note: maybe use PREV and NEXT for field traversal
(defun find-fields (constraints entry &key type (test *field-test*))
  "Return fields from ENTRY that satisfy QUERY, where QUERY is a list of string header-specifier, string field value, or integer index. The result is designed to be not orthogonal to the length of CONSTRAINTS because header-specifiers can exist multiple times in a header."
  (let ((constraints (ensure-list constraints))
        (fields (fields entry))
        (func (intern (string type))))
    (ecase type
      (:index
       (loop :for constraint :in constraints
             :collect (nth constraint fields)))
      ((:head :value)
       (loop :for constraint :in constraints
             :nconc (loop :for field :in fields
                          :when (funcall test constraint (funcall func field))
                          :collect field))))))

;;; (search-entries volume '(("email" "sstanleynh@wp.com") ("country" "Philippines")) :type :or)
;;; (search-entries volume '(("email" "pwagner1x@gravatar.com") ("country" "Italy")) :type :and)
(defun search-entries (volume specifiers &key (type :or) (test *field-test*))
  "Search VOLUME for entries that satisfy SPECIFIERS."
  (let ((entries (walk-down volume :skip #'unitp)))
    (loop :for entry :in entries
          :nconc (ecase type
                   (:or (loop :for field :in (fields entry)
                              :nconc (loop :for specifier :in specifiers
                                           :when (destructuring-bind (head value) specifier
                                                   (and (funcall test (head field) head)
                                                        (funcall test (value field) value)))
                                           :collect entry)))
                   (:and (destructuring-bind (heads values)
                             (apply #'mapcar #'list specifiers)
                           (let ((fields (find-fields heads entry :type :head)))
                             (when (every test (mapcar #'value fields) values)
                               (list entry)))))))))