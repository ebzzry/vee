;;;; constraints.lisp

(in-package #:muso/core)

(defgeneric apply-constraints (object constraints &key &allow-other-keys)
  (:documentation "Return fields from ENTRY that satisfy CONSTRAINTS, where CONSTRAINTS is a list of header-specifiers or integer indexes."))
(defmethod apply-constraints ((o entry) constraints &key (type :head) (test *field-test*))
  (let ((constraints (ensure-list constraints)))
    (loop :for constraint :in constraints
          :nconc (etypecase constraint
                   (string (dispatch-fields o (list constraint) :type type :test test))
                   (integer (list (nth constraint (fields o))))))))
(defmethod apply-constraints ((o list) constraints &key (fallback ""))
  (let ((constraints (ensure-list constraints)))
    (loop :for constraint :in constraints
          :collect (etypecase constraint
                     (function (funcall constraint o))
                     (integer (nth constraint o))
                     (t fallback)))))
(defmethod apply-constraints ((o volume) constraints &key (test *field-test*) merge)
  "Extract the fields from VOLUME that satisfy CONSTRAINTS."
  (let* ((constraints (ensure-list constraints))
         (entries (loop :for entry :in (walk-down o :skip #'unitp)
                        :collect (apply-constraints entry constraints :type :head :test test))))
    (if merge
        (reduce #'nconc entries)
        entries)))

(defgeneric everyp (object entry constraints &key &allow-other-keys)
  (:documentation "Return true if OBJECT matches the applied version of ENTRY."))
(defmethod everyp ((o list) (e entry) constraints &key (test *field-test*))
  (every test
         (apply-constraints o constraints)
         (mapcar #'value (apply-constraints e constraints))))
(defmethod everyp ((o entry) (e entry) constraints &key (test *field-test*))
  (every test
         (mapcar #'value (apply-constraints o constraints))
         (mapcar #'value (apply-constraints e constraints))))

(defun find-other-volumes (volume registry)
  "Find the other volumes in REGISTRY."
  (let ((vid (vid volume)))
    (find-volumes registry :skip #'(lambda (v) (= vid (vid v))))))

(defgeneric find-similar-entries (store entry constraints &key &allow-other-keys)
  (:documentation "Return entries from VOLUME that satisfy CONSTRAINTS, where CONSTRAINTS is a list of header-specifiers to match against.

    (FIND-SIMILAR-ENTRIES VOLUME ENTRY :CONSTRAINTS '(\"country\"))

returns entries from VOLUME wherein the 'country' field is the same as that of ENTRY.

    (FIND-SIMILAR-ENTRIES VOLUME ENTRY :CONSTRAINTS '(\"country\" \"gender\"))

returns entries from VOLUME wherein the 'country' and 'gender' fields are the same as those of ENTRY.

This generic function is mainly used for matching against data that is already inside a registry.
"))
(defmethod find-similar-entries ((v volume) (e entry) constraints &key (origin #'volume-start)
                                                                    (test *field-test*))
  (when (linkedp v)
    (loop :for record :in (walk-down v :origin origin :skip #'unitp)
          :for offset = 0 :then (1+ offset)
          :when (everyp e record constraints :test test)
            ;; :collect (make-match record v offset)
            :collect record)))
(defmethod find-similar-entries ((r registry) (e entry) constraints &key (origin #'volume-start)
                                                                      (test *field-test*)
                                                                      exclusive)
  (let ((volumes (if exclusive
                     (find-other-volumes (find-volume (vid e) r) r)
                     (find-volumes r))))
    (loop :for volume :in volumes
          :nconc (find-similar-entries volume e constraints :origin origin :test test))))

(defgeneric find-similar-record (query store constraints &key &allow-other-keys)
  (:documentation "Return the first record from STORE that matches QUERY."))
(defmethod find-matching-record ((query list) (v volume) constraints
                                 &key (origin #'volume-start)
                                   (test *field-test*))
  (when (linkedp v)
    (loop :for record :in (walk-down v :origin origin)
          :for offset = 0 :then (1+ offset)
          :when (everyp query record constraints :test test)
            ;; :return (make-match record v offset)
            :return record)))
(defmethod find-similar-record ((query entry) (v volume) constraint &rest args)
  (apply #'find-similar-record (fields query) v constraint args))

(defun dispatch-fields (entry constraints &key (type :head) (test *field-test*))
  "Return fields from ENTRY that satisfy CONSTRAINTS. The result is designed to be not orthogonal to the length of CONSTRAINTS because header-specifiers can exist multiple times in a header."
  (let ((constraints (ensure-list constraints))
        (fields (fields entry))
        (func (intern (string type))))
    (ecase type
      ((:index)
       (loop :for constraint :in constraints
             :collect (nth constraint fields)))
      ((:head :value)
       (loop :for constraint :in constraints
             :nconc (loop :for field :in fields
                          :when (funcall test constraint (funcall func field))
                            :collect field))))))

(defgeneric find-matching-entries (store specifiers &key &allow-other-keys)
  (:documentation "Return entries from STORE that SPECIFIERS, where SPECIFIERS are lists of header-specifier and header-value lists, or a single item of such type.

    (FIND-MATCHING-ENTRIES STORE '((\"email\" \"sstanleynh@wp.com\") (\"country\" \"Philippines\")) :TYPE :OR)

returns entries that have set either the email to sstanleynh@wp.com or the country to Philippines.

    (FIND-MATCHING-ENTRIES STORE '((\"email\" \"pwagner1x@gravatar.com\") (\"country\" \"Italy\")) :TYPE :AND)

returns entries that have set both the email to pwagner1x@gravatar.com and the country to Italy.

This generic function is mainly used for matching againstn data that is provided by the user, which may or may not exist inside a registry.
"))
(defmethod find-matching-entries ((v volume) specifiers &key (type :or) (test *field-test*))
  (let ((entries (walk-down v :skip #'unitp))
        (specifiers (if (and (every-string-p specifiers)
                             (not (every-list-p specifiers)))
                        (list specifiers)
                        specifiers)))
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
                           (let ((fields (apply-constraints entry heads :type :head)))
                             (when (every test (mapcar #'value fields) values)
                               (list entry)))))))))
(defmethod find-matching-entries ((r registry) specifiers &rest args)
  (let ((volumes (find-volumes r)))
    (loop :for volume :in volumes
          :nconc (apply #'find-matching-entries volume specifiers args))))

(defun split-field (field &optional (regex "\\s+"))
  "Split a field into components."
  (cl-ppcre:split regex (value field)))

(defun make-feed (field &rest args)
  "Create a feed from the text value stored in FIELD using predefined rules."
  (mapcar #'list (apply #'split-field field args)))
