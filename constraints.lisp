;;;; constraints.lisp

(in-package #:muso/core)

(defun fields-values (entry)
  "Return the values contained inside ENTRY."
  (mapcar #'value (fields entry)))

(defun extract-fields (constraints volume &key (test *field-test*))
  "Extract the fields from VOLUME that satisfy CONSTRAINTS."
  (loop :for entry :in (walk-down volume :skip #'unitp)
        :collect (apply-constraints entry constraints :type :head :test test)))

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

(defgeneric apply-constraints (object constraints &key &allow-other-keys)
  (:documentation "Return fields from ENTRY that satisfy CONSTRAINTS, where CONSTRAINTS is a list of header-specifiers or integer indexes."))
(defmethod apply-constraints ((o entry) constraints &key (type :head) (test *field-test*))
  (loop :for constraint :in constraints
        :nconc (etypecase constraint
                 (string (dispatch-fields o (list constraint) :type type :test test))
                 (integer (list (nth constraint (fields o)))))))
(defmethod apply-constraints ((o list) constraints &key (fallback ""))
  (let ((constraints (ensure-list constraints)))
    (loop :for constraint :in constraints
          :collect (etypecase constraint
                     (function (funcall constraint o))
                     (integer (nth constraint o))
                     (t fallback)))))

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
