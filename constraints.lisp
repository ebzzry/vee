;;;; constraints.lisp

(in-package #:muso/core)

(defun point (origin volume)
  "Return starting point based on the type of origin."
  (etypecase origin
    (function (funcall origin volume))
    (entry origin)))

(defun walk-up (volume &key (origin #'volume-end) (fn #'identity))
  "Return records from VOLUME starting from record ORIGIN applying FN to each record."
  (when (linkedp volume)
    (loop :for entry = (point origin volume) :then (prev entry)
          :while entry
          :collect (funcall fn entry))))

(defun walk-down (volume &key (origin #'volume-start) (fn #'identity) (skip #'false))
  "Return records from VOLUME starting from record ORIGIN applying FN to each record."
  (when (linkedp volume)
    (loop :for entry = (point origin volume) :then (next entry)
          :while entry
          :unless (funcall skip entry)
          :collect (funcall fn entry))))

(defun walk-left (volume)
  "Return records starting from VOLUME across all the other volumes, going left."
  (declare (ignorable volume))
  nil)

(defun walk-right (volume)
  "Return records starting from VOLUME across all the other volumes, going right."
  (declare (ignorable volume))
  nil)

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
  (:documentation "Return fields from ENTRY that satisfy CONSTRAINTS, where CONSTRAINTS is a list of header-specifiers or integer indexes.")
  (:method ((o entry) constraints &key (type :head) (test *field-test*))
    (let ((constraints (ensure-list constraints)))
      (loop :for constraint :in constraints
            :nconc (etypecase constraint
                     (string (dispatch-fields o (list constraint) :type type :test test))
                     (integer (list (nth constraint (fields o))))))))
  (:method ((o list) constraints &key (fallback ""))
    (let ((constraints (ensure-list constraints)))
      (loop :for constraint :in constraints
            :collect (etypecase constraint
                       (function (funcall constraint o))
                       (integer (nth constraint o))
                       (t fallback)))))
  (:method ((o volume) constraints &key (test *field-test*) merge)
    (let* ((constraints (ensure-list constraints))
           (entries (loop :for entry :in (walk-down o :skip #'unitp)
                          :collect (apply-constraints entry constraints :type :head :test test))))
      (if merge
          (reduce #'nconc entries)
          entries))))

(defun resolve-constraints (entry constraints)
  "Return the value of constraints application. This is primarily used to extract the values of fields, whether it is a BLOB or VOLUME object."
  (mapcar #'value (apply-constraints entry constraints)))

(defgeneric everyp (object entry constraints &key &allow-other-keys)
  (:documentation "Return true if OBJECT matches the applied version of ENTRY. TEST should invoke the correct function to check for equality.")
  (:method ((o list) (e entry) constraints &key (test *field-test*))
    (every test
           (apply-constraints o constraints)
           (resolve-constraints e constraints)))
  (:method ((o entry) (e entry) constraints &key (test *field-test*))
    (every test
           (resolve-constraints o constraints)
           (resolve-constraints e constraints))))

(defun find-other-volumes (volume registry)
  "Find the other volumes in REGISTRY."
  (let ((vid (vid volume)))
    (find-volumes registry :skip #'(lambda (v) (= vid (vid v))))))

(defun find-other-volume (volume)
  "Return the other volume in the same registry."
  (let* ((registry (find-registry (rid volume)))
         (volumes (find-other-volumes volume registry)))
    (if (= (length volumes) 1)
        (first volumes)
        -1)))

(defgeneric find-similar-entries (store entry constraints &key &allow-other-keys)
  (:documentation "Return entries from VOLUME that satisfy CONSTRAINTS as applied to ENTRY, where CONSTRAINTS is a list of header-specifiers to match against. The function specified by TEST will determine equality.

    (FIND-SIMILAR-ENTRIES VOLUME ENTRY '(\"country\"))

returns entries from VOLUME wherein the 'country' field is the same as that of ENTRY.

    (FIND-SIMILAR-ENTRIES VOLUME ENTRY '(\"country\" \"gender\"))

returns entries from VOLUME wherein the 'country' and 'gender' fields are the same as those of ENTRY.

This generic function is mainly used for matching against data that is already inside a registry.
")
  (:method ((v volume) (e entry) constraints &key (origin #'volume-start)
                                                  (test *field-test*)
                                                  entry-exclusive)
    (when (linkedp v)
      (let ((records (walk-down v :origin origin
                                  :skip #'(lambda (rec)
                                            (or (unitp rec) (when entry-exclusive (eql rec e)))))))
        (loop :for record :in records
              :when (everyp e record constraints :test test)
              :collect record))))
  (:method ((r registry) (e entry) constraints &key (origin #'volume-start)
                                                    (test *field-test*)
                                                    volume-exclusive
                                                    entry-exclusive)
    (let ((volumes (if volume-exclusive
                       (find-other-volumes (find-volume (vid e) r) r)
                       (find-volumes r))))
      (loop :for volume :in volumes
            :nconc (find-similar-entries volume e constraints
                                         :origin origin
                                         :test test
                                         :entry-exclusive entry-exclusive)))))

(defgeneric find-matching-entries (store specifiers &key &allow-other-keys)
  (:documentation "Return entries from STORE that SPECIFIERS, where SPECIFIERS are lists of header-specifier and header-value lists, or a single item of such type. The function specified by TEST will determine equality.

    (FIND-MATCHING-ENTRIES STORE '((\"email\" \"sstanleynh@wp.com\") (\"country\" \"Philippines\")) :TYPE :OR)

returns entries that have set either the email to sstanleynh@wp.com or the country to Philippines.

    (FIND-MATCHING-ENTRIES STORE '((\"email\" \"pwagner1x@gravatar.com\") (\"country\" \"Italy\")) :TYPE :AND)

returns entries that have set both the email to pwagner1x@gravatar.com and the country to Italy.

This generic function is mainly used for matching againstn data that is provided by the user, which may or may not exist inside a registry.
")
  (:method ((v volume) specifiers &key (type :or) (test *field-test*))
    (let ((entries (walk-down v :skip #'unitp))
          (specifiers (if (and (every-string-p specifiers)
                               (not (every-list-p specifiers)))
                          (list specifiers)
                          specifiers)))
      (loop :for entry :in entries
            :nconc
               (ecase type
                 (:or (lparallel:premove
                       nil
                       (lparallel:pmapcan #'(lambda (field)
                                              (lparallel:pmapcar
                                               #'(lambda (specifier)
                                                   (when (destructuring-bind (head value) specifier
                                                           (and (funcall test (head field) head)
                                                                (funcall test (value field) value)))
                                                     entry))
                                               specifiers))
                                          (fields entry))))
                 (:and (destructuring-bind (heads values)
                           (apply #'mapcar #'list specifiers)
                         (let ((fields (apply-constraints entry heads :type :head)))
                           (when (every test (mapcar #'value fields) values)
                             (list entry)))))))))
  (:method ((r registry) specifiers &rest args)
            (let ((volumes (find-volumes r)))
              (lparallel:pmapcan #'(lambda (volume)
                                     (apply #'find-matching-entries volume specifiers args))
                                 volumes))))

(defun bind-matches (store entry constraints &rest args)
  "Bind matching records of ENTRY in STORE."
  (let ((matches (apply #'find-similar-entries store entry constraints args)))
    (when matches
      (setf (gethash constraints (matches entry)) matches))))

(defun bind-volume-volume (volume-1 volume-2 constraints &rest args)
  "Bind VOLUME-1 to VOLUME-2."
  (loop :for entry :in (walk-down volume-1 :skip #'unitp)
        :do (apply #'bind-matches volume-2 entry constraints
                   :entry-exclusive t
                   args)))

(defun bind-volumes (volume-1 volume-2 constraints)
  "Bind VOLUME-1 to VOLUME-2 with ORIGIN modifications."
  (let ((origin #'volume-start))
    (dolist (entry (walk-down volume-1 :skip #'unitp))
      (when origin
        (let ((matches (find-similar-entries volume-2 entry constraints :origin origin)))
          (when matches
            (setf (gethash constraints (matches entry)) matches)
            (setf origin (next (first (gethash constraints (matches entry)))))))))))

(defun bind-volume-registry (volume registry constraints &rest args)
  "Bind VOLUME to the other volumes in REGISTRY. If there is only one volume inside a registry the volume will only be bound to itself."
  (loop :for entry :in (walk-down volume :skip #'unitp)
        :do (apply #'bind-matches registry entry constraints
                   :entry-exclusive t
                   :volume-exclusive t
                   args)))

(defun bind-wall (registry &optional (constraints *default-constraints*))
  "Bind the wall in REGISTRY to the other volumes."
  (let ((wall (wall registry)))
    (bind-volume-registry wall registry constraints :volume-exclusive t)))

(defun bind-all-volumes (registry &rest args)
  "Bind all the volumes in REGISTRY to one another."
  (let ((volumes (find-volumes registry)))
    (loop :for volume :in volumes :do (apply #'bind-volume-registry volume args))))

(defun field-volume (field &rest args)
  "Return a volume from the feed created from FIELD."
  (apply #'import-field field :return 'volume args))

(defun extract-field (entry head)
  "Extract a field from ENTRY specified by HEAD."
  (loop :for field :in (fields entry)
        :when (string-equal (head field) head)
        :return (value field)))

(defgeneric volume-convert-field (entry constraint volume)
  (:documentation "Replace a FIELD in ENTRY specified by CONSTRAINT with VOLUME.")
  (:method ((e entry) (h string) (v volume))
    (loop :for field :in (fields e)
          :for count = 0 :then (1+ count)
          :when (string-equal (head field) h)
          :do (setf (value (nth count (fields e))) v)))
  (:method ((e entry) (i integer) (v volume))
    (setf (value (nth i (fields e))) v)))

(defun volume-convert-fields (volume constraints)
  "Replace the fields in VOLUME specified by CONSTRAINT, to VOLUME objects."
  (let ((registry-name (mof:cat (name (find-registry (rid volume)))
                                (genstring "/")))
        (constraints (ensure-list constraints)))
    (loop :for constraint :in constraints
          :do (loop :for entry :in (walk-down volume :skip #'unitp)
                    :for field = (first (apply-constraints entry constraint))
                    :for volume = (import-field field :registry-name registry-name
                                                      :header '("element")
                                                      :return 'volume)
                    :when volume
                    :do (volume-convert-field entry constraint volume)))))

(defun contains-matches-p (entry constraints)
  "Return true if ENTRY has matches under CONSTRAINTS."
  (multiple-value-bind (value existsp)
      (gethash constraints (matches entry))
    (declare (ignore value))
    (when existsp
      t)))

(defun match-percentage (volume constraints)
  "Return the amount of matching and non-matching entries in VOLUME."
  (let ((count (float (table-count volume))))
    (loop :for entry :in (walk-down volume :skip #'unitp)
          :counting (contains-matches-p entry constraints) :into matching
          :finally (return (* 100 (/ matching count))))))

(defgeneric simple-volume-matching-p (volume-1 volume-2)
  (:documentation "Return true if VOLUME-1 and VOLUME-2 are matching according to CONSTRAINTS.")
  (:method ((volume-1 volume) (volume-2 volume))
    (bind-volume-volume volume-1 volume-2 '(0))
    (let ((percentage (match-percentage volume-1 '(0))))
      (if (>= percentage *matching-threshold*)
          (values t percentage)
          (values nil percentage))))
  (:method ((volume-1 string) (volume-2 string))
    (simple-volume-matching-p (search-volume volume-1) (search-volume volume-2))))

(defgeneric extended-volume-matching-p (volume-1 volume-2)
  (:documentation "Return true if VOLUME-1 and VOLUME-2 are matching according to CONSTRAINTS.")
  (:method ((volume-1 volume) (volume-2 volume))
    (bind-volumes volume-1 volume-2 '(0))
    (bind-volumes volume-2 volume-1 '(0))
    (let* ((percentage-1 (match-percentage volume-1 '(0)))
           (percentage-2 (match-percentage volume-2 '(0)))
           (mean (/ (+ percentage-1 percentage-2) 2.0)))
      (if (>= mean *matching-threshold*)
          (values t mean)
          (values nil mean))))
  (:method ((volume-1 string) (volume-2 string))
    (extended-volume-matching-p (search-volume volume-1) (search-volume volume-2))))

(defun headp (head volume)
  "Return true if HEAD is a header in VOLUME."
  (when (member head (header volume) :test #'string-equal)
    t))

(defun collect-operators (terms volume)
  "Return the operators from TERMS. Operators are constraints that are not part of a volume header."
  (loop :for term :in terms
        :unless (headp term volume)
        :collect term))

(defun flatten-constraints (terms volume)
  "Return a list containing only items that match the header of VOLUME. This strips the meta characters from the constraints."
  (loop :for term :in terms
        :collect (if (headp term volume) term (strip-meta-char term))))

(defun dispatch-operators (volume operators)
  "Process the operators on VOLUME, handling ! and @ meta characters."
  (lparallel:pmapc #'(lambda (operator)
                       (cond ((string-end-p operator #\!)
                              (blob-convert-fields volume (strip-end-char operator)))
                             ((string-end-p operator #\@)
                              (volume-convert-fields volume (strip-end-char operator)))
                             (t nil)))
                   operators))

(defun find-duplicates (volume terms)
  "Return a list of entries that have similar properties according to TERMS, where each term is either a constraint or a string that constains a meta-character that indicates additional operations to be done beforehand."
  (let ((operators (collect-operators terms volume))
        (constraints (flatten-constraints terms volume)))
    (dispatch-operators volume operators)
    (let ((results (loop :for entry :in (walk-down volume :skip #'unitp)
                         :for offset = (volume-start volume) :then (next offset)
                         :while offset
                         :nconc (find-similar-entries volume entry constraints
                                                      :origin offset :entry-exclusive t))))
      (remove-duplicates results))))

(defun blobp (object)
  "Return true if OBJECT is a BLOB."
  (when (typep object 'blob)
    t))

(defun make-blob (field)
  "Return a BLOB instance from field data."
  (if (blobp (value field))
      (value field)
      (let ((text (filter-text (value field))))
        (make-instance 'blob :fid (id field) :value text :source (value field)))))

(defun blob-convert-fields (volume constraints)
  "Replace the fields in VOLUME specified by CONSTRAINT, to BLOB objects."
  (let ((constraints (ensure-list constraints)))
    (loop :for constraint :in constraints
          :do (loop :for entry :in (walk-down volume :skip #'unitp)
                    :for field = (first (apply-constraints entry constraint))
                    :for value = (value field)
                    :do (setf (value field) (make-blob field))))))

(defun blob-equal-p (field-1 field-2 &key (test #'string-equal))
  "Return true if two BLOB objects are considered equal to one another."
  (let ((value-1 (value field-1))
        (value-2 (value field-2)))
    (>= (float (* (/ (length (intersection value-1 value-2 :test test))
                     (length (union value-1 value-2 :test test)))
                  100))
        *matching-threshold*)))

(defun expunge-duplicates (volume terms)
  "Remove duplicate entries in VOLUME under TERMS, creating void registries as necessary."
  (let ((duplicates (with-levenshtein (find-duplicates volume terms)))
        (registry (find-registry (rid volume))))
    (lparallel:pmapc #'(lambda (entry) (banish entry registry))
                     duplicates)))

(defun write-file (volume file &key expand)
  "Print the contents of VOLUME to FILE. If EXPAND is true, blobs and volumes will expand to their original forms."
  (with-open-file (out (mof:expand-pathname file)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format out "~&~{~S~^,~}" (header volume))
    (loop :for entry :in (walk-down volume :skip #'unitp)
          :do (format out "~&~{~S~^,~}" (fields-values entry :expand expand)))))

(defun filter-file (infile outfile terms)
  "Remove duplicates in INFILE under TERMS then save the changes to OUTFILE."
  (let* ((volume-name (basename infile))
         (registry-name (basedir infile))
         (volume (import-file infile :extract-header t
                                     :volume-name volume-name
                                     :registry-name registry-name)))
    (expunge-duplicates volume terms)
    (write-file volume outfile :expand t)))
