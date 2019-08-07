;;;; matching.lisp

(in-package #:ujo/core)

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

(defun bind-self (volume &key (constraints '(0)))
  "Bind VOLUME to itself."
  (bind-volumes volume volume constraints))

(defun bind-volumes-mutually (volume-1 volume-2 constraints)
  "Bind VOLUME-1 and VOLUME-2 to each other under CONSTRAINTS."
  (bind-volumes volume-1 volume-2 constraints)
  (bind-volumes volume-2 volume-1 constraints))

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

(defun volume-convert-fields (volume constraints &key transform)
  "Replace the fields in VOLUME specified by CONSTRAINT, to VOLUME objects."
  (let ((registry-name (mof:cat (name (find-registry (rid volume)))
                                (genstring "/")))
        (constraints (ensure-list constraints)))
    (loop :for constraint :in constraints
          :do (loop :for entry :in (walk-down volume :skip #'unitp)
                    :for field = (first (apply-constraints entry constraint))
                    :for volume = (import-field field :registry-name registry-name
                                                      :header '("element")
                                                      :transform transform
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

(defun blob-matching-p (field-1 field-2 &key (test #'string-equal))
  "Return true if two BLOB objects are considered equal to one another, by computing its Jaccard similarity."
  (let ((value-1 (value field-1))
        (value-2 (value field-2)))
    (>= (float (* (/ (length (intersection value-1 value-2 :test test))
                     (length (union value-1 value-2 :test test)))
                  100))
        *matching-threshold*)))