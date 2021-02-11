;;;; matching.lisp

(in-package #:veda/core)

(defun bind-matches (store pool constraints &rest args)
  "Bind matching frames of POOL in STORE."
  (let ((matches (apply #'find-similar-pools store pool constraints args)))
    (when matches
      (setf (gethash constraints (matches pool)) matches))))

(defun bind-volume-volume (volume1 volume2 constraints &rest args)
  "Bind VOLUME1 to VOLUME2."
  (loop :for pool :in (walk-down volume1 :skip #'unitp)
        :do (apply #'bind-matches volume2 pool constraints
                   :pool-exclusive t
                   args)))

(defun bind-volumes (volume1 volume2 constraints)
  "Bind VOLUME1 to VOLUME2 with ORIGIN modifications."
  (let ((origin #'volume-start))
    (dolist (pool (walk-down volume1 :skip #'unitp))
      (when origin
        (let ((matches (find-similar-pools volume2 pool constraints :origin origin)))
          (when matches
            (setf (gethash constraints (matches pool)) matches)
            (setf origin (next (first (gethash constraints (matches pool)))))))))))

(defun bind-self (volume &key (constraints '(0)))
  "Bind VOLUME to itself."
  (bind-volumes volume volume constraints))

(defun bind-volumes-mutually (volume1 volume2 constraints)
  "Bind VOLUME1 and VOLUME2 to each other under CONSTRAINTS."
  (bind-volumes volume1 volume2 constraints)
  (bind-volumes volume2 volume1 constraints))

(defun bind-volume-registry (volume registry constraints &rest args)
  "Bind VOLUME to the other volumes in REGISTRY. If there is only one volume inside a registry the volume will only be bound to itself."
  (loop :for pool :in (walk-down volume :skip #'unitp)
        :do (apply #'bind-matches registry pool constraints
                   :pool-exclusive t
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

(defun cell-volume (cell &rest args)
  "Return a volume from the feed created from CELL."
  (apply #'import-cell cell :return 'volume args))

(defun extract-cell (pool head)
  "Extract a cell from POOL specified by HEAD."
  (loop :for cell :in (cells pool)
        :when (string-equal (head cell) head)
        :return (value cell)))

(defgeneric volume-convert-cell (pool constraint volume)
  (:documentation "Replace a CELL in POOL specified by CONSTRAINT with VOLUME.")
  (:method ((p pool) (h string) (v volume))
    (loop :for cell :in (cells p)
          :for count = 0 :then (1+ count)
          :when (string-equal (head cell) h)
          :do (setf (value (nth count (cells p))) v)))
  (:method ((p pool) (i integer) (v volume))
    (setf (value (nth i (cells p))) v)))

(defun volume-convert-cells (volume constraints &key transform)
  "Replace the cells in VOLUME specified by CONSTRAINT, to VOLUME objects."
  (let ((registry-name (cat (name (find-registry (rid volume)))
                                (genstring "/")))
        (constraints (ensure-list constraints)))
    (loop :for constraint :in constraints
          :do (loop :for pool :in (walk-down volume :skip #'unitp)
                    :for cell = (first (apply-constraints pool constraint))
                    :for volume = (import-cell cell :registry-name registry-name
                                                    :header '("element")
                                                    :transform transform
                                                    :return 'volume)
                    :when volume
                    :do (volume-convert-cell pool constraint volume)))))

(defun contains-matches-p (pool constraints)
  "Return true if POOL has matches under CONSTRAINTS."
  (multiple-value-bind (value existsp)
      (gethash constraints (matches pool))
    (declare (ignore value))
    (when existsp
      t)))

(defun match-percentage (volume constraints)
  "Return the amount of matching and non-matching pools in VOLUME."
  (let ((count (float (table-count volume))))
    (loop :for pool :in (walk-down volume :skip #'unitp)
          :counting (contains-matches-p pool constraints) :into matching
          :finally (return (* 100 (/ matching count))))))

(defgeneric simple-volume-matching-p (volume1 volume2)
  (:documentation "Return true if VOLUME1 and VOLUME2 are matching according to CONSTRAINTS.")
  (:method ((volume1 volume) (volume2 volume))
    (bind-volume-volume volume1 volume2 '(0))
    (let ((percentage (match-percentage volume1 '(0))))
      (if (>= percentage *matching-threshold*)
          (values t percentage)
          (values nil percentage))))
  (:method ((volume1 string) (volume2 string))
    (simple-volume-matching-p (search-volume volume1) (search-volume volume2))))

(defgeneric extended-volume-matching-p (volume1 volume2)
  (:documentation "Return true if VOLUME1 and VOLUME2 are matching according to CONSTRAINTS.")
  (:method ((volume1 volume) (volume2 volume))
    (bind-volumes volume1 volume2 '(0))
    (bind-volumes volume2 volume1 '(0))
    (let* ((percentage1 (match-percentage volume1 '(0)))
           (percentage2 (match-percentage volume2 '(0)))
           (mean (/ (+ percentage1 percentage2) 2.0)))
      (if (>= mean *matching-threshold*)
          (values t mean)
          (values nil mean))))
  (:method ((volume1 string) (volume2 string))
    (extended-volume-matching-p (search-volume volume1) (search-volume volume2))))

(defun blobp (object)
  "Return true if OBJECT is a BLOB."
  (when (typep object 'blob)
    t))

(defun make-blob (cell)
  "Return a BLOB instance from cell data."
  (if (blobp (value cell))
      (value cell)
      (let ((text (filter-text (value cell))))
        (make-instance 'blob :fid (id cell) :value text :source (value cell)))))

(defun blob-convert-cells (volume constraints)
  "Replace the cells in VOLUME specified by CONSTRAINT, to BLOB objects."
  (let ((constraints (ensure-list constraints)))
    (loop :for constraint :in constraints
          :do (loop :for pool :in (walk-down volume :skip #'unitp)
                    :for cell = (first (apply-constraints pool constraint))
                    :for value = (value cell)
                    :do (setf (value cell) (make-blob cell))))))

(defun blob-matching-p (cell1 cell2 &key (test #'string-equal))
  "Return true if two BLOB objects are considered equal to one another, by computing its Jaccard similarity."
  (let ((value1 (value cell1))
        (value2 (value cell2)))
    (>= (float (* (/ (length (intersection value1 value2 :test test))
                     (length (union value1 value2 :test test)))
                  100))
        *matching-threshold*)))
