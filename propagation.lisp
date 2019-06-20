;;;; propagation.lisp

(in-package #:muso/core)

(defgeneric full (object)
  (:documentation "Return the full, unsectioned version of OBJECT."))
(defmethod full ((object list)) object)
(defmethod full ((object entry)) object)

;;; Note: handle SB-KERNEL:INDEX-TOO-LARGE-ERROR here
(defun apply-selectors (entry selectors)
  "Apply items in selectors to create a new value."
  (loop :for fn :in selectors :collect (funcall fn (value entry))))

(defun entries-equal-p (entry-1 entry-2
                        &key (selectors *default-selectors*)
                          (test *field-test*))
  "Return true if ENTRY-1 is equal to ENTRY-2 using SELECTORS. SELECTORS is a list of functions that will be used to collect fields in an entry."
  (let ((selected-1 (apply-selectors entry-1 selectors))
        (selected-2 (apply-selectors entry-2 selectors)))
    (every test selected-1 selected-2)))

(defun find-entries (column)
  "Return entries from COLUMN."
  (find-records column :test #'entryp))

(defun find-units (column)
  "Return units from COLUMN."
  (find-records column :test #'unitp))

(defgeneric find-matches (entry column registry &key &allow-other-keys)
  (:documentation "Return a list of matching instances of ENTRY in target COLUMN using SELECTOR and TEST."))
(defmethod find-matches ((item list) (c column) (r registry)
                         &key (selectors *default-selectors*)
                           (test *field-test*))
  (loop :for entry :in (find-entries c)
        :for cluster = (apply-selectors entry selectors)
        :when (every test item cluster)
          :collect entry))
(defmethod find-matches ((e entry) (c column) (r registry)
                         &key (selectors *default-selectors*)
                           (test *field-test*))
  (find-matches (apply-selectors e selectors)
                c r
                :selectors selectors :test test))

;;; Note: no longer reliable because of the new table mechanism
(defun compute-offset (column entry)
  "Return the offset in the wall for the matching entry."
  (- (id entry) (id (first (find-records column)))))

(defun offsets (query column registry
                &key (selectors *default-selectors*)
                  (test *field-test*))
  "Return the offsets of a query in COLUMN within REGISTRY."
  (mapcar #'(lambda (entry)
              (compute-offset column entry))
          (find-matches query column registry :selectors selectors :test test)))

(defun point (origin column)
  "Return starting point based on the type of origin."
  (etypecase origin
    (function (funcall origin column))
    (entry origin)))

(defun column-start (column)
  "Return the first entry in COLUMN."
  (let ((records (find-records column)))
    (loop :for record :in records
          :when (and (null (prev record))
                     (next record))
            :return record)))

(defun column-end (column)
  "Return the last entry in COLUMN."
  (let ((records (nreverse (find-records column))))
    (loop :for record :in records
          :when (and (prev record)
                     (null (next record)))
            :return record)))

(defun walk-up (column &key (origin #'column-end) (fn #'identity))
  "Return records from COLUMN starting from record ORIGIN applying FN to each record."
  (when (linked column)
    (loop :for entry = (point origin column) :then (prev entry)
          :while entry
          :collect (funcall fn entry))))

(defun walk-down (column &key (origin #'column-start) (fn #'identity))
  "Return records from COLUMN starting from record ORIGIN applying FN to each record."
  (when (linked column)
    (loop :for entry = (point origin column) :then (next entry)
          :while entry
          :collect (funcall fn entry))))

(defun walk-left (column)
  "Return records starting from column across all the other columns, going left."
  (declare (ignorable column))
  nil)

(defun walk-right (column)
  "Return records starting from column across all the other columns, going right."
  (declare (ignorable column))
  nil)

(defmethod initialize-instance :after ((u unit) &key registry)
  "Update unit U in REGISTRY."
  (let ((counter (spawn-ucounter registry)))
    (with-slots (id) u
      (setf id counter))))

(defun make-unit (cid registry)
  "Create an instance of the unit class."
  (make-instance 'unit :cid cid :registry registry))

(defun forge-unit (column registry)
  "Create a unit under COLUMN within REGISTRY."
  (let* ((cid (cid column))
         (unit (make-unit cid registry)))
    (add-record unit registry)
    (add-record unit column)
    unit))

(defun link-unit-before (record column registry)
  "Link a UNIT before RECORD in COLUMN within REGISTRY."
  (let ((unit (forge-unit column registry)))
    (cond ((column-start-p record)
           (progn (setf (next unit) record)
                  (setf (prev record) unit)))
          (t (progn (setf (prev unit) (prev record))
                    (setf (next unit) record)
                    (setf (next (prev record)) unit)
                    (setf (prev record) unit))))
    unit))

(defun link-unit-after (record column registry)
  "Link a UNIT after RECORD in COLUMN within REGISTRY."
  (let ((unit (forge-unit column registry)))
    (cond ((column-end-p record)
           (progn (setf (prev unit) record)
                  (setf (next record) unit)))
          (t (progn (setf (prev unit) record)
                    (setf (next unit) (next record))
                    (setf (prev (next record)) unit)
                    (setf (next record) unit))))
    unit))

(defun link-unit (record column registry &key (position :after))
  "Link a unit POSITION RECORD in COLUMN within REGISTRY."
  (when (keywordp position)
    (let ((fn (intern (mof:cat "LINK-UNIT-" (string position)))))
      (funcall fn record column registry))))

(defun link-units (record column registry count &key (position :after))
  "Link COUNT units POSITION RECORD in COLUMN within REGISTRY."
  (flet ((link (rec col reg pos)
           (link-unit rec col reg :position pos)))
    (loop :for count :from 1 :to count
          :for unit = (link record column registry position)
            :then (link unit column registry position)
          :finally (return column))))
