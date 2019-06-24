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

(defun walk-up (column &key (origin #'column-end) (fn #'identity))
  "Return records from COLUMN starting from record ORIGIN applying FN to each record."
  (when (linkedp column)
    (loop :for entry = (point origin column) :then (prev entry)
          :while entry
          :collect (funcall fn entry))))

(defun walk-down (column &key (origin #'column-start) (fn #'identity) (skip #'false))
  "Return records from COLUMN starting from record ORIGIN applying FN to each record."
  (when (linkedp column)
    (loop :for entry = (point origin column) :then (next entry)
          :while entry
          :unless (funcall skip entry)
          :collect (funcall fn entry))))

(defun walk-left (column)
  "Return records starting from column across all the other columns, going left."
  (declare (ignorable column))
  nil)

(defun walk-right (column)
  "Return records starting from column across all the other columns, going right."
  (declare (ignorable column))
  nil)

(defun get-offset (record column &key (origin #'column-start))
  "Return the 0-indexed offset of ENTRY in COLUMN."
  (when (linkedp column)
    (loop :for rec = (point origin column) :then (next rec)
          :for count = 0 :then (1+ count)
          :when (eql record rec)
          :return count)))

(defun nth-record (n column &key (origin #'column-start))
  "Return the 0-indexed Nth record in COLUMN."
  (when (linkedp column)
    (loop :for rec = (point origin column) :then (next rec)
          :for count = 0 :then (1+ count)
          :when (= count n)
          :return rec)))

(defmethod print-object ((m match) stream)
  (print-unreadable-object (m stream :type t)
    (with-slots (column) m
      (format stream "~A" (cid column)))))

(defun make-match (record column offset)
  "Return a MATCH object."
  (make-instance 'match :record record :column column :offset offset))

(defun everyp (item entry &key (test *field-test*) (selectors *default-selectors*))
  "Return true if ITEM matches to the applied version of ENTRY."
  (every test item (apply-selectors entry selectors)))

(defgeneric find-matching-records (query store &key &allow-other-keys)
  (:documentation "Return all the records from STORE that matches QUERY."))
(defmethod find-matching-records ((query list) (c column)
                                  &key (origin #'column-start)
                                       (test *field-test*)
                                       (selectors *default-selectors*))
  (when (linkedp c)
    (loop :for record :in (walk-down c :origin origin)
          :for offset = 0 :then (1+ offset)
          :when (everyp query record :test test :selectors selectors)
          :collect (make-match record c offset))))
(defmethod find-matching-records ((query entry) (c column)
                                  &key (origin #'column-start)
                                       (test *field-test*)
                                       (selectors *default-selectors*))
  (find-matching-records (value query) c :origin origin :test test :selectors selectors))
(defmethod find-matching-records ((query list) (r registry)
                                  &key (origin #'column-start)
                                       (test *field-test*)
                                       (selectors *default-selectors*))
  (let ((columns (find-columns r)))
    (loop :for column :in columns
          :nconc (find-matching-records query column :origin origin :test test :selectors selectors))))
(defmethod find-matching-records ((query entry) (r registry)
                                  &key (origin #'column-start)
                                       (test *field-test*)
                                       (selectors *default-selectors*))
  (find-matching-records (value query) r :origin origin :test test :selectors selectors))

(defgeneric find-matching-record (query store &key &allow-other-keys)
  (:documentation "Return the first record from STORE that matches QUERY."))
(defmethod find-matching-record ((query list) (c column)
                                 &key (origin #'column-start)
                                      (test *field-test*)
                                      (selectors *default-selectors*))
  (when (linkedp c)
    (loop :for record :in (walk-down c :origin origin)
          :for offset = 0 :then (1+ offset)
          :when (everyp query record :test test :selectors selectors)
          :return (make-match record c offset))))
(defmethod find-matching-record ((query entry) (c column)
                                 &key (origin #'column-start)
                                      (test *field-test*)
                                      (selectors *default-selectors*))
  (find-matching-record (value query) c :origin origin :test test :selectors selectors))

(defmethod value ((m match))
  "Return contents of MATCH object."
  (value (record m)))

(defun find-other-columns (column registry)
  "Find the other columns in REGISTRY."
  (let ((cid (cid column)))
    (find-columns registry :skip #'(lambda (c) (= cid (cid c))))))

;;; Note: if multiple matches are found, find a way to disambiguate them
;;; Note: create snaking and non-snaking bindings?
(defun bind-all-matches (record column registry tcolumn tregistry)
  "Bind all matching records."
  (declare (ignorable record column registry tcolumn tregistry))
  nil)

(defun bind-first-matches (record column registry tcolumn tregistry)
  "Bind all first matching records."
  (declare (ignorable record column registry tcolumn tregistry))
  nil)
