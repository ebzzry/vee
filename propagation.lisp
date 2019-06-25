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

(defun point (origin volume)
  "Return starting point based on the type of origin."
  (etypecase origin
    (function (funcall origin volume))
    (entry origin)))

(defun volume-start (volume)
  "Return the first entry in VOLUME."
  (let ((records (find-records volume)))
    (loop :for record :in records
          :when (and (null (prev record))
                     (next record))
          :return record)))

(defun volume-end (volume)
  "Return the last entry in VOLUME."
  (let ((records (nreverse (find-records volume))))
    (loop :for record :in records
          :when (and (prev record)
                     (null (next record)))
          :return record)))

(defmethod initialize-instance :after ((u unit) &key registry)
  "Update unit U in REGISTRY."
  (let ((counter (spawn-ucounter registry)))
    (with-slots (id) u
      (setf id counter))))

(defun make-unit (vid registry)
  "Create an instance of the unit class."
  (make-instance 'unit :vid vid :registry registry))

(defun forge-unit (volume registry)
  "Create a unit under VOLUME within REGISTRY."
  (let* ((vid (vid volume))
         (unit (make-unit vid registry)))
    (add-record unit registry)
    (add-record unit volume)
    unit))

(defun link-unit-before (record volume registry)
  "Link a UNIT before RECORD in VOLUME within REGISTRY."
  (let ((unit (forge-unit volume registry)))
    (cond ((volume-start-p record)
           (progn (setf (next unit) record)
                  (setf (prev record) unit)))
          (t (progn (setf (prev unit) (prev record))
                    (setf (next unit) record)
                    (setf (next (prev record)) unit)
                    (setf (prev record) unit))))
    unit))

(defun link-unit-after (record volume registry)
  "Link a UNIT after RECORD in VOLUME within REGISTRY."
  (let ((unit (forge-unit volume registry)))
    (cond ((volume-end-p record)
           (progn (setf (prev unit) record)
                  (setf (next record) unit)))
          (t (progn (setf (prev unit) record)
                    (setf (next unit) (next record))
                    (setf (prev (next record)) unit)
                    (setf (next record) unit))))
    unit))

(defun link-unit (record volume registry &key (position :after))
  "Link a unit POSITION RECORD in VOLUME within REGISTRY."
  (when (keywordp position)
    (let ((fn (intern (mof:cat "LINK-UNIT-" (string position)))))
      (funcall fn record volume registry))))

(defun link-units (record volume registry count &key (position :after))
  "Link COUNT units POSITION RECORD in VOLUME within REGISTRY."
  (flet ((link (rec vol reg pos)
           (link-unit rec vol reg :position pos)))
    (loop :for count :from 1 :to count
          :for unit = (link record volume registry position)
          :then (link unit volume registry position)
          :finally (return volume))))

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
  "Return records starting from volume across all the other volumes, going left."
  (declare (ignorable volume))
  nil)

(defun walk-right (volume)
  "Return records starting from volume across all the other volumes, going right."
  (declare (ignorable volume))
  nil)

(defun get-offset (record volume &key (origin #'volume-start))
  "Return the 0-indexed offset of ENTRY in VOLUME."
  (when (linkedp volume)
    (loop :for rec = (point origin volume) :then (next rec)
          :for count = 0 :then (1+ count)
          :when (eql record rec)
          :return count)))

(defun nth-record (n volume &key (origin #'volume-start))
  "Return the 0-indexed Nth record in VOLUME."
  (when (linkedp volume)
    (loop :for rec = (point origin volume) :then (next rec)
          :for count = 0 :then (1+ count)
          :when (= count n)
          :return rec)))

(defmethod print-object ((m match) stream)
  (print-unreadable-object (m stream :type t)
    (with-slots (volume) m
      (format stream "~A" (vid volume)))))

(defun make-match (record volume offset)
  "Return a MATCH object."
  (make-instance 'match :record record :volume volume :offset offset))

(defun everyp (item entry &key (test *field-test*) (selectors *default-selectors*))
  "Return true if ITEM matches to the applied version of ENTRY."
  (every test item (apply-selectors entry selectors)))

(defgeneric find-matching-records (query store &key &allow-other-keys)
  (:documentation "Return all the records from STORE that matches QUERY."))
(defmethod find-matching-records ((query list) (v volume)
                                  &key (origin #'volume-start)
                                       (test *field-test*)
                                       (selectors *default-selectors*))
  (when (linkedp v)
    (loop :for record :in (walk-down v :origin origin :skip #'unitp)
          :for offset = 0 :then (1+ offset)
          :when (everyp query record :test test :selectors selectors)
          :collect (make-match record v offset))))
(defmethod find-matching-records ((query entry) (v volume)
                                  &key (origin #'volume-start)
                                       (test *field-test*)
                                       (selectors *default-selectors*))
  (find-matching-records (value query) v :origin origin :test test :selectors selectors))
(defmethod find-matching-records ((query list) (r registry)
                                  &key (origin #'volume-start)
                                       (test *field-test*)
                                       (selectors *default-selectors*))
  (let ((volumes (find-volumes r)))
    (loop :for volume :in volumes
          :nconc (find-matching-records query volume
                                        :origin origin :test test :selectors selectors))))
(defmethod find-matching-records ((query entry) (r registry)
                                  &key (origin #'volume-start)
                                       (test *field-test*)
                                       (selectors *default-selectors*))
  (find-matching-records (value query) r :origin origin :test test :selectors selectors))

(defgeneric find-matching-record (query store &key &allow-other-keys)
  (:documentation "Return the first record from STORE that matches QUERY."))
(defmethod find-matching-record ((query list) (v volume)
                                 &key (origin #'volume-start)
                                      (test *field-test*)
                                      (selectors *default-selectors*))
  (when (linkedp v)
    (loop :for record :in (walk-down v :origin origin)
          :for offset = 0 :then (1+ offset)
          :when (everyp query record :test test :selectors selectors)
          :return (make-match record v offset))))
(defmethod find-matching-record ((query entry) (v volume)
                                 &key (origin #'volume-start)
                                      (test *field-test*)
                                      (selectors *default-selectors*))
  (find-matching-record (value query) v :origin origin :test test :selectors selectors))

(defmethod value ((m match))
  "Return record value from M."
  (value (record m)))

(defmethod id ((m match))
  "Retturn record id from M."
  (id (record m)))

(defun find-other-volumes (volume registry)
  "Find the other volumes in REGISTRY."
  (let ((vid (vid volume)))
    (find-volumes registry :skip #'(lambda (v) (= vid (vid v))))))

(defgeneric find-exclusive-matching-records (query volume registry &key &allow-other-keys)
  (:documentation "Find all matching records of QUERY in all VOLUMES of REGISTRY except the volume of QUERY."))
(defmethod find-exclusive-matching-records ((query entry) (v volume) (r registry)
                                            &key (origin #'volume-start)
                                                 (test *field-test*)
                                                 (selectors *default-selectors*))
  (when (linkedp v)
    (let ((volumes (find-other-volumes v r)))
      (loop :for volume :in volumes
            :nconc (find-matching-records (value query) volume :origin origin :test test :selectors selectors)))))
(defmethod find-exclusive-matching-records ((query list) (v volume) (r registry)
                                            &key (origin #'volume-start)
                                                 (test *field-test*)
                                                 (selectors *default-selectors*))
  (when (linkedp v)
    (let ((volumes (find-other-volumes v r)))
      (loop :for volume :in volumes
            :nconc (find-matching-records query volume :origin origin :test test :selectors selectors)))))

(defgeneric find-exclusive-matching-first-records (query volume registry &key &allow-other-keys)
  (:documentation "Find all matching first records of QUERY in all VOLUMES of REGISTRY except the volume of QUERY."))
(defmethod find-exclusive-matching-first-records ((query entry) (v volume) (r registry)
                                                  &key (origin #'volume-start)
                                                       (test *field-test*)
                                                       (selectors *default-selectors*))
  (when (linkedp v)
    (let ((volumes (find-other-volumes v r)))
      (loop :for volume :in volumes
            :collect (find-matching-record (value query) volume :origin origin :test test :selectors selectors)))))
(defmethod find-exclusive-matching-first-records ((query list) (v volume) (r registry)
                                                  &key (origin #'volume-start)
                                                       (test *field-test*)
                                                       (selectors *default-selectors*))
  (when (linkedp v)
    (let ((volumes (find-other-volumes v r)))
      (loop :for volume :in volumes
            :collect (find-matching-record query volume :origin origin :test test :selectors selectors)))))

;;; Note: snaking bindings
;;; Note: find a way to disambiguate multiple matches
(defun bind-all-matches (record volume registry &key exclusive)
  "Bind all matching records."
  (let ((matches (if exclusive
                     (find-exclusive-matching-records record volume registry)
                     (find-matching-records record volume registry))))
    (when matches
      (setf (matches record) matches))))

;;; Note: non-snaking bindings
(defun bind-first-matches (record volume registry &key exclusive)
  "Bind all first matching records."
  (let ((matches (if exclusive
                     (find-exclusive-matching-first-records record volume registry)
                     (find-matching-first-records record volume registry))))
    (when matches
      (setf (matches record) matches))))

(defun bind-volume (volume registry
                    &key (origin #'volume-start)
                         (test *field-test*)
                         (selectors *default-selectors*)
                         exclusive)
  "Bind volume to other volumes in the registry."
  (loop :for entry :in (walk-down volume :origin origin :skip #'unitp)
        :do (bind-all-matches entry volume registry :exclusive exclusive)))

(defun bind-wall (registry)
  "Bind the wall in REGISTRY to the other volumes."
  (let ((wall (wall registry)))
    (bind-volume wall)))
