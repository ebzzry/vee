;;;; propagation.lisp

(in-package #:muso/core)

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

(defun everyp (item entry registry &key (test *field-test*) (constraints *default-constraints*))
  "Return true if ITEM matches to the applied version of ENTRY."
  (every test
         (apply-constraints constraints item registry :use-header nil)
         (apply-constraints constraints entry registry)))

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

(defgeneric find-matching-volume-records (query volume &key &allow-other-keys)
  (:documentation "Return all the records from VOLUME that match QUERY."))
(defmethod find-matching-volume-records ((query entry) (v volume)
                                         &key (origin #'volume-start)
                                              (test *field-test*)
                                              (constraints *default-constraints*))
  (when (linkedp v)
    (let ((registry (find-registry (rid v))))
      (loop :for record :in (walk-down v :origin origin :skip #'unitp)
            :for offset = 0 :then (1+ offset)
            :when (everyp (value query) record registry :test test :constraints constraints)
            :collect (make-match record v offset)))))
(defmethod find-matching-volume-records ((query list) (v volume) &rest args)
  (apply #'find-matching-volume-records (value query) v args))

(defgeneric find-matching-registry-records (query registry &key &allow-other-keys)
  (:documentation "Return all the records from REGISTRY that match QUERY."))
(defmethod find-matching-registry-records ((query entry) (r registry)
                                           &key (origin #'volume-start)
                                                (test *field-test*)
                                                (constraints *default-constraints*)
                                                exclusive)
  (let* ((volume (find-volume (vid query) r))
         (volumes (if exclusive
                      (find-other-volumes volume r)
                      (find-volumes r))))
    (loop :for v :in volumes
          :nconc (find-matching-volume-records query v :origin origin :test test :constraints constraints))))

(defgeneric find-matching-record (query store &key &allow-other-keys)
  (:documentation "Return the first record from STORE that matches QUERY."))
(defmethod find-matching-record ((query list) (v volume) (r registry)
                                 &key (origin #'volume-start)
                                      (test *field-test*)
                                      (constraints *default-constraints*))
  (when (linkedp v)
    (loop :for record :in (walk-down v :origin origin)
          :for offset = 0 :then (1+ offset)
          :when (everyp query record r :test test :constraints constraints)
          :return (make-match record v offset))))
(defmethod find-matching-record ((query entry) (v volume) &rest args)
  (apply #'find-matching-record (value query) v args))

;;; Note: snaking bindings
;;; Note: find a way to disambiguate multiple matches
(defun bind-all-matches (record registry &rest args)
  "Bind all matching records."
  (let ((matches (apply #'find-matching-registry-records record registry args)))
    (when matches
      (setf (matches record) matches))))

;;; Note: non-snaking bindings
(defun bind-first-matches ()
  "Bind all first matching records."
  nil)

(defun bind-volume (volume registry &rest args)
  "Bind VOLUME to other volumes in the registry."
  (loop :for entry :in (walk-down volume :skip #'unitp)
        :do (apply #'bind-all-matches entry registry args)))

(defun bind-wall (registry)
  "Bind the wall in REGISTRY to the other volumes."
  (let ((wall (wall registry)))
    (bind-volume wall registry :exclusive t)))

(defun bind-volumes (registry &rest args)
  "Bind all the volumes in REGISTRY to one another."
  (let ((volumes (find-volumes registry)))
    (loop :for volume :in volumes :do (apply #'bind-volume volume registry args))))

(defun make-column (value)
  "Return a column object."
  (make-instance 'column :value value))

(defun extract-column (index volume)
  "Return a column object from VOLUME specified by 1-indexed INDEX."
  (flet ((fn (entry)
           (nth (1- index) (value entry))))
    (let* ((entries (walk-down volume :skip #'unitp))
           (value (mapcar #'fn entries)))
      (make-column value))))

(defun view-column (index volume)
  "Return a column from volume in a readable form."
  (let ((value (value (extract-column index volume))))
    (format t "~{~S~^ ~}" value)))
