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

(defgeneric find-matches (entry column registry &key &allow-other-keys)
  (:documentation "Return a list of matching instances of ENTRY in target COLUMN using SELECTOR and TEST."))
(defmethod find-matches ((item list) (c column) (r registry)
                         &key (selectors *default-selectors*)
                           (test *field-test*))
  (loop :for entry :in (find-records c)
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

(defmethod initialize-instance :after ((u unit) &key registry)
  "Update unit U in REGISTRY."
  (let ((counter (spawn-ucounter registry)))
    (with-slots (id) u
      (setf id counter))))

(defun make-unit (cid registry &optional prev next)
  "Create an instance of the unit class."
  (make-instance 'unit :cid cid :prev prev :next next :registry registry))

;;; Note: update the links of the PREV and NEXT records, much like burying
;;; Note: PREV and NEXT specifies the location of the unit
;;; Note: if PREV is null, then the unit is at the start
;;; Note: if NEXT is null, then the unit is at the end
;;; Note: it should return the unit created
;;; Note: what is the minimum context needed to provide an unambiguous location?
(defun forge-unit (column registry &key prev next)
  "Create a unit under COLUMN in REGISTRY."
  (let* ((cid (cid column))
         (unit (make-unit cid registry prev next)))
    (add-record unit registry)
    (add-record unit column)))

;;; Note: is it possible to provide a range?
(defun forge-units (column registry count)
  "Create units in COLUMN en masse"
  (declare (ignorable column registry count))
  nil)
