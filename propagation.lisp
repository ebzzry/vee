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
(defmethod find-matches ((item list) (column column) (registry registry)
                         &key (selectors *default-selectors*)
                              (test *field-test*))
  (loop :for entry :in (find-entries column)
        :for cluster = (apply-selectors entry selectors)
        :when (every test item cluster)
        :collect entry))
(defmethod find-matches ((entry entry) (column column) (registry registry)
                         &key (selectors *default-selectors*)
                              (test *field-test*))
  (find-matches (apply-selectors entry selectors)
                column registry :selectors selectors :test test))

;;; Note: no longer reliable because of the new table mechanism
(defun compute-offset (column entry)
  "Return the offset in the wall for the matching entry."
  (- (id entry) (id (first (find-entries column)))))

(defun offsets (query column registry
                &key (selectors *default-selectors*)
                     (test *field-test*))
  "Return the offsets of a query in COLUMN within REGISTRY."
  (mapcar #'(lambda (entry)
              (compute-offset column entry))
          (find-matches query column registry :selectors selectors :test test)))

(defun splice (location column registry count)
  "Create COUNT entries from LOCATION in COLUMN within REGISTRY."
  (declare (ignorable location column registry count))
  ;; If prev is null, ...
  ;; If next is null, ...

  ;; Handle the contiguousness of IDs
  nil)

(defun forge-block (column registry &optional prev next)
  "Create a block entry in registry."
  (forge-entry column registry prev next nil))
