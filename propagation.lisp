;;;; propagation.lisp

;;; Notes
;;;
;;; - Define new class CLUSTER?

(defun compute-offset (column entry)
  "Return the offset in the wall for the matching entry."
  (- (cstar column) (id entry)))

;;; Note: handle out-of-bounds error here
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

(defun find-matching-entries (entry column registry
                              &key (selectors *default-selectors*)
                                   (test *field-test*))
  "Return the IDs of matching instances of ENTRY in target COLUMN using SELECTOR."
  ;; Note: entry is source, column is destination
  (let ((entry-selected (apply-selectors entry selectors)))
    (loop :for cid :from (cstart column) :to (cend column)
          :for target = (find-entry cid registry)
          :for target-selected = (apply-selectors target selectors)
          :when (every test entry-selected target-selected)
          :collect target)))
