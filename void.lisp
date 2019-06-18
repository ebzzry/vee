;;;; void.lisp

(in-package #:muso/core)

(defun void-name (template)
  "Return the void name of TEMPLATE."
  (let* ((rname (rname template))
         (vname (mof:cat "%" rname)))
    vname))

(defun void-create (template)
  "Create a VOID from TEMPLATE."
  (let* ((vname (void-name template))
         (void (spawn-registry vname)))
    (setf (vid template) (rid void))
    void))

(defun void-add (entry void)
  "Add an ENTRY to VOID."
  (with-slots (prev next value) entry
    (let ((e (make-instance 'entry :prev prev :next next :value value :registry void)))
      (add-record e void))))

(defun void-get (id void)
  "Return an item by ID from VOID"
  (gethash id (etable void)))

(defun bury (entry)
  "Remove the linking of an entry, but keep it."
  (when (and (null (buried entry))
             (or (prev entry)
                 (next entry)))
    (cond ((column-start-p entry)
           (setf (prev (next entry)) nil))
          ((column-end-p entry)
           (setf (next (prev entry)) nil))
          (t (progn (setf (next (prev entry))
                          (next entry))
                    (setf (prev (next entry))
                          (prev entry)))))
    (setf (buried entry) t))
  (values))

(defun unbury (entry)
  "Make an entry appear again."
  (when (and (buried entry)
             (or (prev entry)
                 (next entry)))
    (cond ((column-start-p entry)
           (setf (prev (next entry)) entry))
          ((column-end-p entry)
           (setf (next (prev entry)) entry))
          (t (progn (setf (next (prev entry))
                          entry)
                    (setf (prev (next entry))
                          entry))))
    (setf (buried entry) nil))
  (values))

(defun find-buried (registry)
  "Show the buried entries in REGISTRY."
  (loop :for k :being :the :hash-keys :in (etable registry)
        :for entry = (find-entry k registry)
        :when (buried entry)
        :collect entry))

(defun unlink (entry)
  "Remove the linkage of ENTRY in its surrounding context."
  (setf (prev entry) nil)
  (setf (next entry) nil))

(defun blank (entry)
  "Set a blank value to ENTRY, but keep it linked."
  (setf (value entry) nil))

(defun deregister (entry registry)
  "Remove ENTRY from REGISTRY."
  (let* ((id (id entry))
         (cid (cid entry))
         (column (find-column cid registry)))
    (remhash id (etable registry))
    (decf (ecounter registry))
    (setf (gapped column) t)))

(defun banish (entry registry)
  "Remove an entry from COLUMN within REGISTRY and adjust the pointers accordingly."
  (let* ((id (id entry))
         (val (gethash id (etable registry))))
    (when val
      (let ((void (void-create registry)))
        (bury entry)
        (void-add entry void)
        (deregister entry registry)
        void))))

(defun find-gaps (column registry)
  "Show where the gaps are in a registry."
  (declare (ignorable column registry))
  nil)

(defun fill-gaps (column registry)
  "Fill the gaps in a column with information."
  (declare (ignorable column registry))
  nil)
