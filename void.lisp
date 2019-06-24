;;;; void.lisp

(in-package #:muso/core)

(defun void-name (template)
  "Return the void name of TEMPLATE."
  (let* ((name (name template))
         (vname (mof:cat "%" name)))
    vname))

(defun void-create (template)
  "Create a VOID from TEMPLATE."
  (let* ((vname (void-name template))
         (void (spawn-registry vname)))
    (setf (xid template) (rid void))
    void))

(defgeneric void-send (record void)
  (:documentation "Add RECORD to VOID"))
(defmethod void-send ((e entry) void)
  (with-slots (prev next value) e
    (let ((record (make-instance 'entry :prev prev :next next :value value :registry void)))
      (add-record record void))))
(defmethod void-send ((u unit) void)
  (with-slots (prev next) u
    (let ((record (make-instance 'unit :prev prev :next next :registry void)))
      (add-record record void))))

(defun void-get (id void)
  "Return an item by ID from VOID"
  (gethash id (etable void)))

(defun bury (record)
  "Remove the linking of a record, but keep it stored."
  (when (and (null (buriedp record))
             (or (prev record)
                 (next record)))
    (cond ((volume-start-p record)
           (setf (prev (next record)) nil))
          ((volume-end-p record)
           (setf (next (prev record)) nil))
          (t (progn (setf (next (prev record))
                          (next record))
                    (setf (prev (next record))
                          (prev record)))))
    (setf (buriedp record) t))
  (values))

(defun unbury (record)
  "Make a record appear again."
  (when (and (buriedp record)
             (or (prev record)
                 (next record)))
    (cond ((volume-start-p record)
           (setf (prev (next record)) record))
          ((volume-end-p record)
           (setf (next (prev record)) record))
          (t (progn (setf (next (prev record))
                          record)
                    (setf (prev (next record))
                          record))))
    (setf (buriedp record) nil))
  (values))

(defun find-buried (registry)
  "Show the buriedp entries in REGISTRY."
  (loop :for k :being :the :hash-keys :in (etable registry)
        :for record = (find-record k registry)
        :when (buriedp record)
        :collect record))

(defun unlink (record)
  "Remove the linkage of ENTRY in its surrounding context."
  (setf (prev record) nil)
  (setf (next record) nil))

(defgeneric blank (record)
  (:documentation "Set a blank value to RECORD, but keep it linked."))
(defmethod blank ((e entry))
  (setf (value e) nil))

(defun deregister (record registry)
  "Remove RECORD from REGISTRY."
  (let* ((vid (vid record))
         (volume (find-volume vid registry)))
    (delete-record record registry)
    (delete-record record volume)
    (values)))

(defun banish (record registry)
  "Completely remove RECORD from REGISTRY."
  (let* ((id (id record)))
    (multiple-value-bind (value present)
        (gethash id (etable registry))
      (declare (ignore value))
      (when present
        (let ((void (void-create registry)))
          (bury record)
          (void-send record void)
          (deregister record registry)
          void)))))

(defun find-gaps (volume registry)
  "Show where the gaps are in a registry."
  (declare (ignorable volume registry))
  nil)

(defun fill-gaps (volume registry)
  "Fill the gaps in a volume with information."
  (declare (ignorable volume registry))
  nil)
