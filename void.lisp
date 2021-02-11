;;;; void.lisp

(in-package #:veda/core)

(defun void-name (template)
  "Return the void name of TEMPLATE."
  (let* ((name (name template))
         (vname (cat "%" name)))
    vname))

(defun void-create (template)
  "Create a VOID from TEMPLATE."
  (let* ((vname (void-name template))
         (void (spawn-registry vname)))
    (setf (xid template) (rid void))
    void))

(defgeneric void-send (frame void)
  (:documentation "Add FRAME to VOID")
  (:method ((p pool) void)
    (with-slots (prev next cells)
        p
      (let ((frame (make-instance 'pool :prev prev :next next :cells cells :registry void)))
        (add-object frame void))))
  (:method ((u unit) void)
    (with-slots (prev next)
        u
      (let ((frame (make-instance 'unit :prev prev :next next :registry void)))
        (add-object frame void)))))

(defun void-get (id void)
  "Return an item by ID from VOID"
  (gethash id (etable void)))

(defun bury (frame)
  "Remove the linking of a frame, but keep it stored."
  (when (and (null (buriedp frame))
             (or (prev frame)
                 (next frame)))
    (cond ((volume-start-p frame)
           (setf (prev (next frame)) nil))
          ((volume-end-p frame)
           (setf (next (prev frame)) nil))
          (t (progn (setf (next (prev frame))
                          (next frame))
                    (setf (prev (next frame))
                          (prev frame)))))
    (setf (buriedp frame) t))
  (values))

(defun unbury (frame)
  "Make a frame appear again."
  (when (and (buriedp frame)
             (or (prev frame)
                 (next frame)))
    (cond ((volume-start-p frame)
           (setf (prev (next frame)) frame))
          ((volume-end-p frame)
           (setf (next (prev frame)) frame))
          (t (progn (setf (next (prev frame))
                          frame)
                    (setf (prev (next frame))
                          frame))))
    (setf (buriedp frame) nil))
  (values))

(defun find-buried (registry)
  "Show the buriedp pools in REGISTRY."
  (loop :for k :being :the :hash-keys :in (etable registry)
        :for frame = (find-frame k registry)
        :when (buriedp frame)
        :collect frame))

(defun unlink (frame)
  "Remove the linkage of POOL in its surrounding context."
  (setf (prev frame) nil)
  (setf (next frame) nil))

(defgeneric blank (frame)
  (:documentation "Set a blank CELLS value to FRAME, but keep it linked.")
  (:method blank ((p pool))
    (setf (cells p) nil)))

(defun deregister (frame registry)
  "Remove FRAME from REGISTRY."
  (let* ((vid (vid frame))
         (volume (find-volume vid registry)))
    (delete-object frame registry)
    (delete-object frame volume)
    (values)))

(defun banish (frame registry)
  "Completely remove FRAME from REGISTRY."
  (let* ((id (id frame)))
    (multiple-value-bind (value present)
        (gethash id (etable registry))
      (declare (ignore value))
      (when present
        (let ((void (void-create registry)))
          (bury frame)
          (void-send frame void)
          (deregister frame registry)
          void)))))

(defun find-gaps (volume registry)
  "Show where the gaps are in a registry."
  (declare (ignorable volume registry))
  nil)

(defun fill-gaps (volume registry)
  "Fill the gaps in a volume with information."
  (declare (ignorable volume registry))
  nil)
