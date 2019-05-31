;;;; registry.lisp

(in-package #:muso/core)

(defun make-registry ()
  "Create an instance of the registry class."
  (make-instance 'registry))

(defgeneric reset-counter (registry)
  (:documentation "Reset the counter found in REGISTRY."))
(defmethod reset-counter ((r registry))
  (setf (counter r) *initial-counter*))

(defgeneric gen-counter (registry)
  (:documentation "Generate a new counter value from REGISTRY."))
(defmethod gen-counter ((r registry))
  (incf (counter r))
  (counter r))

(defgeneric reset-ccounter (registry)
  (:documentation "Reset the ccounter found in REGISTRY."))
(defmethod reset-ccounter ((r registry))
  (setf (ccounter r) *initial-ccounter*))

(defgeneric gen-ccounter (registry)
  (:documentation "Generate a new ccounter value from REGISTRY."))
(defmethod gen-ccounter ((r registry))
  (incf (ccounter r))
  (ccounter r))

(defgeneric add-entry (entry registry)
  (:documentation "Add entry to registry"))
(defmethod add-entry ((e entry) (r registry))
  (setf (gethash (counter r) (table r)) e)
  (values))

(defgeneric add-column (column registry)
  (:documentation "Add column to registry"))
(defmethod add-column ((c column) (r registry))
  (setf (gethash (ccounter r) (ctable r)) c)
  (values))

(defun pad-feed (feed &optional (pad ""))
  "Add starting and ending padding for column based on the first element."
  (let* ((initial (elt0 feed))
         (length (length initial))
         (pad (make-list length :initial-element pad)))
    (append (list pad) feed (list pad))))

(defun initialize-registry ()
  "Initialize the global registry."
  (setf *registry* (make-registry)))

(defgeneric reset-registry (registry)
  (:documentation "Reset the contents of registry."))
(defmethod reset-registry ((r registry))
  (setf (counter r) *initial-counter*)
  (setf (table r) (make-hash-table))
  (setf (ccounter r) *initial-ccounter*)
  (setf (ctable r) (make-hash-table))
  (values))

(defgeneric dump-registry (registry)
  (:documentation "Dump the contents of table from REGISTRY."))
(defmethod dump-registry ((r registry))
  (maphash #'(lambda (k v)
               (format t "~S => ~S~%" k (list (id v) (prev v) (curr v) (next v) (cid v))))
           (table r))
  (maphash #'(lambda (k v)
               (format t "~S => ~S~%" k v))
           (ctable r)))

(defun make-entry (id prev curr next &optional (cid nil))
  "Create an instance of the entry class."
  (make-instance 'entry :id id :prev prev :curr curr :next next :cid cid))

(defun make-column (cid cstart cend &optional (cleft -1) (cright -1))
  "Create an instance of the column class."
  (make-instance 'column :cid cid :cstart cstart :cend cend :cleft cleft :cright :cright))

(defun add-entries (feed registry)
  "Add items from FEED to REGISTRY, creating column and entry objects and updating the registry.r"
  (let* ((pillar (pad-feed feed))
         (length (length feed))
         (start (1+ (counter registry)))
         (end (+ start length))
         (column (make-column (gen-ccounter registry) start end)))
    (add-column column registry)
    (loop :for prev :in pillar
          :for curr :in (rest pillar)
          :for next :in (rest (rest pillar))
          :do (add-entry (make-entry (gen-counter registry) prev curr next (cid column))
                         registry))
    (values)))
