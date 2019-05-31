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
  (:documentation "Dump the contents of the tables from REGISTRY."))
(defmethod dump-registry ((r registry))
  (format t "** ENTRIES~%")
  (maphash #'(lambda (k v)
               (format t "~S => ~S~%" k (list (cid v) (id v) (prev v) (curr v) (next v))))
           (table r))
  (format t "~%** COLUMNS~%")
  (maphash #'(lambda (k v)
               (format t "~S => ~S~%" k (list (cid v) (cstart v) (cend v) (cleft v) (cright v))))
           (ctable r)))

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

(defun make-entry (cid id prev curr next)
  "Create an instance of the entry class."
  (make-instance 'entry :cid cid :id id :prev prev :curr curr :next next))

(defun make-column (cid cstart cend &optional (cleft -1) (cright -1))
  "Create an instance of the column class."
  (make-instance 'column :cid cid :cstart cstart :cend cend :cleft cleft :cright cright))

(defun import-feed (feed registry)
  "Import items from FEED to REGISTRY, creating column and entry objects and updating the registry."
  (let* ((pillar (pad-feed feed))
         (length (length feed))
         (start (1+ (counter registry)))
         (end (1- (+ start length)))
         (column (make-column (gen-ccounter registry) start end)))
    (add-column column registry)
    (loop :for prev :in pillar
          :for curr :in (rest pillar)
          :for next :in (rest (rest pillar))
          :do (add-entry (make-entry (cid column) (gen-counter registry) prev curr next)
                         registry))
    (values)))
