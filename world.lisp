;;;; registry.lisp

(in-package #:muso/core)

(defgeneric reset-counter (registry)
  (:documentation "Reset the COUNTER in REGISTRY."))
(defmethod reset-counter ((r registry))
  (setf (counter r) *initial-counter*))

(defgeneric spawn-counter (registry)
  (:documentation "Generate a new COUNTER in REGISTRY."))
(defmethod spawn-counter ((r registry))
  (incf (counter r))
  (counter r))

(defgeneric reset-ccounter (registry)
  (:documentation "Reset the CCOUNTER in REGISTRY."))
(defmethod reset-ccounter ((r registry))
  (setf (ccounter r) *initial-ccounter*))

(defgeneric spawn-ccounter (registry)
  (:documentation "Generate a new CCOUNTER in REGISTRY."))
(defmethod spawn-ccounter ((r registry))
  (incf (ccounter r))
  (ccounter r))

(defun reset-rcounter ()
  "Reset the RCOUNTER in WORLD."
  (setf (rcounter *world*) *initial-rcounter*))

(defun spawn-rcounter ()
  "Generate a new RCOUNTER in WORLD."
  (incf (rcounter *world*))
  (rcounter *world*))

(defun reset-world ()
  "Reset the world."
  (setf (rcounter *world*) *initial-rcounter*)
  (setf (rtable *world*) (make-hash-table))
  (values))

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

(defun dump-world ()
  "Dump the contents of the world."
  (let ((registries (loop :for v :being :the :hash-values :in (rtable *world*)
                          :collect v)))
    (loop :for registry :in registries
          :do (dump-registry registry))))

(defgeneric add-entry (entry registry)
  (:documentation "Add ENTRY to REGISTRY."))
(defmethod add-entry ((e entry) (r registry))
  (setf (gethash (counter r) (table r)) e)
  (values))

(defgeneric add-column (column registry)
  (:documentation "Add COLUMN to REGISTRY."))
(defmethod add-column ((c column) (r registry))
  (setf (gethash (ccounter r) (ctable r)) c)
  (values))

(defgeneric add-registry (registry)
  (:documentation "Add REGISTRY to WORLD."))
(defmethod add-registry ((r registry))
  (setf (gethash (rcounter *world*) (rtable *world*)) r)
  r)

(defmethod print-object ((r registry) stream)
  (print-unreadable-object (r stream :type t)
    (with-slots (rid rname)
        r
      (format stream "RID:~A RNAME:~A" rid rname))))

(defun make-entry (cid id prev curr next)
  "Create an instance of the entry class."
  (make-instance 'entry :cid cid :id id :prev prev :curr curr :next next))

(defun make-column (cid cstart cend &optional (cleft -1) (cright -1))
  "Create an instance of the column class."
  (make-instance 'column :cid cid :cstart cstart :cend cend :cleft cleft :cright cright))

(defun make-registry (rid rname)
  "Create an instance of the registry class."
  (make-instance 'registry :rid rid :rname rname))

(defun spawn-registry (name)
  "Generate a new registry object with name NAME then add it to WORLD."
  (let ((registry (make-registry (spawn-rcounter) name)))
    (unless (find-registry name)
      (add-registry registry))))

(defun make-world ()
  "Create an instance of the world class."
  (make-instance 'world))

(defun initialize-world ()
  "Initialize the world."
  (setf *world* (make-world)))
(mof:defalias boot initialize-world)

(defun import-feed (feed registry)
  "Import items from FEED to REGISTRY, creating column and entry objects and updating the registry."
  (let* ((pillar (pad-feed feed))
         (length (length feed))
         (start (1+ (counter registry)))
         (end (1- (+ start length)))
         (column (make-column (spawn-ccounter registry) start end)))
    (add-column column registry)
    (loop :for prev :in pillar
          :for curr :in (rest pillar)
          :for next :in (rest (rest pillar))
          :do (add-entry (make-entry (cid column) (spawn-counter registry) prev curr next)
                         registry))
    ;; Note: because a registry object is returned, chaining of calls is possible
    registry))

(defgeneric find-registry (query)
  (:documentation "Return the registry which matches NAME in WORLD."))
(defmethod find-registry ((query string))
  (loop :for rid :being :the :hash-keys :in (rtable *world*)
        :for registry = (gethash rid (rtable *world*))
        :when (string-equal query (rname registry))
        :return registry))
(defmethod find-registry ((query integer))
  (loop :for rid :being :the :hash-keys :in (rtable *world*)
        :for registry = (gethash rid (rtable *world*))
        :when (= query (rid registry))
        :return registry))
