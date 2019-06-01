;;; registry.lisp

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
               (with-slots (cid id prev curr next) v
                 (format t "~S => ~S~%" k (list cid id prev curr next))))
           (table r))
  (format t "~%** COLUMNS~%")
  (maphash #'(lambda (k v)
               (with-slots (rid cid cstart cend cleft cright) v
                 (format t "~S => ~S~%" k
                         (list rid cid cstart cend cleft cright))))
           (ctable r)))

(defun dump-world ()
  "Dump the contents of the world."
  (let ((registries (loop :for v :being :the :hash-values :in (rtable *world*)
                          :collect v)))
    (loop :for registry :in registries
          :do (dump-registry registry))
    (format t "~%")
    (dump-registries)
    (values)))

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

(defmethod print-object ((e entry) stream)
  (print-unreadable-object (e stream :type t)
    (with-slots (cid id curr) e
      (format stream "CID:~A ID:~A CURR:~S" cid id curr))))

(defmethod print-object ((c column) stream)
  (print-unreadable-object (c stream :type t)
    (with-slots (rid cid cname) c
      (format stream "RID:~A CID:~A CNAME:~A" rid cid cname))))

(defmethod print-object ((r registry) stream)
  (print-unreadable-object (r stream :type t)
    (with-slots (rid rname) r
      (format stream "RID:~A RNAME:~A" rid rname))))

(defun make-entry (cid id prev curr next)
  "Create an instance of the entry class."
  (make-instance 'entry :cid cid :id id :prev prev :curr curr :next next))

(defun make-column (rid cid cname cstart cend &optional (cleft -1) (cright -1))
  "Create an instance of the column class."
  (make-instance 'column :rid rid :cid cid :cname cname
                         :cstart cstart :cend cend :cleft cleft :cright cright))

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

(defun import-feed (feed name registry)
  "Import items from FEED to REGISTRY, creating column and entry objects and updating the registry."
  (let* ((pillar (pad-feed feed))
         (length (length feed))
         (start (1+ (counter registry)))
         (end (1- (+ start length)))
         (cname (if (mof:empty-string-p name) (string (gensym "COLUMN")) name))
         (column (make-column (rid registry) (spawn-ccounter registry) cname start end)))
    (add-column column registry)
    (loop :for prev :in pillar
          :for curr :in (rest pillar)
          :for next :in (rest (rest pillar))
          :do (add-entry (make-entry (cid column) (spawn-counter registry) prev curr next)
                         registry))
    registry))

(defgeneric find-registry (query)
  (:documentation "Return the registry which matches QUERY in WORLD."))
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

(defun find-registries ()
  "Return all registries from the world."
  (loop :for r :being :the :hash-values :in (rtable *world*) :collect r))

(defun dump-registries ()
  "Display inforamtion about the registries."
  (format t "~&** REGISTRIES~%")
  (maphash #'(lambda (k v)
               (with-slots (rid rname counter ccounter) v
                 (format t "~S => ~S~%" k (list rid rname ccounter counter))))
           (rtable *world*)))

(defgeneric find-column (query registry)
  (:documentation "Return a column which matches QUERY in REGISTRY."))
(defmethod find-column ((query integer) (r registry))
  (loop :for cid :being :the :hash-keys :in (ctable r)
        :for column = (gethash cid (ctable r))
        :when (= query (cid column))
        :return column))
(defmethod find-column ((query string) (r registry))
  (loop :for cid :being :the :hash-keys :in (ctable r)
        :for column = (gethash cid (ctable r))
        :when (string-equal query (cname column))
        :return column))

(defgeneric find-columns (registry)
  (:documentation "Return all columns from REGISTRY."))
(defmethod find-columns ((r registry))
  (loop :for c :being :the :hash-values :in (ctable r) :collect c))

(defgeneric find-entry (query registry)
  (:documentation "Return an entry which matches QUERY in COLUMN."))
(defmethod find-entry ((query integer) (r registry))
  (loop :for id :being :the :hash-keys :in (table r)
        :for entry = (gethash id (table r))
        :when (= query (id entry))
        :return entry))

(defgeneric find-entries (registry)
  (:documentation "Return all entries from REGISTRY."))
(defmethod find-entries ((r registry))
  (loop :for e :being :the :hash-values :in (table r) :collect e))

(defgeneric dump-column (column &key &allow-other-keys)
  (:documentation "Print information about a column."))
(defmethod dump-column ((c column) &key (complete nil))
  (with-slots (rid cid cstart cend cleft cright) c
    (when complete
      (format t "~&RID:~A, CID:~A, CSTART:~A, CEND:~A, CLEFT:~A, CRIGHT:~A~%"
              rid cid cstart cend cleft cright))
    (loop :for e :from cstart :to cend
          :for entry = (find-entry e (find-registry rid))
          :do (with-slots (cid id prev curr next) entry
                (if complete
                    (format t "~&CID:~A, ID:~A, PREV:~S, CURR:~S, NEXT:~S~%"
                            cid id prev curr next)
                    (format t "~&~S~%" curr))))
    (values)))

(defgeneric dump-entry (entry &key &allow-other-keys)
  (:documentation "Print information about an entry."))
(defmethod dump-entry ((e entry) &key (complete nil))
  (with-slots (cid id prev curr next) e
    (if complete
        (format t "~&CID: ~S~%ID: ~S~%PREV: ~S~%CURR: ~S~%NEXT: ~S~%"
                cid id prev curr next)
        (format t "~&PREV: ~S~%CURR: ~S~%NEXT: ~S~%"
                prev curr next))
    (values)))

(defun print-column (cname rname)
  "Display the contents of column CNAME in registry RNAME."
  (dump-column (find-column cname (find-registry rname))))

(defun print-entry (id rname)
  "Display the contents of entry ID in registry RNAME."
  (dump-entry (find-entry id (find-registry rname))))
