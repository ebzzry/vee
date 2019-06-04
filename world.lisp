;;; registry.lisp

(in-package #:muso/core)

(defgeneric reset-ecounter (registry)
  (:documentation "Reset the COUNTER in REGISTRY."))
(defmethod reset-ecounter ((r registry))
  (setf (ecounter r) *initial-ecounter*)
  (values))

(defgeneric spawn-ecounter (registry)
  (:documentation "Generate a new COUNTER in REGISTRY."))
(defmethod spawn-ecounter ((r registry))
  (incf (ecounter r))
  (ecounter r))

(defgeneric reset-ccounter (registry)
  (:documentation "Reset the CCOUNTER in REGISTRY."))
(defmethod reset-ccounter ((r registry))
  (setf (ccounter r) *initial-ccounter*)
  (values))

(defgeneric spawn-ccounter (registry)
  (:documentation "Generate a new CCOUNTER in REGISTRY."))
(defmethod spawn-ccounter ((r registry))
  (incf (ccounter r))
  (ccounter r))

(defun reset-rcounter ()
  "Reset the RCOUNTER in WORLD."
  (setf (rcounter *world*) *initial-rcounter*)
  (values))

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
  (setf (ecounter r) *initial-ecounter*)
  (setf (etable r) (make-hash-table))
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
           (etable r))
  (format t "~%** COLUMNS~%")
  (maphash #'(lambda (k v)
               (with-slots (rid cid cname cstart cend clength cleft cright) v
                 (format t "~S => ~S~%" k
                         (list rid cid cname cstart cend clength cleft cright))))
           (ctable r)))

(defun dump-world ()
  "Dump the contents of the world."
  (let ((registries (loop :for v :being :the :hash-values :in (rtable *world*)
                          :collect v)))
    (loop :for registry :in registries
          :do (dump-registry registry))
    (when registries
      (format t "~%")
      (dump-registries))
    (values)))

(defgeneric add-record (entry registry)
  (:documentation "Add a record to REGISTRY."))
(defmethod add-record ((e entry) (r registry))
  (setf (gethash (ecounter r) (etable r)) e)
  e)
(defmethod add-record ((c column) (r registry))
  (setf (gethash (ccounter r) (ctable r)) c)
  c)

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

(defun make-entry (cid id &optional prev curr next)
  "Create an instance of the entry class."
  (make-instance 'entry :cid cid :id id :prev prev :curr curr :next next))

(defmethod initialize-instance :after ((c column) &key)
  "Set the CLENGTH slot for C after instantiating."
  (let ((start (cstart c))
        (end (cend c)))
    (setf (clength c) (1+ (- end start)))))

(defun make-column (rid cid cname cstart cend &optional (cleft -1) (cright -1))
  "Create an instance of the column class."
  (make-instance 'column :rid rid :cid cid :cname (string-upcase cname)
                         :cstart cstart :cend cend
                         :cleft cleft :cright cright))

(defun make-registry (&optional (rname (genstring "REGISTRY")))
  "Create an instance of the registry class."
  (make-instance 'registry :rid (spawn-rcounter) :rname (string-upcase rname)))

(defgeneric spawn-registry (query)
  (:documentation "Return a new registry object then add it to the world, or return an existing one"))
(defmethod spawn-registry ((query string))
  (let ((registry (find-registry query)))
    (if (not registry)
        (let ((r (make-registry query)))
          (add-registry r))
        registry)))
(defmethod spawn-registry ((query integer))
  (let ((registry (find-registry query)))
    (if (not registry)
        (let ((r (make-registry)))
          (add-registry r))
        registry)))

(defun make-world ()
  "Create an instance of the world class."
  (make-instance 'world))

(defun initialize-world ()
  "Initialize the world."
  (setf *world* (make-world)))
(mof:defalias boot-world initialize-world)

(defun import-feed (feed name registry)
  "Import items from FEED to REGISTRY with name NAME."
  (let* ((pillar (pad-feed feed))
         (length (length feed))
         (start (1+ (ecounter registry)))
         (offset (1- (+ start length)))
         (cname (if (mof:empty-string-p name) (genstring "COLUMN") name))
         (column (make-column (rid registry) (spawn-ccounter registry) cname start offset)))
    (add-record column registry)
    (loop :for prev :in pillar
          :for curr :in (rest pillar)
          :for next :in (rest (rest pillar))
          :for entry = (make-entry (cid column) (spawn-ecounter registry) prev curr next)
          :do (add-record entry registry))
    registry))

(defgeneric find-registry (query)
  (:documentation "Return the registry which matches QUERY in WORLD."))
(defmethod find-registry ((query integer))
  (loop :for rid :being :the :hash-keys :in (rtable *world*)
        :for registry = (gethash rid (rtable *world*))
        :when (= query (rid registry))
        :return registry))
(defmethod find-registry ((query string))
  (loop :for rid :being :the :hash-keys :in (rtable *world*)
        :for registry = (gethash rid (rtable *world*))
        :when (string-equal (string-upcase query) (rname registry))
        :return registry))

(defun find-registries ()
  "Return all registries from the world."
  (loop :for r :being :the :hash-values :in (rtable *world*) :collect r))

(defun dump-registries ()
  "Display inforamtion about the registries."
  (format t "~&** REGISTRIES~%")
  (maphash #'(lambda (k v)
               (with-slots (rid rname ecounter ccounter) v
                 (format t "~S => ~S~%" k (list rid rname ccounter ecounter))))
           (rtable *world*)))

(defgeneric find-column (query registry)
  (:documentation "Return a column which matches QUERY in REGISTRY."))
(defmethod find-column ((query integer) (r registry))
  (when r
    (loop :for cid :being :the :hash-keys :in (ctable r)
          :for column = (gethash cid (ctable r))
          :when (= query (cid column))
          :return column)))
(defmethod find-column ((query string) (r registry))
  (when r
    (loop :for cid :being :the :hash-keys :in (ctable r)
          :for column = (gethash cid (ctable r))
          :when (string-equal (string-upcase query) (cname column))
          :return column)))

(defgeneric find-columns (registry)
  (:documentation "Return all columns from REGISTRY."))
(defmethod find-columns ((r registry))
  (loop :for c :being :the :hash-values :in (ctable r) :collect c))

(defgeneric find-entry (query registry)
  (:documentation "Return an entry which matches QUERY in COLUMN."))
(defmethod find-entry ((query integer) (r registry))
  (loop :for id :being :the :hash-keys :in (etable r)
        :for entry = (gethash id (etable r))
        :when (= query (id entry))
        :return entry))

(defgeneric find-entries (registry)
  (:documentation "Return all entries from REGISTRY."))
(defmethod find-entries ((r registry))
  (loop :for e :being :the :hash-values :in (etable r) :collect e))

(defgeneric dump-column (column &key &allow-other-keys)
  (:documentation "Print information about a column."))
(defmethod dump-column ((c column) &key (complete nil))
  (with-slots (rid cid cstart cend clength cleft cright) c
    (when complete
      (format t "~&RID:~A, CID:~A, CSTART:~A, CEND:~A, CLENGTH:~A, CLEFT:~A, CRIGHT:~A~%"
              rid cid cstart cend clength cleft cright))
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

(defun display-column (cname rname)
  "Display the contents of column CNAME in registry RNAME."
  (dump-column (find-column cname (find-registry rname))))
(defun display-entry (id rname)
  "Display the contents of entry ID in registry RNAME."
  (dump-entry (find-entry id (find-registry rname))))

(defun locate-column (cname rname)
  "Locate the column CNAME in registry RNAME."
  (find-column cname (find-registry rname)))
(defun locate-entry (id rname)
  "Locate the entry ID in registry RNAME."
  (find-entry id (find-registry rname)))

(defun max-column (registry)
  "Return the biggest column in REGISTRY."
  (first (sort (find-columns registry) #'> :key #'clength)))
(mof:defalias wall max-column)

(defun mktemp-registry ()
  "Return an empty registry."
  (make-registry (genstring "REGISTRY")))

(defun shallow-copy-registry (registry name)
  "Create a shallow copy of REGISTRY."
  (when (find-registry name)
    (error "Target registry name already exists."))
  nil)

(defun wall-copy-registry (template registry)
  "Create blank columns from TEMPLATE to REGISTRY with uniform lengths from the wall in TEMPLATE."
  (assert (null (find-registry registry)))
  (let* ((wall (wall template))
         (length (clength wall)))
    (loop :for count :from 1 :to (length (find-columns template))
          :for start = (1+ (ecounter registry))
          :for offset = (+ start length)
          :for cname = (genstring "COLUMN")
          :for column = (add-record (make-column (rid registry) (spawn-ccounter registry)
                                                 cname start offset)
                                    registry)
          :do (loop :for cid :from (cstart column) :to (cend column)
                    :for entry = (make-entry (cid column) (spawn-ecounter registry))
                    :do (add-record entry registry)))
    registry))
