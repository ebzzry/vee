;;;; world.lisp

(in-package #:muso/core)

(defmacro spawn-counter (registry accessor)
  "Generate a new counter in REGISTRY with ACCESSOR."
  `(progn (incf (,accessor ,registry))
          (,accessor ,registry)))
(defun spawn-ecounter (r) "See SPAWN-COUNTER." (spawn-counter r ecounter))
(defun spawn-vcounter (r) "See SPAWN-COUNTER." (spawn-counter r vcounter))

(defun spawn-ucounter (registry)
  "Update ucounter value."
  (decf (ucounter registry))
  (ucounter registry))

(defmacro reset-counter (registry accessor)
  "Reset counter in REGISTRY with ACCESSOR."
  (mof:with-gensyms (global)
    `(let ((,global ,(intern (mof:cat "*INITIAL-" (string accessor) "*"))))
       (progn
         (setf (,accessor ,registry) ,global)
         (values)))))
(defun reset-ecounter (r) "See RESET-COUNTER." (reset-counter r ecounter))
(defun reset-ucounter (r) "See RESET-COUNTER." (reset-counter r ucounter))
(defun reset-vcounter (r) "See RESET-COUNTER." (reset-counter r vcounter))

(defun spawn-rcounter ()
  "Increment the world RCOUNTER."
  (incf (rcounter *world*))
  (rcounter *world*))

(defun reset-rcounter ()
  "Reset the world RCOUNTER."
  (setf (rcounter *world*) *initial-rcounter*)
  (values))

(defun reset-world ()
  "Reset the whole world."
  (setf (rcounter *world*) *initial-rcounter*)
  (setf (rtable *world*) (make-hash-table))
  (values))

(defun reset-registry (registry)
  "Reset the contents of REGISTRY."
  (setf (ecounter registry) *initial-ecounter*)
  (setf (etable registry) (make-hash-table))
  (setf (ucounter registry) *initial-ucounter*)
  (setf (utable registry) (make-hash-table))
  (setf (vcounter registry) *initial-vcounter*)
  (setf (vtable registry) (make-hash-table))
  (values))

(defun yield-id (entry)
  "Return ID of ENTRY."
  (when entry
    (id entry)))

(defun dump-registry (registry &key simple)
  "Dump the contents of the tables from REGISTRY."
  (if simple
      (with-slots (rid name ecounter etable ucounter utable vcounter vtable xid control) registry
        (format t "~&RID: ~A~%NAME: ~S~%ECOUNTER: ~A~%ETABLE: ~A~%UCOUNTER: ~A~%UTABLE: ~A~%VCOUNTER: ~A~%VTABLE: ~A~%XID: ~A~%CONTROL: ~A~%"
                rid name ecounter etable ucounter utable vcounter vtable xid control))
      (progn (format t "~&** ENTRIES~%")
             (maphash #'(lambda (k v)
                          (with-slots (vid id prev next value buriedp) v
                            (let ((fmt "~S => ~S~%")
                                  (slots (list vid id prev next value buriedp)))
                              (format t fmt k slots))))
                      (etable registry))
             (format t "~&** VOLUMES~%")
             (maphash #'(lambda (k v)
                          (with-slots (rid vid name table prev next) v
                            (format t "~S => ~S~%" k
                                    (list rid vid name table prev next))))
                      (vtable registry)))))

(defun dump-world ()
  "Dump the contents of the world."
  (let ((registries (loop :for v :being :the :hash-values :in (rtable *world*)
                          :collect v)))
    (loop :for registry :in registries
          :do (dump-registry registry))
    (when registries
      (dump-registries))
    (values)))

(defun add-registry (registry)
  "Add REGISTRY to WORLD."
  (setf (gethash (rcounter *world*) (rtable *world*)) registry)
  registry)

(defgeneric add-record (record store)
  (:documentation "Add RECORD to REGISTRY."))
(defmethod add-record ((v volume) (r registry))
  (setf (gethash (vcounter r) (vtable r)) v)
  v)
(defmethod add-record ((e entry) (r registry))
  (setf (gethash (ecounter r) (etable r)) e)
  e)
(defmethod add-record ((e entry) (v volume))
  (setf (gethash (id e) (table v)) e)
  e)
(defmethod add-record ((u unit) (r registry))
  (setf (gethash (ucounter r) (utable r)) u)
  u)
(defmethod add-record ((u unit) (v volume))
  (setf (gethash (id u) (table v)) u)
  u)

(defgeneric delete-record (record store)
  (:documentation "Remove RECORD from STORE."))
(defmethod delete-record ((e entry) (r registry))
  (let ((id (id e)))
    (remhash id (etable r))
    (decf (ecounter r))))
(defmethod delete-record ((e entry) (v volume))
  (let ((id (id e)))
    (remhash id (table v))))
(defmethod delete-record ((u unit) (r registry))
  (let ((id (id u)))
    (remhash id (utable r))
    (decf (ucounter r))))
(defmethod delete-record ((u unit) (v volume))
  (let ((id (id u)))
    (remhash id (table v))))

(defmethod print-object ((e entry) stream)
  (print-unreadable-object (e stream :type t)
    (with-slots (id) e
      (format stream "~A" id))))
(defmethod print-object ((u unit) stream)
  (print-unreadable-object (u stream :type t)
    (with-slots (id) u
      (format stream "~A" id))))
(defmethod print-object ((v volume) stream)
  (print-unreadable-object (v stream :type t)
    (with-slots (vid) v
      (format stream "~A" vid))))
(defmethod print-object ((r registry) stream)
  (print-unreadable-object (r stream :type t)
    (with-slots (rid name) r
      (format stream "~A/~S" rid name))))
(defmethod print-object ((ht hash-table) stream)
  (print-unreadable-object (ht stream :type t)
    (format stream "~A/~A" (hash-table-test ht) (hash-table-count ht))))

(defmethod initialize-instance :after ((e entry) &key registry)
  "Update entry E in REGISTRY."
  (let ((counter (spawn-ecounter registry)))
    (with-slots (id) e
      (setf id counter))))

(defun make-entry (vid registry &optional prev next value)
  "Create an instance of the entry class."
  (make-instance 'entry :vid vid :prev prev :next next :value value :registry registry))

(defmethod initialize-instance :after ((v volume) &key registry)
  "Update volume v in REGISTRY."
  (let ((counter (spawn-vcounter registry)))
    (with-slots (vid) v
      (setf vid counter))))

(defmethod prev ((e null)) "Return nil on null entries." nil)
(defmethod next ((e null)) "Return nil on null entries." nil)

(defun forge-entry (volume registry &optional prev next value)
  "Create an entry under VOLUME in REGISTRY."
  (let* ((vid (vid volume))
         (entry (make-entry vid registry prev next value)))
    (add-record entry registry)
    (add-record entry volume)))

(defun make-volume (registry name &optional (prev -1) (next -1))
  "Create an instance of the volume class."
  (let ((rid (rid registry)))
    (make-instance 'volume :rid rid :name (string-upcase name)
                           :prev prev :next next
                           :registry registry)))

(defun forge-volume (registry name &optional (prev -1) (next -1))
  "Create a volume under registry RID in REGISTRY."
  (let ((volume (make-volume registry name prev next)))
    (add-record volume registry)))

(defun make-registry (&optional (name (genstring "REGISTRY")))
  "Create an instance of the registry class."
  (make-instance 'registry :rid (spawn-rcounter) :name (string-upcase name)))

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

(defun boot-world ()
  "Initialize the world."
  (setf *world* (make-world)))

(defun forge-entries (volume registry &optional feed)
  "Forge unlinked entries in VOLUME under REGISTRY. If FEED is true, use it to seed values."
  (if feed
      (loop :for item :in feed :do (forge-entry volume registry nil nil item))
      nil))

(defun find-next (entry volume)
  "Return the closest next element in VOLUME."
  (let* ((id (id entry))
         (table (table volume))
         (entry (gethash (1+ id) table)))
    (if entry
        entry
        (let* ((entries (find-records volume))
               (mem (member entry entries))
               (next (second mem)))
          (when next
            next)))))

(defun find-prev (entry volume)
  "Return the closest previous element in VOLUME."
  (let* ((id (id entry))
         (table (table volume))
         (entry (gethash (1- id) table)))
    (if entry
        entry
        (let* ((entries (nreverse (find-records volume)))
               (mem (member entry entries))
               (prev (second mem)))
          (when prev
            prev)))))

(defun link-records (volume)
  "Link the records in VOLUME to one another."
  (let* ((records (find-records volume)) ;returns records from the hash table
         ;; Note: does this need fixing?
         ;; (cstart (id (volume-start volume)))
         ;; (cend (id (volume-end volume)))
         (cstart (id (first records)))
         (cend (id (mof:last* records))))
    (when records
      (loop :for record :in records
            :for id = (id record)
            :do (cond ((= id cstart)
                       (setf (next record) (find-next record volume)))
                      ((= id cend)
                       (setf (prev record) (find-prev record volume)))
                      (t (progn
                           (setf (prev record) (find-record (1- id) volume))
                           (setf (next record) (find-record (1+ id) volume))))))
      (setf (linkedp volume) t))))

(defun import-feed (feed name registry)
  "Import items from FEED to REGISTRY with name NAME."
  (let* ((name (build-name "VOLUME" name))
         (volume (forge-volume registry name)))
    (forge-entries volume registry feed)
    (link-records volume)
    registry))

(defgeneric find-registry (query)
  (:documentation "Return the registry which matches QUERY in WORLD."))
(defmethod find-registry ((query integer))
  (let ((val (gethash query (rtable *world*))))
    (when val
      val)))
(defmethod find-registry ((query string))
  (loop :for rid :being :the :hash-keys :in (rtable *world*)
        :for registry = (gethash rid (rtable *world*))
        :when (string-equal (string-upcase query) (name registry))
          :return registry))

(defun find-registries ()
  "Return all registries from the world."
  (loop :for r :being :the :hash-values :in (rtable *world*) :collect r))

(defun dump-registries ()
  "Display inforamtion about the registries."
  (format t "~&** REGISTRIES~%")
  (maphash #'(lambda (k v)
               (with-slots (rid name vcounter ecounter ucounter) v
                 (format t "~S => ~S~%" k (list rid name vcounter ecounter ucounter))))
           (rtable *world*)))

(defgeneric find-volume (query registry)
  (:documentation "Return a volume which matches QUERY in REGISTRY."))
(defmethod find-volume ((query integer) (r registry))
  (let ((val (gethash query (vtable r))))
    (when val
      val)))
(defmethod find-volume ((query string) (r registry))
  (loop :for vid :being :the :hash-keys :in (vtable r)
        :for volume = (gethash vid (vtable r))
        :when (string-equal (string-upcase query) (name volume))
          :return volume))

(defun find-volumes (registry &key (skip #'false))
  "Return all volumes from REGISTRY, except SKIP"
  (loop :for volume :being :the :hash-values :in (vtable registry)
        :unless (funcall skip volume)
        :collect volume))

(defgeneric find-record (query registry)
  (:documentation "Return an entry which matches QUERY in VOLUME."))
(defmethod find-record ((query integer) (r registry))
  (multiple-value-bind (value present)
      (gethash query (etable r))
    (when present
      value)))
(defmethod find-record ((query integer) (v volume))
  (multiple-value-bind (value present)
      (gethash query (table v))
    (when present
      value)))

(defun sort-records (records &key (key #'id))
  "Sort records numerically."
  (sort records #'< :key key))

(defun entryp (record)
  "Return true if RECORD is of type ENTRY."
  (eql (type-of record) 'entry))

(defun unitp (record)
  "Return true if RECORD is of type UNIT."
  (eql (type-of record) 'unit))

(defun entry-or-unit-p (record)
  "Return true if RECORD is an entry or unit."
  (or (entryp record)
      (unitp record)))

(defun true (arg)
  "Return true for anything."
  (declare (ignore arg))
  t)

(defun false (arg)
  "Return false for anything."
  (declare (ignore arg))
  nil)

(defgeneric find-entries (store)
  (:documentation "Return entries from STORE."))
(defmethod find-entries ((r registry))
  (loop :for entry :being :the :hash-values :in (etable r)
        :collect entry))
(defmethod find-entries ((v volume))
  (loop :for record :being :the :hash-values :in (table v)
        :when (entryp record)
          :collect record))

(defgeneric find-units (store)
  (:documentation "Return units from STORE."))
(defmethod find-units ((r registry))
  (loop :for unit :being :the :hash-values :in (utable r)
        :collect unit))
(defmethod find-units ((v volume))
  (loop :for record :being :the :hash-values :in (table v)
        :when (unitp record)
          :collect record))

(defgeneric find-records (store &key &allow-other-keys)
  (:documentation "Return all records from STORE."))
(defmethod find-records ((r registry) &key sort)
  (let* ((entries (find-entries r))
         (units (loop :for unit :being :the :hash-values :in (utable r)
                      :collect unit))
         (records (nconc entries units)))
    (if sort
        (sort-records records)
        records)))
(defmethod find-records ((v volume) &key sort)
  (loop :for record :being :the :hash-values :in (table v)
        :collect record :into records
        :finally (return (if sort
                             (sort-records records)
                             records))))

(defun volume-start-p (entry)
  "Return true if entry is found at the start of a volume."
  (when (and (null (prev entry))
             (next entry))
    t))

(defun volume-end-p (entry)
  "Return true if entry is found at the end of a volume."
  (when (and (prev entry)
             (null (next entry)))
    t))

(defgeneric dump-volume (volume &key &allow-other-keys)
  (:documentation "Print information about VOLUME."))
(defmethod dump-volume ((v volume) &key complete)
  (let ((registry (find-registry (rid v))))
    (with-slots (rid vid name table prev next) v
      (if complete
          (loop :for k :being :the :hash-keys :in (table v)
                :for entry = (find-record k registry)
                :do (format t "~&~A => ~A~%" k entry))
          (format t "~&RID: ~A~%VID: ~A~%NAME: ~A~%TABLE: ~A~%PREV: ~A~%NEXT: ~A~%"
                  rid vid name table prev next))
      (values))))

(defgeneric dump-entry (entry &key &allow-other-keys)
  (:documentation "Print information about an entry."))
(defmethod dump-entry ((e entry) &key simple)
  (with-slots (vid id prev next value buriedp) e
    (if simple
        (format t "~&PREV: ~S~%NEXT: ~S~%VALUE: ~S~%BURIEDP: ~S~%"
                prev next value buriedp)
        (format t "~&VID: ~S~%ID: ~S~%PREV: ~S~%NEXT: ~S~%VALUE: ~S~%BURIEDP: ~S~%"
                vid id prev next value buriedp))
    (values)))

(defun display-volume (query name)
  "Display the contents of volume QUERY in registry NAME."
  (dump-volume (find-volume query (find-registry name))))
(defun display-entry (query name)
  "Display the contents of entry QUERY in registry NAME."
  (dump-entry (find-record query (find-registry name))))

(defun locate-volume (query name)
  "Locate the volume QUERY in registry NAME."
  (find-volume query (find-registry name)))
(defun locate-entry (query name)
  "Locate the entry ID in registry NAME."
  (find-record query (find-registry name)))
(mof:defalias locate-registry find-registry)

(defun max-volume (registry)
  "Return the biggest volume in REGISTRY."
  (first (sort (find-volumes registry) #'> :key #'(lambda (v) (hash-table-size (table v))))))
(mof:defalias wall max-volume)

(defun shallow-copy-registry (template registry)
  "Create a shallow copy of TEMPLATE to REGISTRY."
  (declare (ignorable template registry))
  (unless (and template registry)
    (error "Either the template or the target registry does not exist."))
  nil)

(defun forge-record (&optional prev next left right buriedp)
  "Create a record instance."
  (make-instance 'record :prev prev :next next :left left :right right :buriedp buriedp))
