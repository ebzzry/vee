;;;; world.lisp

(in-package #:muso/core)

(defmacro spawn-counter (registry accessor)
  "Generate a new counter in REGISTRY with ACCESSOR."
  `(progn (incf (,accessor ,registry))
          (,accessor ,registry)))
(defun spawn-ecounter (r) "See SPAWN-COUNTER." (spawn-counter r ecounter))
(defun spawn-ccounter (r) "See SPAWN-COUNTER." (spawn-counter r ccounter))

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
(defun reset-ccounter (r) "See RESET-COUNTER." (reset-counter r ccounter))

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
  (setf (ccounter registry) *initial-ccounter*)
  (setf (ctable registry) (make-hash-table))
  (values))

(defun yield-id (entry)
  "Return ID of ENTRY."
  (when entry
    (id entry)))

(defun dump-registry (registry &key simple)
  "Dump the contents of the tables from REGISTRY."
  (if simple
      (with-slots (rid name ecounter etable ucounter utable ccounter ctable vid control) registry
        (format t "~&RID: ~A~%NAME: ~S~%ECOUNTER: ~A~%ETABLE: ~A~%UCOUNTER: ~A~%UTABLE: ~A~%CCOUNTER: ~A~%CTABLE: ~A~%VID: ~A~%CONTROL: ~A~%"
                rid name ecounter etable ucounter utable ccounter ctable vid control))
      (progn (format t "~&** ENTRIES~%")
             (maphash #'(lambda (k v)
                          (with-slots (cid id prev next value buried) v
                            (let ((fmt "~S => ~S~%")
                                  (slots (list cid id prev next value buried)))
                              (format t fmt k slots))))
                      (etable registry))
             (format t "~&** COLUMNS~%")
             (maphash #'(lambda (k v)
                          (with-slots (rid cid name table prev next) v
                            (format t "~S => ~S~%" k
                                    (list rid cid name table prev next))))
                      (ctable registry)))))

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
(defmethod add-record ((c column) (r registry))
  (setf (gethash (ccounter r) (ctable r)) c)
  c)
(defmethod add-record ((e entry) (r registry))
  (setf (gethash (ecounter r) (etable r)) e)
  e)
(defmethod add-record ((e entry) (c column))
  (setf (gethash (id e) (table c)) e)
  e)
(defmethod add-record ((u unit) (r registry))
  (setf (gethash (ucounter r) (utable r)) u)
  u)
(defmethod add-record ((u unit) (c column))
  (setf (gethash (id u) (table c)) u)
  u)

(defgeneric delete-record (record store)
  (:documentation "Remove RECORD from STORE."))
(defmethod delete-record ((e entry) (r registry))
  (let ((id (id e)))
    (remhash id (etable r))
    (decf (ecounter r))))
(defmethod delete-record ((e entry) (c column))
  (let ((id (id e)))
    (remhash id (table c))))
(defmethod delete-record ((u unit) (r registry))
  (let ((id (id u)))
    (remhash id (utable r))
    (decf (ucounter r))))
(defmethod delete-record ((u unit) (c column))
  (let ((id (id u)))
    (remhash id (table c))))

(defmethod print-object ((e entry) stream)
  (print-unreadable-object (e stream :type t)
    (with-slots (id) e
      (format stream "~A" id))))
(defmethod print-object ((u unit) stream)
  (print-unreadable-object (u stream :type t)
    (with-slots (id) u
      (format stream "~A" id))))
(defmethod print-object ((c column) stream)
  (print-unreadable-object (c stream :type t)
    (with-slots (cid) c
      (format stream "~A" cid))))
(defmethod print-object ((r registry) stream)
  (print-unreadable-object (r stream :type t)
    (with-slots (rid name) r
      (format stream "~A ~A" rid name))))
(defmethod print-object ((ht hash-table) stream)
  (print-unreadable-object (ht stream :type t)
    (format stream "~A ~A" (hash-table-test ht) (hash-table-count ht))))

(defmethod initialize-instance :after ((e entry) &key registry)
  "Update entry E in REGISTRY."
  (let ((counter (spawn-ecounter registry)))
    (with-slots (id) e
      (setf id counter))))

(defun make-entry (cid registry &optional prev next value)
  "Create an instance of the entry class."
  (make-instance 'entry :cid cid :prev prev :next next :value value :registry registry))

(defmethod initialize-instance :after ((c column) &key registry)
  "Update column C in REGISTRY."
  (let ((counter (spawn-ccounter registry)))
    (with-slots (cid) c
      (setf cid counter))))

(defmethod prev ((e null)) "Return nil on null entries." nil)
(defmethod next ((e null)) "Return nil on null entries." nil)

(defun forge-entry (column registry &optional prev next value)
  "Create an entry under COLUMN in REGISTRY."
  (let* ((cid (cid column))
         (entry (make-entry cid registry prev next value)))
    (add-record entry registry)
    (add-record entry column)))

(defun make-column (registry name &optional (prev -1) (next -1))
  "Create an instance of the column class."
  (let ((rid (rid registry)))
    (make-instance 'column :rid rid :name (string-upcase name)
                           :prev prev :next next
                           :registry registry)))

(defun forge-column (registry name &optional (prev -1) (next -1))
  "Create a column under registry RID in REGISTRY."
  (let ((column (make-column registry name prev next)))
    (add-record column registry)))

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

(defun forge-entries (column registry &optional feed)
  "Forge unlinked entries in COLUMN under REGISTRY. If FEED is true, use it to seed values."
  (if feed
      (loop :for item :in feed :do (forge-entry column registry nil nil item))
      nil))

(defun find-next (entry column)
  "Return the closest next element in COLUMN."
  (let* ((id (id entry))
         (table (table column))
         (entry (gethash (1+ id) table)))
    (if entry
        entry
        (let* ((entries (find-records column))
               (mem (member entry entries))
               (next (second mem)))
          (when next
            next)))))

(defun find-prev (entry column)
  "Return the closest previous element in COLUMN."
  (let* ((id (id entry))
         (table (table column))
         (entry (gethash (1- id) table)))
    (if entry
        entry
        (let* ((entries (nreverse (find-records column)))
               (mem (member entry entries))
               (prev (second mem)))
          (when prev
            prev)))))

(defun link-records (column)
  "Link the records in COLUMN to one another."
  (let* ((records (find-records column)) ;returns records from the hash table
         ;; Note: fix this
         ;; (cstart (id (column-start column)))
         ;; (cend (id (column-end column)))
         (cstart (id (first records)))
         (cend (id (mof:last* records))))
    (when records
      (loop :for record :in records
            :for id = (id record)
            :do (cond ((= id cstart)
                       (setf (next record) (find-next record column)))
                      ((= id cend)
                       (setf (prev record) (find-prev record column)))
                      (t (progn
                           (setf (prev record) (find-record (1- id) column))
                           (setf (next record) (find-record (1+ id) column))))))
      (setf (linked column) t))))

(defun import-feed (feed name registry)
  "Import items from FEED to REGISTRY with name NAME."
  (let* ((name (build-name "COLUMN" name))
         (column (forge-column registry name)))
    (forge-entries column registry feed)
    (link-records column)
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
               (with-slots (rid name ccounter ecounter ucounter) v
                 (format t "~S => ~S~%" k (list rid name ccounter ecounter ucounter))))
           (rtable *world*)))

(defgeneric find-column (query registry)
  (:documentation "Return a column which matches QUERY in REGISTRY."))
(defmethod find-column ((query integer) (r registry))
  (let ((val (gethash query (ctable r))))
    (when val
      val)))
(defmethod find-column ((query string) (r registry))
  (loop :for cid :being :the :hash-keys :in (ctable r)
        :for column = (gethash cid (ctable r))
        :when (string-equal (string-upcase query) (name column))
        :return column))

(defgeneric find-columns (registry)
  (:documentation "Return all columns from REGISTRY."))
(defmethod find-columns ((r registry))
  (loop :for c :being :the :hash-values :in (ctable r) :collect c))

(defgeneric find-record (query registry)
  (:documentation "Return an entry which matches QUERY in COLUMN."))
(defmethod find-record ((query integer) (r registry))
  (multiple-value-bind (value present)
      (gethash query (etable r))
    (when present
      value)))
(defmethod find-record ((query integer) (c column))
  (multiple-value-bind (value present)
      (gethash query (table c))
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

(defgeneric find-entries (store)
  (:documentation "Return entries from STORE."))
(defmethod find-entries ((r registry))
  (loop :for entry :being :the :hash-values :in (etable r)
        :collect entry))
(defmethod find-entries ((c column))
  (loop :for record :being :the :hash-values :in (table c)
        :when (entryp record)
          :collect record))

(defgeneric find-units (store)
  (:documentation "Return units from STORE."))
(defmethod find-units ((r registry))
  (loop :for unit :being :the :hash-values :in (utable r)
        :collect unit))
(defmethod find-units ((c column))
  (loop :for record :being :the :hash-values :in (table c)
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
(defmethod find-records ((c column) &key sort)
  (loop :for record :being :the :hash-values :in (table c)
        :collect record :into records
        :finally (return (if sort
                             (sort-records records)
                             records))))

(defun column-start-p (entry)
  "Return true if entry is found at the start of a column."
  (when (and (null (prev entry))
             (next entry))
    t))

(defun column-end-p (entry)
  "Return true if entry is found at the end of a column."
  (when (and (prev entry)
             (null (next entry)))
    t))

(defgeneric dump-column (column &key &allow-other-keys)
  (:documentation "Print information about COLUMN."))
(defmethod dump-column ((c column) &key complete)
  (let ((registry (find-registry (rid c))))
    (with-slots (rid cid name table prev next) c
      (if complete
          (loop :for k :being :the :hash-keys :in (table c)
                :for entry = (find-record k registry)
                :do (format t "~&~A => ~A~%" k entry))
          (format t "~&RID: ~A~%CID: ~A~%NAME: ~A~%TABLE: ~A~%PREV: ~A~%NEXT: ~A~%"
                  rid cid name table prev next))
      (values))))

(defgeneric dump-entry (entry &key &allow-other-keys)
  (:documentation "Print information about an entry."))
(defmethod dump-entry ((e entry) &key simple)
  (with-slots (cid id prev next value buried) e
    (if simple
        (format t "~&PREV: ~S~%NEXT: ~S~%VALUE: ~S~%BURIED: ~S~%"
                prev next value buried)
        (format t "~&CID: ~S~%ID: ~S~%PREV: ~S~%NEXT: ~S~%VALUE: ~S~%BURIED: ~S~%"
                cid id prev next value buried))
    (values)))

(defun display-column (query name)
  "Display the contents of column QUERY in registry NAME."
  (dump-column (find-column query (find-registry name))))
(defun display-entry (query name)
  "Display the contents of entry QUERY in registry NAME."
  (dump-entry (find-record query (find-registry name))))

(defun locate-column (query name)
  "Locate the column QUERY in registry NAME."
  (find-column query (find-registry name)))
(defun locate-entry (query name)
  "Locate the entry ID in registry NAME."
  (find-record query (find-registry name)))
(mof:defalias locate-registry find-registry)

(defun max-column (registry)
  "Return the biggest column in REGISTRY."
  (first (sort (find-columns registry) #'> :key #'hash-table-size)))
(mof:defalias wall max-column)

(defun shallow-copy-registry (template registry)
  "Create a shallow copy of TEMPLATE to REGISTRY."
  (declare (ignorable template registry))
  (unless (and template registry)
    (error "Either the template or the target registry does not exist."))
  nil)

(defun forge-record (&optional prev next left right buried)
  "Create a record instance."
  (make-instance 'record :prev prev :next next :left left :right right :buried buried))
