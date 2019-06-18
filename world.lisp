;;;; world.lisp

(in-package #:muso/core)

(defmacro spawn-counter (registry accessor)
  "Generate a new counter in REGISTRY with ACCESSOR."
  `(progn (incf (,accessor ,registry))
          (,accessor ,registry)))
(defun spawn-ecounter (r) "See SPAWN-COUNTER." (spawn-counter r ecounter))
(defun spawn-ccounter (r) "See SPAWN-COUNTER." (spawn-counter r ccounter))

(defmacro reset-counter (registry accessor)
  "Reset counter in REGISTRY with ACCESSOR."
  (mof:with-gensyms (global)
    `(let ((,global ,(intern (mof:cat "*INITIAL-" (string accessor) "*"))))
       (progn
         (setf (,accessor ,registry) ,global)
         (values)))))
(defun reset-ecounter (r) "See RESET-COUNTER." (reset-counter r ecounter))
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
      (with-slots (rid rname ecounter etable ccounter ctable vid control) registry
        (format t "~&RID: ~A~%RNAME: ~S~%ECOUNTER: ~A~%ETABLE: ~A~%CCOUNTER: ~A~%CTABLE: ~A~%VID: ~A~%CONTROL: ~A~%"
                rid rname ecounter etable ccounter ctable vid control))
      (progn (format t "~&** ENTRIES~%")
             (maphash #'(lambda (k v)
                          (with-slots (cid id prev next value buried) v
                            (let ((fmt "~S => ~S~%")
                                  (slots (list cid id prev next value buried)))
                              (format t fmt k slots))))
                      (etable registry))
             (format t "~&** COLUMNS~%")
             (maphash #'(lambda (k v)
                          (with-slots (rid cid cname etable cprev cnext) v
                            (format t "~S => ~S~%" k
                                    (list rid cid cname etable cprev cnext))))
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

(defgeneric add-record (data store)
  (:documentation "Add RECORD to REGISTRY."))
(defmethod add-record ((e entry) (r registry))
  (setf (gethash (ecounter r) (etable r)) e)
  e)
(defmethod add-record ((c column) (r registry))
  (setf (gethash (ccounter r) (ctable r)) c)
  c)
(defmethod add-record ((e entry) (c column))
  (setf (gethash (id e) (etable c)) e)
  e)

(defun add-registry (registry)
  "Add REGISTRY to WORLD."
  (setf (gethash (rcounter *world*) (rtable *world*)) registry)
  registry)

(defmethod print-object ((e entry) stream)
  (print-unreadable-object (e stream :type t)
    (with-slots (id) e
      (format stream "~A" id))))
(defmethod print-object ((c column) stream)
  (print-unreadable-object (c stream :type t)
    (with-slots (cid) c
      (format stream "~A" cid))))
(defmethod print-object ((r registry) stream)
  (print-unreadable-object (r stream :type t)
    (with-slots (rid rname) r
      (format stream "~A ~A" rid rname))))
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
  "Create an entry under column CID in REGISTRY."
  (let* ((cid (cid column))
         (entry (make-entry cid registry prev next value)))
    (add-record entry registry)
    (add-record entry column)))

(defun make-column (registry cname &optional (cprev -1) (cnext -1))
  "Create an instance of the column class."
  (let ((rid (rid registry)))
    (make-instance 'column :rid rid :cname (string-upcase cname)
                           :cprev cprev :cnext cnext
                           :registry registry)))

(defun forge-column (registry cname &optional (cprev -1) (cnext -1))
  "Create a column under registry RID in REGISTRY."
  (let ((column (make-column registry cname cprev cnext)))
    (add-record column registry)))

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
         (table (etable column))
         (entry (gethash (1+ id) table)))
    (if entry
        entry
        (let* ((entries (find-entries column))
               (mem (member entry entries))
               (next (second mem)))
          (when next
            next)))))

(defun find-prev (entry column)
  "Return the closest previous element in COLUMN."
  (let* ((id (id entry))
         (table (etable column))
         (entry (gethash (1- id) table)))
    (if entry
        entry
        (let* ((entries (nreverse (find-entries column)))
               (mem (member entry entries))
               (prev (second mem)))
          (when prev
            prev)))))

(defun link-entries (column)
  "Attached the entries in COLUMN to one another."
  (let* ((entries (find-entries column))
         (ctop (id (first entries)))
         (cbottom (id (mof:last* entries))))
    (loop :for entry :in entries
          :for id = (id entry)
          :do (cond ((= id ctop)
                     (setf (next entry) (find-next entry column)))
                    ((= id cbottom)
                     (setf (prev entry) (find-prev entry column)))
                    (t (progn
                         (setf (prev entry) (find-entry (1- id) column))
                         (setf (next entry) (find-entry (1+ id) column))))))))

(defun import-feed (feed name registry)
  "Import items from FEED to REGISTRY with name NAME."
  (let* ((cname (if (mof:empty-string-p name) (genstring "COLUMN") name))
         (column (forge-column registry cname)))
    (forge-entries column registry feed)
    (link-entries column)
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
        :when (string-equal (string-upcase query) (rname registry))
        :return registry))

(defun find-registries ()
  "Return all registries from the world."
  (loop :for r :being :the :hash-values :in (rtable *world*) :collect r))

(defun dump-registries()
  "Display inforamtion about the registries."
  (format t "~&** REGISTRIES~%")
  (maphash #'(lambda (k v)
               (with-slots (rid rname ccounter ecounter) v
                 (format t "~S => ~S~%" k (list rid rname ccounter ecounter))))
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
        :when (string-equal (string-upcase query) (cname column))
        :return column))

(defgeneric find-columns (registry)
  (:documentation "Return all columns from REGISTRY."))
(defmethod find-columns ((r registry))
  (loop :for c :being :the :hash-values :in (ctable r) :collect c))

(defgeneric find-entry (query registry)
  (:documentation "Return an entry which matches QUERY in COLUMN."))
(defmethod find-entry ((query integer) (r registry))
  (let ((val (gethash query (etable r))))
    (when val
      val)))
(defmethod find-entry ((query integer) (c column))
  (let ((val (gethash query (etable c))))
    (when val
      val)))

(defgeneric find-entries (store &key)
  (:documentation "Return all entries from STORE."))
(defmethod find-entries ((r registry) &key column)
  (cond (column (find-entries column))
        (t (loop :for entry :being :the :hash-values :in (etable r)
                 :collect entry))))
(defmethod find-entries ((c column) &key)
  (loop :for entry :being :the :hash-values :in (etable c)
        :collect entry :into entries
        :finally (return (sort entries #'< :key #'id))))

(defun locate-entities (registry &optional column)
  "Return registry and column as values."
  (cond (column (values (locate-registry registry)
                        (locate-column column registry)))
        (t (locate-registry registry))))

(defun collect (registry &optional column)
  "Return all the entries in registry or as scoped by column. Registry and column here are represented in query format, that is, as string or integer."
  (multiple-value-bind (r c)
      (locate-entities registry column)
    (find-entries r :column c)))

(defgeneric dump-column (column &key &allow-other-keys)
  (:documentation "Print information about COLUMN."))
(defmethod dump-column ((c column) &key complete)
  (let ((registry (find-registry (rid c))))
    (with-slots (rid cid cname etable cprev cnext) c
      (if complete
          (loop :for k :being :the :hash-keys :in (etable c)
                :for entry = (find-entry k registry)
                :do (format t "~&~A => ~A~%" k entry))
          (format t "~&RID: ~A~%CID: ~A~%CNAME: ~A~%ETABLE: ~A~%CPREV: ~A~%CNEXT: ~A~%"
                  rid cid cname etable cprev cnext))
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

(defun display-column (query rname)
  "Display the contents of column QUERY in registry RNAME."
  (dump-column (find-column query (find-registry rname))))
(defun display-entry (query rname)
  "Display the contents of entry QUERY in registry RNAME."
  (dump-entry (find-entry query (find-registry rname))))

(defun locate-column (query rname)
  "Locate the column CNAME in registry RNAME."
  (find-column query (find-registry rname)))
(defun locate-entry (query rname)
  "Locate the entry ID in registry RNAME."
  (find-entry query (find-registry rname)))
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

(defgeneric delete-record (entry store)
  (:documentation "Remove ENTRY from STORE."))
(defmethod delete-record ((e entry) (r registry))
  (let* ((id (id e)))
    (remhash id (etable r))
    (decf (ecounter r))))
(defmethod delete-record ((e entry) (c column))
  (let* ((id (id e)))
    (remhash id (etable c))))
