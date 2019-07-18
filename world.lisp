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

(defun spawn-fcounter (volume)
  "Update fcounter value."
  (incf (fcounter volume))
  (fcounter volume))

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
  (initialize-lparallel)
  (values))
(mof:defalias reset reset-world)

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
(defmethod add-record ((f field) (v volume))
  (setf (gethash (id f) (ftable v)) f)
  f)

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

(defun make-entry (vid registry &optional prev next fields)
  "Create an instance of the entry class."
  (make-instance 'entry :vid vid :prev prev :next next :fields fields :registry registry))

(defun make-field (volume value)
  "Create an instance of the field class."
  (make-instance 'field :value value :volume volume))

;;; Note: rename these
(defgeneric fields-values (object)
  (:documentation "Return the values contained inside ENTRY."))
(defmethod fields-values ((e entry))
  (mapcar #'value (fields e)))
(defmethod fields-values ((l list))
  (mapcar #'fields-values l))

(defmethod prev ((o null)) "Return nil on null entries." nil)
(defmethod next ((o null)) "Return nil on null entries." nil)

(defun link-fields (fields)
  "Link FIELDS to each other."
  (let ((start (first fields))
        (end (mof:last* fields))
        (fields (nil-wrap fields)))
    (loop :for field-left :in fields
          :for field-mid :in (rest fields)
          :for field-right :in (rest (rest fields))
          :do (cond ((eql field-mid start) (setf (next field-mid) field-right))
                    ((eql field-mid end) (setf (prev field-mid) field-left))
                    (t (progn
                         (setf (prev field-mid) field-left)
                         (setf (next field-mid) field-right)))))))

(defun forge-fields (values volume)
  "Create fields from a list of values."
  (let ((fields (loop :for value :in values
                      :collect (make-field volume value))))
    (loop :for field :in fields
          :do (progn
                (when (header volume)
                  (destructuring-bind (header fields)
                      (equalize-lists (header volume) fields)
                    (loop :for h :in header
                          :for f :in fields
                          :do (setf (head f) h))))
                (add-record field volume)))
    (link-fields fields)
    fields))

(defun forge-entry (volume registry &optional prev next values)
  "Create an entry under VOLUME in REGISTRY."
  (let* ((vid (vid volume))
         (fields (forge-fields values volume))
         (entry (make-entry vid registry prev next fields)))
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

(defun make-registry (&optional (name (make-registry-name)))
  "Create an instance of the registry class."
  (make-instance 'registry :rid (spawn-rcounter) :name (string-upcase name)))

(defgeneric spawn-registry (query)
  (:documentation "Return a new registry object then add it to the world, or return an existing one."))
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

(defun clone-registry (template)
  "Create a selective copy of TEMPLATE."
  (with-slots (name ecounter etable ucounter utable vcounter vtable) template
    (let* ((cname (mof:cat name (genstring "/")))
           (clone (make-instance 'registry
                                 :rid (spawn-rcounter) :name cname
                                 :ecounter ecounter :etable etable
                                 :ucounter ucounter :utable utable
                                 :vcounter vcounter :vtable vtable)))
      (add-registry clone)
      clone)))

(defun build-registry ()
  "Return a new unique registry."
  (let ((name (make-registry-name)))
    (spawn-registry name)))

(defun make-world ()
  "Create an instance of the world class."
  (make-instance 'world))

(defun initialize-world ()
  "Initialize the world."
  (setf *world* (make-world)))

(defun forge-entries (volume registry &optional feed)
  "Forge unlinked entries in VOLUME under REGISTRY. If FEED is true, use it to seed values."
  (if feed
      (loop :for item :in feed :do (forge-entry volume registry nil nil item))
      nil))

(defun find-next (entry volume)
  "Return the closest next entry in VOLUME from ENTRY."
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
  "Return the closest previous entry in VOLUME from ENTRY"
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
  (let* ((records (find-records volume))
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
(defmethod find-registry ((n null))
  nil)
(defmethod find-registry ((r registry))
  r)
(mof:defalias search-registry find-registry)

(defun find-registries ()
  "Return all registries from the world."
  (loop :for r :being :the :hash-values :in (rtable *world*) :collect r))

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
(defmethod find-volume ((query t) (n null))
  nil)
(defmethod find-volume ((v volume) (r registry))
  v)

(defgeneric find-volumes (registry &key &allow-other-keys)
  (:documentation "Return all volumes from REGISTRY, except SKIP"))
(defmethod find-volumes ((r registry) &key (skip #'false))
  (loop :for volume :being :the :hash-values :in (vtable r)
        :unless (funcall skip volume)
        :collect volume))
(defmethod find-volumes ((s string) &rest args)
  (apply #'find-volumes (find-registry s) args))

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
  (typep record 'entry))

(defun unitp (record)
  "Return true if RECORD is of type UNIT."
  (typep record 'unit))

(defun entry-or-unit-p (record)
  "Return true if RECORD is an entry or unit."
  (or (entryp record)
      (unitp record)))

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

(defun volume-start (volume)
  "Return the first entry in VOLUME."
  (let ((records (find-records volume)))
    (loop :for record :in records
          :when (and (null (prev record))
                     (next record))
          :return record)))

(defun volume-end (volume)
  "Return the last entry in VOLUME."
  (let ((records (nreverse (find-records volume))))
    (loop :for record :in records
          :when (and (prev record)
                     (null (next record)))
            :return record)))

(defgeneric find-entries (store)
  (:documentation "Return entries from STORE."))
(defmethod find-entries ((r registry))
  (loop :for entry :being :the :hash-values :in (etable r)
        :collect entry))
(defmethod find-entries ((v volume))
  (loop :for record :being :the :hash-values :in (table v)
        :when (entryp record)
        :collect record))

(defgeneric table-count (store)
  (:documentation "Return the size of the hash table stored in STORE."))
(defmethod table-count ((v volume))
  (hash-table-count (table v)))

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

(defun max-volume (registry)
  "Return the biggest volume in REGISTRY. Size is determined by the number of records."
  (first (sort (find-volumes registry) #'> :key #'(lambda (v) (hash-table-size (table v))))))
(mof:defalias wall max-volume)

(defun forge-record (&optional prev next left right buriedp)
  "Return a record instance."
  (make-instance 'record :prev prev :next next :left left :right right :buriedp buriedp))

(defun make-match (record volume offset)
  "Return a MATCH object."
  (make-instance 'match :record record :volume volume :offset offset))

(defmethod value ((m match))
  "Return record value from M."
  (fields-values (record m)))

(defmethod id ((m match))
  "Retturn record id from M."
  (id (record m)))

(defun search-volume (query)
  "Return the first volume that that matches QUERY in all the registries."
  (let ((registries (find-registries)))
    (loop :for registry :in registries
          :for volume = (find-volume query registry)
          :when volume
          :return (values volume registry))))

(defun volumep (object)
  "Return true if OBJECT is a volume."
  (typep object 'volume))

(defun registryp (object)
  "Return true if OBJECT is a registry."
  (typep object 'registry))
