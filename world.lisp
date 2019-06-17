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
                          (with-slots (rid cid cname cstart cend clength cleft cright) v
                            (format t "~S => ~S~%" k
                                    (list rid cid cname cstart cend clength cleft cright))))
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

(defgeneric add-record (record registry)
  (:documentation "Add RECORD to REGISTRY."))
(defmethod add-record ((entry entry) (r registry))
  (setf (gethash (ecounter r) (etable r)) entry)
  entry)
(defmethod add-record ((column column) (r registry))
  (setf (gethash (ccounter r) (ctable r)) column)
  column)

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
    (with-slots (cid cstart cend clength) c
      (setf clength (1+ (- cend cstart))
            cid counter))))

(defmethod prev ((e null)) "Return nil on null entries." nil)
(defmethod next ((e null)) "Return nil on null entries." nil)

(defun forge-entry (cid registry &optional prev next value)
  "Create an entry under column CID in REGISTRY."
  (let ((entry (make-entry cid registry prev next value)))
    (add-record entry registry)))

(defun make-column (rid registry cname cstart cend &optional (cleft -1) (cright -1))
  "Create an instance of the column class."
  (make-instance 'column :rid rid :cname (string-upcase cname)
                         :cstart cstart :cend cend
                         :cleft cleft :cright cright
                         :registry registry))

(defun forge-column (rid registry cname cstart cend &optional (cleft -1) (cright -1))
  "Create a column under registry RID in REGISTRY."
  (let ((column (make-column rid registry cname cstart cend cleft cright)))
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

(defun initialize-world ()
  "Initialize the world."
  (setf *world* (make-world)))
(mof:defalias boot-world initialize-world)

(defun forge-entries (column registry &key feed)
  "Forge unlinked entries in COLUMN under REGISTRY. If FEED is true, use it to seed values."
  (if feed
      (loop :for item :in feed
            :do (forge-entry (cid column) registry nil nil item))
      (loop :for cid :from (cstart column) :to (cend column)
            :do (forge-entry (cid column) registry))))

(defun link-entries (column registry)
  "Attached the entries in column under registry to one another."
  (loop :for id :from (cstart column) :to (cend column)
        :for entry = (find-entry id registry)
        :do (cond ((= id (cstart column))
                   (setf (next entry) (find-entry (1+ id) registry)))
                  ((= id (cend column))
                   (setf (prev entry) (find-entry (1- id) registry)))
                  (t (progn
                       (setf (prev entry) (find-entry (1- id) registry))
                       (setf (next entry) (find-entry (1+ id) registry)))))))

(defun import-feed (feed name registry)
  "Import items from FEED to REGISTRY with name NAME."
  (let* ((length (length feed))
         (start (1+ (ecounter registry)))
         (offset (1- (+ start length)))
         (cname (if (mof:empty-string-p name) (genstring "COLUMN") name))
         (column (forge-column (rid registry) registry cname start offset)))
    (forge-entries column registry :feed feed)
    (link-entries column registry)
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

(defun find-entries (registry &optional column)
  "Return all items in registry. Limit search only to COLUMN if it is specified."
  (cond (column (loop :for cid :from (cstart column) :to (cend column)
                      :collect (find-entry cid registry)))
        (t (loop :for entry :being :the :hash-values :in (etable registry)
                 :collect entry))))

(defun locate-entities (registry &optional column)
  "Return registry and column as values."
  (cond (column (values (locate-registry registry)
                        (locate-column column registry)))
        (t (locate-registry registry))))

(defun collect (registry &optional column)
  "Return all the entries in registry or as scoped by column. Registry and column here are represented in query format, that is, as string or integer."
  (multiple-value-bind (r c)
      (locate-entities registry column)
    (find-entries r c)))

(defgeneric dump-column (column &key &allow-other-keys)
  (:documentation "Print information about COLUMN."))
(defmethod dump-column ((c column) &key complete)
  (with-slots (rid cid cname cstart cend clength cleft cright) c
    (format t "~&RID: ~A~%CID: ~A~%CNAME: ~A~%CSTART: ~A~%CEND: ~A~%CLENGTH: ~A~%CLEFT: ~A~%CRIGHT: ~A~%"
              rid cid cname cstart cend clength cleft cright)
    (loop :for e :from cstart :to cend
          :for entry = (find-entry e (find-registry rid))
          :do (with-slots (cid id prev next value) entry
                (when complete
                  (format t "~&CID: ~A, ID: ~A, PREV: ~S, NEXT: ~S, VALUE: ~S~%"
                          cid id prev next value))))
    (values)))

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
  (first (sort (find-columns registry) #'> :key #'clength)))
(mof:defalias wall max-column)

(defun mktemp-registry ()
  "Return an empty REGISTRY."
  (make-registry (genstring "REGISTRY")))

(defun shallow-copy-registry (template registry)
  "Create a shallow copy of TEMPLATE to REGISTRY."
  (declare (ignorable template registry))
  (unless (and template registry)
    (error "Either the template or the target registry does not exist."))
  nil)

(defun wall-copy-registry (template registry)
  "Create a wall copy of TEMPLATE to REGISTRY."
  (unless (and template registry)
    (error "Either the template or the target registry does not exist."))
  (let* ((wall (wall template))
         (length (clength wall)))
    (loop :for count :from 1 :to (length (find-columns template))
          :for start = (1+ (ecounter registry))
          :for offset = (+ start length)
          :for cname = (genstring "COLUMN")
          :for column = (forge-column (rid registry) registry cname start offset)

          ;; Note: these entries are not linked to one another, should they be?
          :do (forge-entries column registry))
    registry))

;;; Note: when holes will be linked with normal entries, the sequencing of
;;; CSTART and CEND may be disrupted. So column values must also be updated
;;; appropriately
(defun forge-hole (cid registry &optional prev next)
  "Create a hole entry in registry."
  (forge-entry cid registry prev next nil))

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
