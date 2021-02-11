;;;; world.lisp

(in-package #:veda/core)

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

(defun spawn-ccounter (volume)
  "Update ccounter value."
  (incf (ccounter volume))
  (ccounter volume))

(defmacro reset-counter (registry accessor)
  "Reset counter in REGISTRY with ACCESSOR."
  (with-gensyms (global)
    `(let ((,global ,(intern (cat "*INITIAL-" (string accessor) "*"))))
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

(def (reset-world reset) ()
  "Reset the whole world."
  (setf (rcounter *world*) *initial-rcounter*)
  (setf (rtable *world*) (make-hash-table))
  (initialize-lparallel)
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

(defun yield-id (pool)
  "Return ID of POOL."
  (when pool
    (id pool)))

(defun add-registry (registry)
  "Add REGISTRY to WORLD."
  (setf (gethash (rcounter *world*) (rtable *world*)) registry)
  registry)

(defgeneric add-object (object store)
  (:documentation "Add OBJECT to STORE.")
  (:method ((v volume) (r registry))
    (setf (gethash (vcounter r) (vtable r)) v)
    v)
  (:method ((p pool) (r registry))
    (setf (gethash (ecounter r) (etable r)) p)
    p)
  (:method ((p pool) (v volume))
    (setf (gethash (id p) (table v)) p)
    p)
  (:method ((u unit) (r registry))
    (setf (gethash (ucounter r) (utable r)) u)
    u)
  (:method ((u unit) (v volume))
    (setf (gethash (id u) (table v)) u)
    u)
  (:method ((c cell) (v volume))
    (setf (gethash (id c) (ctable v)) c)
    c))

(defgeneric delete-object (object store)
  (:documentation "Remove OBJECT from STORE.")
  (:method ((p pool) (r registry))
    (let ((id (id p)))
      (remhash id (etable r))
      (decf (ecounter r))))
  (:method ((p pool) (v volume))
    (let ((id (id p)))
      (remhash id (table v))))
  (:method ((u unit) (r registry))
    (let ((id (id u)))
      (remhash id (utable r))
      (decf (ucounter r))))
  (:method ((u unit) (v volume))
    (let ((id (id u)))
      (remhash id (table v)))))

(defun make-pool (vid registry &optional prev next cells)
  "Create an instance of the pool class."
  (make-instance 'pool :vid vid :prev prev :next next :cells cells :registry registry))

(defun make-cell (volume value)
  "Create an instance of the cell class."
  (make-instance 'cell :value value :volume volume))

(defgeneric cells-values (object &key &allow-other-keys)
  (:documentation "Return the values contained inside POOL.")
  (:method ((p pool) &key expand)
    (mapcar #'(lambda (cell)
                (if expand
                    (if (blobp (value cell)) (source (value cell)) (value cell))
                    (value cell)))
            (cells p)))
  (:method ((l list) &key)
    (mapcar #'cells-values l)))

(defmethod prev ((o null)) "Return NIL on null pools." nil)
(defmethod next ((o null)) "Return NIL on null pools." nil)

(defun link-cells (cells)
  "Link CELLS to each other."
  (let ((start (first cells))
        (end (end cells))
        (cells (nil-wrap cells)))
    (loop :for cell-left :in cells
          :for cell-mid :in (rest cells)
          :for cell-right :in (rest (rest cells))
          :do (cond ((eql cell-mid start) (setf (next cell-mid) cell-right))
                    ((eql cell-mid end) (setf (prev cell-mid) cell-left))
                    (t (progn
                         (setf (prev cell-mid) cell-left)
                         (setf (next cell-mid) cell-right)))))))

(defun forge-cells (values volume)
  "Create cells from a list of values."
  (let ((cells (loop :for value :in values
                     :collect (make-cell volume value))))
    (loop :for cell :in cells
          :do (progn
                (when (header volume)
                  (destructuring-bind (header cells)
                      (equalize-lists (header volume) cells)
                    (loop :for h :in header
                          :for c :in cells
                          :do (when (or (cellp c) (not (empty-string-p c)))
                                (setf (head c) h)))))
                (add-object cell volume)))
    (link-cells cells)
    cells))

(defun forge-pool (volume registry &optional prev next values)
  "Create an pool under VOLUME in REGISTRY."
  (let* ((vid (vid volume))
         (cells (forge-cells values volume))
         (pool (make-pool vid registry prev next cells)))
    (add-object pool registry)
    (add-object pool volume)))

(defun make-volume (registry name &optional (prev -1) (next -1))
  "Create an instance of the volume class."
  (let ((rid (rid registry)))
    (make-instance 'volume :rid rid :name (string-upcase name)
                           :prev prev :next next
                           :registry registry)))

(defun forge-volume (registry name &optional (prev -1) (next -1))
  "Create a volume under registry RID in REGISTRY."
  (let ((volume (make-volume registry name prev next)))
    (add-object volume registry)))

(defun make-registry (&optional (name (make-registry-name)))
  "Create an instance of the registry class."
  (make-instance 'registry :rid (spawn-rcounter) :name (string-upcase name)))

(defgeneric spawn-registry (query)
  (:documentation "Return a new registry object then add it to the world, or return an existing one.")
  (:method ((query string))
    (let ((registry (find-registry query)))
      (if (not registry)
          (let ((r (make-registry query)))
            (add-registry r))
          registry)))
  (:method ((query integer))
    (let ((registry (find-registry query)))
      (if (not registry)
          (let ((r (make-registry)))
            (add-registry r))
          registry)))
  (:method ((query null))
    (let ((name (make-registry-name)))
      (spawn-registry name))))

(defun shallow-copy-registry (registry)
  "Create a shallow copy of REGISTRY."
  (with-slots (name ecounter etable ucounter utable vcounter vtable)
      registry
    (let* ((cname (cat name (genstring "/")))
           (copy (make-instance 'registry
                                 :rid (spawn-rcounter) :name cname
                                 :ecounter ecounter :etable etable
                                 :ucounter ucounter :utable utable
                                 :vcounter vcounter :vtable vtable)))
      (add-registry copy))))

(defun shallow-copy-volume (volume)
  "Create a shallow copy of VOLUME."
  (with-slots (rid name prev next)
      volume
    (let* ((registry (find-registry rid))
           (cname (cat name (genstring "/")))
           (copy (make-instance 'volume
                                :rid rid :name cname
                                :prev prev :next next
                                :registry (find-registry rid)
                                :link nil)))
      (add-object copy registry))))

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

(defun forge-pools (volume registry &optional feed)
  "Forge unlinked pools in VOLUME under REGISTRY. If FEED is true, use it to seed values."
  (if feed
      (loop :for item :in feed :do (forge-pool volume registry nil nil item))
      nil))

(defun find-next (pool volume)
  "Return the closest next pool in VOLUME from POOL."
  (let* ((id (id pool))
         (table (table volume))
         (pool (gethash (1+ id) table)))
    (if pool
        pool
        (let* ((pools (find-frames volume))
               (mem (member pool pools))
               (next (second mem)))
          (when next
            next)))))

(defun find-prev (pool volume)
  "Return the closest previous pool in VOLUME from POOL"
  (let* ((id (id pool))
         (table (table volume))
         (pool (gethash (1- id) table)))
    (if pool
        pool
        (let* ((pools (nreverse (find-frames volume)))
               (mem (member pool pools))
               (prev (second mem)))
          (when prev
            prev)))))

(defun link-frames (volume)
  "Link the frames in VOLUME to one another."
  (let* ((frames (find-frames volume))
         (cstart (id (first frames)))
         (cend (id (end frames))))
    (when frames
      (loop :for frame :in frames
            :for id = (id frame)
            :do (cond ((= id cstart)
                       (setf (next frame) (find-next frame volume)))
                      ((= id cend)
                       (setf (prev frame) (find-prev frame volume)))
                      (t (progn
                           (setf (prev frame) (find-frame (1- id) volume))
                           (setf (next frame) (find-frame (1+ id) volume))))))
      (setf (linkedp volume) t))))

(defgeneric find-registry (query)
  (:documentation "Return the registry which matches QUERY in WORLD.")
  (:method ((query integer))
    (let ((val (gethash query (rtable *world*))))
      (when val
        val)))
  (:method ((query string))
    (loop :for rid :being :the :hash-keys :in (rtable *world*)
          :for registry = (gethash rid (rtable *world*))
          :when (string-equal (string-upcase query) (name registry))
          :return registry))
  (:method ((n null))
    nil)
  (:method ((r registry))
    r))

(defun find-registries ()
  "Return all registries from the world."
  (loop :for r :being :the :hash-values :in (rtable *world*) :collect r))

(defgeneric find-volume (query registry)
  (:documentation "Return a volume which matches QUERY in REGISTRY.")
  (:method ((query integer) (r registry))
    (let ((val (gethash query (vtable r))))
      (when val
        val)))
  (:method ((query string) (r registry))
    (loop :for vid :being :the :hash-keys :in (vtable r)
          :for volume = (gethash vid (vtable r))
          :when (string-equal (string-upcase query) (name volume))
          :return volume))
  (:method ((query t) (n null))
    nil)
  (:method ((v volume) (r registry))
    v)
  (:method ((n null) (r registry))
    nil))

(defgeneric find-volumes (registry &key &allow-other-keys)
  (:documentation "Return all volumes from REGISTRY, except SKIP")
  (:method ((r registry) &key (skip #'false))
    (loop :for volume :being :the :hash-values :in (vtable r)
          :unless (funcall skip volume)
          :collect volume))
  (:method ((s string) &rest args)
    (apply #'find-volumes (find-registry s) args)))

(defgeneric find-frame (query store)
  (:documentation "Return an pool which matches QUERY in STORE.")
  (:method ((query integer) (r registry))
    (multiple-value-bind (value present)
        (gethash query (etable r))
      (when present
        value)))
  (:method ((query integer) (v volume))
    (multiple-value-bind (value present)
        (gethash query (table v))
      (when present
        value))))

(defun sort-frames (frames &key (key #'id))
  "Sort frames numerically."
  (sort frames #'< :key key))

(defun poolp (frame)
  "Return true if FRAME is of type POOL."
  (typep frame 'pool))

(defun unitp (frame)
  "Return true if FRAME is of type UNIT."
  (typep frame 'unit))

(defun pool-or-unit-p (frame)
  "Return true if FRAME is an pool or unit."
  (or (poolp frame)
      (unitp frame)))

(defun volume-start-p (pool)
  "Return true if pool is found at the start of a volume."
  (when (and (null (prev pool))
             (next pool))
    t))

(defun volume-end-p (pool)
  "Return true if pool is found at the end of a volume."
  (when (and (prev pool)
             (null (next pool)))
    t))

(defun volume-start (volume)
  "Return the first pool in VOLUME."
  (let ((frames (find-frames volume)))
    (loop :for frame :in frames
          :when (and (null (prev frame))
                     (next frame))
          :return frame)))

(defun volume-end (volume)
  "Return the last pool in VOLUME."
  (let ((frames (nreverse (find-frames volume))))
    (loop :for frame :in frames
          :when (and (prev frame)
                     (null (next frame)))
          :return frame)))

(defgeneric find-pools (store)
  (:documentation "Return pools from STORE.")
  (:method ((r registry))
    (loop :for pool :being :the :hash-values :in (etable r)
          :collect pool))
  (:method ((v volume))
    (loop :for frame :being :the :hash-values :in (table v)
          :when (poolp frame)
          :collect frame)))

(defgeneric table-count (store)
  (:documentation "Return the size of the hash table stored in STORE.")
  (:method ((v volume))
    (hash-table-count (table v))))

(defgeneric find-units (store)
  (:documentation "Return units from STORE.")
  (:method ((r registry))
    (loop :for unit :being :the :hash-values :in (utable r)
          :collect unit))
  (:method ((v volume))
    (loop :for frame :being :the :hash-values :in (table v)
          :when (unitp frame)
          :collect frame)))

(defgeneric find-frames (store &key &allow-other-keys)
  (:documentation "Return all frames from STORE.")
  (:method ((r registry) &key sort)
    (let* ((pools (find-pools r))
           (units (loop :for unit :being :the :hash-values :in (utable r)
                        :collect unit))
           (frames (nconc pools units)))
      (if sort
          (sort-frames frames)
          frames)))
  (:method ((v volume) &key sort)
    (loop :for frame :being :the :hash-values :in (table v)
          :collect frame :into frames
          :finally (return (if sort
                               (sort-frames frames)
                               frames)))))

(def (max-volume wall) (registry)
  "Return the biggest volume in REGISTRY. Size is determined by the number of frames."
  (first (sort (find-volumes registry) #'> :key #'(lambda (v) (hash-table-size (table v))))))

(defun forge-frame (&optional prev next left right buriedp)
  "Return a frame instance."
  (make-instance 'frame :prev prev :next next :left left :right right :buriedp buriedp))

(defun make-match (frame volume offset)
  "Return a MATCH object."
  (make-instance 'match :frame frame :volume volume :offset offset))

(defmethod value ((m match))
  "Return frame value from M."
  (cells-values (frame m)))

(defmethod id ((m match))
  "Retturn frame id from M."
  (id (frame m)))

(defun search-volume (query)
  "Return the first volume that that matches QUERY in all the registries."
  (let ((registries (find-registries)))
    (loop :for registry :in registries
          :for volume = (find-volume query registry)
          :when volume
          :return (values volume registry))))

(defun cellp (object)
  "Return true if OBJECT is a cell."
  (typep object 'cell))

(defun volumep (object)
  "Return true if OBJECT is a volume."
  (typep object 'volume))

(defun registryp (object)
  "Return true if OBJECT is a registry."
  (typep object 'registry))

(defgeneric delete-volume (volume registry)
  (:documentation "Delete VOLUME in REGISTRY.")
  (:method ((vid integer) (r registry))
    (remhash vid (vtable r)))
  (:method ((vname string) (r registry))
    (delete-volume (vid (find-volume vname r)) r))
  (:method ((v volume) (r registry))
    (delete-volume (vid v) r)))

(defgeneric delete-registry (registry)
  (:documentation "Delete REGISTRY in world.")
  (:method ((rid integer))
    (remhash rid (rtable *world*)))
  (:method ((rname string))
    (delete-registry (rid (find-registry rname))))
  (:method ((r registry))
    (delete-registry (rid (find-registry r)))))
