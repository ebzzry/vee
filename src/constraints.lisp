;;;; constraints.lisp

(in-package #:ujo/core)

(defun point (origin volume)
  "Return starting point based on the type of origin."
  (etypecase origin
    (function (funcall origin volume))
    (pool origin)))

(defun walk-up (volume &key (origin #'volume-end) (fn #'identity))
  "Return frames from VOLUME starting from frame ORIGIN applying FN to each frame."
  (when (linkedp volume)
    (loop :for pool = (point origin volume) :then (prev pool)
          :while pool
          :collect (funcall fn pool))))

(defun plus (pool count)
  "Return a new pool based from POOL and AMOUNT based on volume addition."
  (cond ((zerop count) pool)
        (t (plus (next pool) (1- count)))))

(defun range (pool count)
  "Return pools starting from POOL with COUNT steps."
  (labels ((fn (pool count acc)
               (cond ((zerop count) (nreverse acc))
                     ((null pool) nil)
                     (t (fn (next pool) (1- count) (cons pool acc))))))
    (fn pool count nil)))

(defun pool-limit-p (volume pool destination &key test)
  "Return true if POOL reached the limit of, or the concept of, DESTINATION."
  (and pool (etypecase destination
              (function (funcall test (id pool) (id (funcall destination volume))))
              (pool (funcall test (id pool) (id destination)))
              (t nil))))

(defun walk-down (volume &key (origin #'volume-start)
                              (destination #'volume-end)
                              (skip #'mof:false)
                              (fn #'identity))
  "Return frames from VOLUME starting from frame ORIGIN applying FN to each frame."
  (when (linkedp volume)
    (loop :for pool = (point origin volume) :then (next pool)
          :while (pool-limit-p volume pool destination :test #'<=)
          :unless (funcall skip pool)
          :collect (funcall fn pool))))

(defun walk-left (volume)
  "Return frames starting from VOLUME across all the other volumes, going left."
  (declare (ignorable volume))
  nil)

(defun walk-right (volume)
  "Return frames starting from VOLUME across all the other volumes, going right."
  (declare (ignorable volume))
  nil)

(defun dispatch-cells (pool constraints &key (type :head) (test *cell-test*))
  "Return cells from POOL that satisfy CONSTRAINTS. The result is designed to be not orthogonal to the length of CONSTRAINTS because header-specifiers can exist multiple times in a header."
  (let ((constraints (ensure-list constraints))
        (cells (cells pool))
        (func (intern (string type))))
    (ecase type
      ((:index)
       (loop :for constraint :in constraints
             :collect (nth constraint cells)))
      ((:head :value)
       (loop :for constraint :in constraints
             :nconc (loop :for cell :in cells
                          :when (funcall test constraint (funcall func cell))
                          :collect cell))))))

(defgeneric apply-constraints (object constraints &key &allow-other-keys)
  (:documentation "Return cells from POOL that satisfy CONSTRAINTS, where CONSTRAINTS is a list of header-specifiers or integer indexes.")
  (:method ((o pool) constraints &key (type :head) (test *cell-test*))
    (let ((constraints (ensure-list constraints)))
      (loop :for constraint :in constraints
            :nconc (etypecase constraint
                     (string (dispatch-cells o (list constraint) :type type :test test))
                     (integer (list (nth constraint (cells o))))))))
  (:method ((o list) constraints &key (fallback ""))
    (let ((constraints (ensure-list constraints)))
      (loop :for constraint :in constraints
            :collect (etypecase constraint
                       (function (funcall constraint o))
                       (integer (nth constraint o))
                       (t fallback)))))
  (:method ((o volume) constraints &key (test *cell-test*) merge)
    (let* ((constraints (ensure-list constraints))
           (pools (loop :for pool :in (walk-down o :skip #'unitp)
                        :collect (apply-constraints pool constraints :type :head :test test))))
      (if merge
          (reduce #'nconc pools)
          pools))))

(defun resolve-constraints (pool constraints)
  "Return the value of constraints application. This is primarily used to extract the values of cells, whether it is a BLOB or VOLUME object."
  (mapcar #'value (apply-constraints pool constraints)))

(defgeneric everyp (object pool constraints &key &allow-other-keys)
  (:documentation "Return true if OBJECT matches the applied version of POOL. TEST should invoke the correct function to check for equality.")
  (:method ((o list) (p pool) constraints &key (test *cell-test*))
    (every test
           (apply-constraints o constraints)
           (resolve-constraints p constraints)))
  (:method ((o pool) (p pool) constraints &key (test *cell-test*))
    (every test
           (resolve-constraints o constraints)
           (resolve-constraints p constraints))))

(defun find-other-volumes (volume registry)
  "Find the other volumes in REGISTRY."
  (let ((vid (vid volume)))
    (find-volumes registry :skip #'(lambda (v) (= vid (vid v))))))

(defun find-other-volume (volume)
  "Return the other volume in the same registry."
  (let* ((registry (find-registry (rid volume)))
         (volumes (find-other-volumes volume registry)))
    (if (= (length volumes) 1)
        (first volumes)
        -1)))

(defgeneric find-similar-pools (store pool constraints &key &allow-other-keys)
  (:documentation "Return pools from VOLUME that satisfy CONSTRAINTS as applied to POOL, where CONSTRAINTS is a list of header-specifiers to match against. The function specified by TEST will determine equality.

    (FIND-SIMILAR-POOLS VOLUME POOL '(\"country\"))

returns pools from VOLUME wherein the 'country' cell is the same as that of POOL.

    (FIND-SIMILAR-POOLS VOLUME POOL '(\"country\" \"gender\"))

returns pools from VOLUME wherein the 'country' and 'gender' cells are the same as those of POOL.

This generic function is mainly used for matching against data that is already inside a registry.
")
  (:method ((v volume) (p pool) constraints &key (origin #'volume-start)
                                                 (test *cell-test*)
                                                 pool-exclusive)
    (when (linkedp v)
      (let ((frames (walk-down v :origin origin
                                 :skip #'(lambda (frm)
                                           (or (unitp frm) (when pool-exclusive (eql frm p)))))))
        (loop :for frame :in frames
              :when (everyp p frame constraints :test test)
              :collect frame))))
  (:method ((r registry) (p pool) constraints &key (origin #'volume-start)
                                                   (test *cell-test*)
                                                   volume-exclusive
                                                   pool-exclusive)
    (let ((volumes (if volume-exclusive
                       (find-other-volumes (find-volume (vid p) r) r)
                       (find-volumes r))))
      (loop :for volume :in volumes
            :nconc (find-similar-pools volume p constraints
                                       :origin origin
                                       :test test
                                       :pool-exclusive pool-exclusive)))))

(defgeneric find-matching-pools (store specifiers &key &allow-other-keys)
  (:documentation "Return pools from STORE that SPECIFIERS, where SPECIFIERS are lists of header-specifier and header-value lists, or a single item of such type. The function specified by TEST will determine equality.

    (FIND-MATCHING-POOLS STORE '((\"email\" \"sstanleynh@wp.com\") (\"country\" \"Philippines\")) :TYPE :OR)

returns pools that have set either the email to sstanleynh@wp.com or the country to Philippines.

    (FIND-MATCHING-POOLS STORE '((\"email\" \"pwagner1x@gravatar.com\") (\"country\" \"Italy\")) :TYPE :AND)

returns pools that have set both the email to pwagner1x@gravatar.com and the country to Italy.

This generic function is mainly used for matching againstn data that is provided by the user, which may or may not exist inside a registry.
")
  (:method ((v volume) specifiers &key (type :or) (test *cell-test*))
    (let ((pools (walk-down v :skip #'unitp))
          (specifiers (if (and (every-string-p specifiers)
                               (not (every-list-p specifiers)))
                          (list specifiers)
                          specifiers)))
      (loop :for pool :in pools
            :nconc
               (ecase type
                 (:or (lparallel:premove
                       nil
                       (lparallel:pmapcan #'(lambda (cell)
                                              (lparallel:pmapcar
                                               #'(lambda (specifier)
                                                   (when (destructuring-bind (head value) specifier
                                                           (and (funcall test (head cell) head)
                                                                (funcall test (value cell) value)))
                                                     pool))
                                               specifiers))
                                          (cells pool))))
                 (:and (destructuring-bind (heads values)
                           (apply #'mapcar #'list specifiers)
                         (let ((cells (apply-constraints pool heads :type :head)))
                           (when (every test (mapcar #'value cells) values)
                             (list pool)))))))))
  (:method ((r registry) specifiers &rest args)
    (let ((volumes (find-volumes r)))
      (lparallel:pmapcan #'(lambda (volume)
                             (apply #'find-matching-pools volume specifiers args))
                         volumes))))

(defun headp (head volume)
  "Return true if HEAD is a header in VOLUME."
  (when (member head (header volume) :test #'string-equal)
    t))

(defun collect-operators (terms volume)
  "Return the operators from TERMS. Operators are constraints that are not part of a volume header."
  (loop :for term :in terms
        :unless (headp term volume)
        :collect term))

(defun flatten-constraints (terms volume)
  "Return a list containing only items that match the header of VOLUME. This strips the meta characters from the constraints."
  (loop :for term :in terms
        :collect (if (headp term volume) term (strip-meta-char term))))

(defun dispatch-operators (volume operators)
  "Process the operators on VOLUME, handling ! and @ meta characters."
  (lparallel:pmapc #'(lambda (operator)
                       (cond ((string-end-p operator #\!)
                              (blob-convert-cells volume (strip-end-char operator)))
                             ((string-end-p operator #\@)
                              (volume-convert-cells volume (strip-end-char operator)))
                             (t nil)))
                   operators))

(defun find-duplicates (volume terms)
  "Return a list of pools that have similar properties according to TERMS, where each term is either a constraint or a string that constains a meta-character that indicates additional operations to be done beforehand."
  (let ((operators (collect-operators terms volume))
        (constraints (flatten-constraints terms volume)))
    (dispatch-operators volume operators)
    (let ((results (loop :for pool :in (walk-down volume :skip #'unitp)
                         :for offset = (volume-start volume) :then (next offset)
                         :while offset
                         :nconc (find-similar-pools volume pool constraints
                                                    :origin offset :pool-exclusive t))))
      (remove-duplicates results))))

(defun expunge-duplicates (volume terms)
  "Remove duplicate pools in VOLUME under TERMS, creating void registries as necessary."
  (let ((duplicates (with-levenshtein (find-duplicates volume terms)))
        (registry (find-registry (rid volume))))
    (lparallel:pmapc #'(lambda (pool) (banish pool registry))
                     duplicates)))
