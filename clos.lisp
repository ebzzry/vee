;;;; clos.lisp

(in-package #:muso/core)

(defmethod initialize-instance :after ((e entry) &key registry)
  "Initialize entry E in REGISTRY."
  (let ((counter (spawn-ecounter registry)))
    (with-slots (id) e
      (setf id counter))))

(defmethod initialize-instance :after ((f field) &key volume)
  "Initialize field F in VOLUME."
  (let ((counter (spawn-fcounter volume)))
    (with-slots (id) f
      (setf id counter))))

(defmethod initialize-instance :after ((v volume) &key registry)
  "Initialize volume V in REGISTRY."
  (let ((last (find-volume (vcounter registry) registry))
        (counter (spawn-vcounter registry)))
    (with-slots (vid) v
      (setf vid counter))
    (when (> (vcounter registry) (1+ *initial-vcounter*))
      (setf (next last) v)
      (setf (prev v) last))))

(defmethod initialize-instance :after ((u unit) &key registry)
  "Initialize unit U in REGISTRY."
  (let ((counter (spawn-ucounter registry)))
    (with-slots (id) u
      (setf id counter))))

(defmethod print-object ((e entry) stream)
  (print-unreadable-object (e stream :type t)
    (with-slots (id) e
      (format stream "~A" id))))

(defmethod print-object ((u unit) stream)
  (print-unreadable-object (u stream :type t)
    (with-slots (id) u
      (format stream "~A" id))))

(defmethod print-object ((f field) stream)
  (print-unreadable-object (f stream :type t)
    (with-slots (id) f
      (format stream "~A" id))))

(defmethod print-object ((v volume) stream)
  (print-unreadable-object (v stream :type t)
    (with-slots (vid name) v
      (format stream "~A ~S" vid name))))

(defmethod print-object ((r registry) stream)
  (print-unreadable-object (r stream :type t)
    (with-slots (rid name) r
      (format stream "~A ~S" rid name))))

(defmethod print-object ((ht hash-table) stream)
  (print-unreadable-object (ht stream :type t)
    (format stream "~A ~A" (hash-table-test ht) (hash-table-count ht))))

(defmethod print-object ((m match) stream)
  (print-unreadable-object (m stream :type t)
    (with-slots (volume) m
      (format stream "~A" (vid volume)))))