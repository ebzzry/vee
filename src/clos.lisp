;;;; clos.lisp

(in-package #:ujo/core)

(defmethod initialize-instance :after ((p pool) &key registry)
  "Initialize pool P in REGISTRY."
  (let ((counter (spawn-ecounter registry)))
    (with-slots (id) p
      (setf id counter))))

(defmethod initialize-instance :after ((f node) &key volume)
  "Initialize node F in VOLUME."
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

(defmethod print-object ((p pool) stream)
  (print-unreadable-object (p stream :type t)
    (with-slots (id) p
      (format stream "~A" id))))

(defmethod print-object ((u unit) stream)
  (print-unreadable-object (u stream :type t)
    (with-slots (id) u
      (format stream "~A" id))))

(defmethod print-object ((f node) stream)
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

(defmethod print-object ((m match) stream)
  (print-unreadable-object (m stream :type t)
    (with-slots (volume) m
      (format stream "~A" (vid volume)))))
