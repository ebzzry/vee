;;;; initialize.lisp

(in-package #:muso/core)

;;; Note: should entry-linking be done here, instead?
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
