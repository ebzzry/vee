;;;; print.lisp

(in-package #:muso/core)

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