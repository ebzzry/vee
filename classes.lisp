;;;; classes.lisp

(in-package #:muso/core)

(defclass register ()
  ((counter :initarg :counter :initform *initial-counter* :accessor counter)
   (table :initarg :table :initform (make-hash-table :test #'equal) :accessor table))
  (:documentation "The global register."))

(defclass entry ()
  ((id :initarg :id :initform -1 :reader id)
   (prev :initarg :prev :initform nil :reader prev)
   (curr :initarg :curr :initform nil :reader curr)
   (next :initarg :next :initform nil :reader next)
   (column :initarg :column :initform nil :reader column))
  (:documentation "Data entries relative to a column."))

(defclass column ()
  ((left :initarg :left :initform nil :reader left)
   (right :initarg :right :initform nil :reader right))
  (:documentation "Links between columns."))
