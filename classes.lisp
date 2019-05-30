;;;; classes.lisp

(in-package #:muso/core)

(defclass registry ()
  ((counter :initarg :counter
            :initform *initial-counter*
            :accessor counter
            :documentation "The global registry counter.")
   (table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The global table."))
  (:documentation "The global registry."))

(defclass entry ()
  ((id :initarg :id
       :initform -1
       :reader id
       :documentation "The numeric ID of an entry.")
   (prev :initarg :prev
         :initform nil
         :reader prev
         :documentation "The previous entry in a column.")
   (curr :initarg :curr
         :initform nil
         :reader curr
         :documentation "The current entry in a column.")
   (next :initarg :next
         :initform nil
         :reader next
         :documentation "The next entry in a column.")
   (column :initarg :column
           :initform -1
           :reader column
           :documentation "The ID of the column wherein an entry appears."))
  (:documentation "Entries relative to a column."))

(defclass column (entry)
  ((cid :initarg :cid
        :initform nil
        :reader cid
        :documentation "The numeric ID of a column.")
   (left :initarg :left
         :initform nil
         :reader left
         :documentation "The column to the left of the current one.")
   (right :initarg :right
          :initform nil
          :reader right
          :documentation "The column to the right of the current one."))
  (:documentation "Links between columns."))
