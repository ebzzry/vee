;;;; classes.lisp

(in-package #:muso/core)

(defclass registry ()
  ((counter :initarg :counter
            :initform *initial-counter*
            :accessor counter
            :documentation "The global entry counter.")
   (table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The global entry table.")
   (ccounter :initarg :ccounter
             :initform *initial-ccounter*
             :accessor ccounter
             :documentation "The global column counter")
   (ctable :initarg :ctable
           :initform (make-hash-table)
           :accessor ctable
           :documentation "The global column table."))
  (:documentation "The global registry."))

(defclass column ()
  ((cid :initarg :cid
        :initform nil
        :reader cid
        :documentation "The numeric ID of a column.")
   (cstart :initarg :cstart
           :initform -1
           :reader cstart
           :documentation "The starting number for a column, which corresponds to an entry ID.")
   (cend :initarg :cend
         :initform -1
         :reader cend
         :documentation "The end number for a column, which corresponds to an entry ID.")
   (cleft :initarg :cleft
          :initform -1
          :reader cleft
          :documentation "The column to the cleft of the current one.")
   (cright :initarg :cright
           :initform -1
           :reader cright
           :documentation "The column to the cright of the current one."))
  (:documentation "Links between columns. An instance of ‘feed’."))

(defclass entry ()
  ((cid :initarg :cid
        :initform nil
        :reader cid
        :documentation "The column ID to which an entry belongs to.")
   (id :initarg :id
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
         :documentation "The next entry in a column."))
  (:documentation "Entries relative to a column. An instance of ‘item’."))
