;;;; classes.lisp

(in-package #:muso/core)

(defclass world ()
  ((rcounter :initarg :rcounter
             :initform *initial-rcounter*
             :accessor rcounter
             :documentation "The top-level registry counter")
   (rtable :initarg :rtable
           :initform (make-hash-table)
           :accessor rtable
           :documentation "The top-level collection of registries"))
  (:documentation "The top-level structure for registries. Take note that when the instance is bound to a special variable, the special variable can also point to other world instances"))

(defclass registry ()
  ((rid :initarg :rid
        :initform nil
        :reader rid
        :documentation "The numeric ID of a registry")
   (name :initarg :name
          :initform (error "Specify a registry name")
          :reader name
          :documentation "The name of a registry")
   (ecounter :initarg :ecounter
             :initform *initial-ecounter*
             :accessor ecounter
             :documentation "The entry counter")
   (etable :initarg :etable
           :initform (make-hash-table)
           :accessor etable
           :documentation "The entry table")
   (ucounter :initarg :ucounter
             :initform *initial-ucounter*
             :accessor ucounter
             :documentation "The entry counter")
   (utable :initarg :utable
           :initform (make-hash-table)
           :accessor utable
           :documentation "The entry table")
   (ccounter :initarg :ccounter
             :initform *initial-ccounter*
             :accessor ccounter
             :documentation "The column counter")
   (ctable :initarg :ctable
           :initform (make-hash-table)
           :accessor ctable
           :documentation "The column table")
   (vid :initarg :vid
        :initform 0
        :accessor vid
        :documentation "The RID of the void counterpart of a table")
   (control :initarg :control
            :initform nil
            :accessor control
            :documentation "Whether a column is a control value or not"))
  (:documentation "Tables and counters about entries and columns. This class can be instantiated many times to contain different registries. Different registries can mean different dataset comparisons"))

(defclass column ()
  ((rid :initarg :rid
        :initform nil
        :reader rid
        :documentation "The registry ID to which a column belongs to")
   (cid :initarg :cid
        :initform nil
        :accessor cid
        :documentation "The unique numeric ID of a column in a registry")
   (name :initarg :name
         :initform ""
         :reader name
         :documentation "The name of a column")
   (table :initarg :table
           :initform (make-hash-table)
           :accessor table
           :documentation "The table for entry and unit indexing")
   (prev :initarg :prev
         :initform -1
         :reader prev
         :documentation "The previous column")
   (next :initarg :next
         :initform -1
         :reader next
         :documentation "The next column"))
  (:documentation "Pointer class for the entries. It may also contain links to other columns inside a registry"))

(defclass record ()
  ((cid :initarg :cid
        :initform nil
        :reader cid
        :documentation "The column ID to which a record belongs to")
   (prev :initarg :prev
         :initform nil
         :accessor prev
         :documentation "The ID of the previous record in a column")
   (next :initarg :next
         :initform nil
         :accessor next
         :documentation "The ID of the next record in a column")
   (left :initarg :left
         :initform nil
         :accessor left
         :documentation "The record on the left")
   (right :initarg :right
          :initform nil
          :accessor right
          :documentation "The record on the right")
   (buried :initarg :buried
           :initform nil
           :accessor buried
           :documentation "Whether a record is buried or not"))
  (:documentation "An empty container which links to other records"))

(defclass entry (record)
  ((id :initarg :id
       :initform -1
       :reader id
       :documentation "The unique numeric ID of an entry in a registry")
   (value :initarg :value
          :initform nil
          :accessor value
          :documentation "The datum of an entry"))
  (:documentation "A record that contains a value"))

(defclass unit (record)
  ((id :initarg :id
       :initform -1
       :reader id
       :documentation "The unique numeric ID of a unit in a registry"))
  (:documentation "An empty record"))
