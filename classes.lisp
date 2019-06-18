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
   (rname :initarg :rname
          :initform (error "Specify a registry name")
          :reader rname
          :documentation "The name of a registry")
   (ecounter :initarg :ecounter
             :initform *initial-ecounter*
             :accessor ecounter
             :documentation "The entry counter")
   (etable :initarg :etable
           :initform (make-hash-table)
           :accessor etable
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
   (cname :initarg :cname
          :initform ""
          :reader cname
          :documentation "The name of a column")
   (etable :initarg :etable
           :initform (make-hash-table)
           :accessor etable
           :documentation "The table for entry indexing")
   (cprev :initarg :cprev
          :initform -1
          :reader cprev
          :documentation "The previous column")
   (cnext :initarg :cnext
           :initform -1
           :reader cnext
           :documentation "The next column"))
  (:documentation "Pointer class for the entries. It may also contain links to other columns inside a registry"))

(defclass entry ()
  ((cid :initarg :cid
        :initform nil
        :reader cid
        :documentation "The column ID to which an entry belongs to")
   (id :initarg :id
       :initform -1
       :reader id
       :documentation "The unique numeric ID of an entry in a registry")
   (prev :initarg :prev
         :initform nil
         :accessor prev
         :documentation "The ID of the previous entry in a column")
   (next :initarg :next
         :initform nil
         :accessor next
         :documentation "The ID of the next entry in a column")
   (value :initarg :value
          :initform nil
          :accessor value
          :documentation "The datum of an entry")
   (buried :initarg :buried
           :initform nil
           :accessor buried
           :documentation "Whether an entry is buried or not"))
  (:documentation "Information instantiated from feeds"))

(defclass cluster ()
  ((uid :initarg :uid
        :initform nil
        :reader uid
        :documentation "The unique numeric ID of a cluster in a registry")
   (uvalue :initarg :uvalue
           :initform nil
           :reader uvalue
           :documentation "The contents of a cluster"))
  (:documentation "Information about selected fields from an entry"))
