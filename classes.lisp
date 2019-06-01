;;;; classes.lisp

(in-package #:muso/core)

;;; TODO: design the inheritance

(defclass world ()
  ((rcounter :initarg :rcounter
             :initform *initial-rcounter*
             :accessor rcounter
             :documentation "The top-level registry counter.")
   (rtable :initarg :rtable
           :initform (make-hash-table)
           :accessor rtable
           :documentation "The top-level collection of registries."))
  (:documentation "The top-level structure for registries."))

(defclass registry ()
  ((rid :initarg :rid
        :initform nil
        :reader rid
        :documentation "The numeric ID of a registry.")
   (rname :initarg :rname
          :initform (string (gensym "REGISTRY"))
          :reader rname
          :documentation "The name of a registry.")
   (counter :initarg :counter
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
  (:documentation "Information about entries and columns. This class can be instantiated many times to contain different registries. Different registries can mean different data dataset comparisons."))

(defclass column ()
  ((rid :initarg :rid
        :initform nil
        :reader rid
        :documentation "The registry ID to which a column belongs to..")
   (cid :initarg :cid
        :initform nil
        :reader cid
        :documentation "The numeric ID of a column.")
   (cname :initarg :cname
          :initform ""
          :reader cname
          :documentation "The name of a column.")
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
          :documentation "The column to the left of the current one.")
   (cright :initarg :cright
           :initform -1
           :reader cright
           :documentation "The column to the right of the current one."))
  (:documentation "Information about the range of entries that it contains. It may also contain links to other columns inside a registry."))

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
  (:documentation "Information about individual entries usually coming in from a delimited source."))
