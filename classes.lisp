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
   (vcounter :initarg :vcounter
             :initform *initial-vcounter*
             :accessor vcounter
             :documentation "The volume counter")
   (vtable :initarg :vtable
           :initform (make-hash-table)
           :accessor vtable
           :documentation "The volume table")
   (xid :initarg :xid
        :initform 0
        :accessor xid
        :documentation "The RID of the void counterpart of a table")
   (control :initarg :control
            :initform nil
            :accessor control
            :documentation "Whether a volume is a control value or not"))
  (:documentation "Tables and counters about entries and volumes. This class can be instantiated many times to contain different registries. Different registries can mean different dataset comparisons"))

(defclass volume ()
  ((rid :initarg :rid
        :initform nil
        :reader rid
        :documentation "The registry ID to which a volume belongs to")
   (vid :initarg :vid
        :initform nil
        :accessor vid
        :documentation "The unique numeric ID of a volume in a registry")
   (name :initarg :name
         :initform ""
         :reader name
         :documentation "The name of a volume")
   (header :initarg :header
           :initform ()
           :accessor header
           :documentation "The header of a data source")
   (table :initarg :table
          :initform (make-hash-table)
          :accessor table
          :documentation "The table for entry and unit indexing")
   (prev :initarg :prev
         :initform nil
         :accessor prev
         :documentation "The previous volume")
   (next :initarg :next
         :initform nil
         :accessor next
         :documentation "The next volume")
   (linkedp :initarg :linkedp
            :initform nil
            :accessor linkedp
            :documentation "Flag to indicate whether a volume is linked")
   (fcounter :initarg :fcounter
             :initform *initial-fcounter*
             :accessor fcounter
             :documentation "The field counter")
   (ftable :initarg :ftable
           :initform (make-hash-table)
           :accessor ftable
           :documentation "The volume table"))
  (:documentation "Pointer class for the entries. It may also contain links to other volumes inside a registry"))

(defclass field ()
  ((id :initarg :id
       :initform -1
       :accessor id
       :documentation "The unique numeric ID of a field in a volume")
   (head :initarg :head
         :initform nil
         :accessor head
         :documentation "The header field to which this field belongs to")
   (prev :initarg :prev
         :initform nil
         :accessor prev
         :documentation "The previous field in a record")
   (next :initarg :next
         :initform nil
         :accessor next
         :documentation "The next field in a record")
   (value :initarg :value
          :initform ""
          :reader value
          :documentation "The data value of a field")))

(defclass record ()
  ((vid :initarg :vid
        :initform nil
        :reader vid
        :documentation "The volume ID to which a record belongs to")
   (prev :initarg :prev
         :initform nil
         :accessor prev
         :documentation "The ID of the previous record in a volume")
   (next :initarg :next
         :initform nil
         :accessor next
         :documentation "The ID of the next record in a volume")
   ;; Note: is this for linear bindings?
   (left :initarg :left
         :initform nil
         :accessor left
         :documentation "The record on the left")
   (right :initarg :right
          :initform nil
          :accessor right
          :documentation "The record on the right")
   (buriedp :initarg :buriedp
            :initform nil
            :accessor buriedp
            :documentation "Whether a record is buried or not"))
  (:documentation "An empty container which links to other records"))

(defclass entry (record)
  ((id :initarg :id
       :initform -1
       :reader id
       :documentation "The unique numeric ID of an entry in a registry")
   (fields :initarg :fields
           :initform ()
           :accessor fields
           :documentation "The data fields of an entry")
   (matches :initarg :matches
            :initform ()
            :accessor matches
            :documentation "Structure for the non-linear matches"))
  (:documentation "A record that contains a value"))

(defclass unit (record)
  ((id :initarg :id
       :initform -1
       :reader id
       :documentation "The unique numeric ID of a unit in a registry"))
  (:documentation "An empty record"))

(defclass match ()
  ((record :initarg :record
           :initform nil
           :accessor record
           :documentation "The matching record")
   ;; Note: can this be easily inferred from the record?
   (volume :initarg :volume
           :initform nil
           :accessor volume
           :documentation "The matching volume")
   (offset :initarg :offset
           :initarg nil
           :accessor offset
           :documentation "The index of a match relative to a volume"))
  (:documentation "Information about matching records across volumes in a registry"))
