;;;; classes.lisp

(in-package #:ujo/core)

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
             :documentation "The pool counter")
   (etable :initarg :etable
           :initform (make-hash-table)
           :accessor etable
           :documentation "The pool table")
   (ucounter :initarg :ucounter
             :initform *initial-ucounter*
             :accessor ucounter
             :documentation "The unit counter")
   (utable :initarg :utable
           :initform (make-hash-table)
           :accessor utable
           :documentation "The pool table")
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
  (:documentation "Tables and counters about pools and volumes. This class can be instantiated many times to contain different registries. Different registries can mean different dataset comparisons"))

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
          :documentation "The table for pool and unit indexing")
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
   ;; Note: should these be moved to the pool level?
   (ccounter :initarg :ccounter
             :initform *initial-ccounter*
             :accessor ccounter
             :documentation "The cell counter")
   (ctable :initarg :ctable
           :initform (make-hash-table)
           :accessor ctable
           :documentation "The cell table")
   (left :initarg :left
         :initform nil
         :accessor left
         :documentation "The volume on the left.")
   (right :initarg :right
          :initform nil
          :accessor right
          :documentation "The volume on the right."))
  (:documentation "Pointer class for the pools. It may also contain links to other volumes inside a registry"))

(defclass frame ()
  ((vid :initarg :vid
        :initform nil
        :reader vid
        :documentation "The volume ID to which a frame belongs to")
   (prev :initarg :prev
         :initform nil
         :accessor prev
         :documentation "The ID of the previous frame in a volume")
   (next :initarg :next
         :initform nil
         :accessor next
         :documentation "The ID of the next frame in a volume")
   ;; Note: this can be used for linear bindings
   (left :initarg :left
         :initform nil
         :accessor left
         :documentation "The frame on the left")
   (right :initarg :right
          :initform nil
          :accessor right
          :documentation "The frame on the right")
   (buriedp :initarg :buriedp
            :initform nil
            :accessor buriedp
            :documentation "Whether a frame is buried or not"))
  (:documentation "An empty container which links to other frames"))

(defclass pool (frame)
  ((id :initarg :id
       :initform -1
       :reader id
       :documentation "The unique numeric ID of an pool in a registry")
   (cells :initarg :cells
          :initform ()
          :accessor cells
          :documentation "The data cells of an pool")
   (matches :initarg :matches
            :initform (make-hash-table :test #'equalp)
            :accessor matches
            :documentation "Structure for the non-linear matches"))
  (:documentation "A frame that contains a value"))

(defclass unit (frame)
  ((id :initarg :id
       :initform -1
       :reader id
       :documentation "The unique numeric ID of a unit in a registry"))
  (:documentation "An empty frame"))

(defclass cell ()
  ((id :initarg :id
       :initform -1
       :accessor id
       :documentation "The unique numeric ID of a cell in a frame.")
   (head :initarg :head
         :initform nil
         :accessor head
         :documentation "The header cell to which this cell belongs to")
   (prev :initarg :prev
         :initform nil
         :accessor prev
         :documentation "The previous cell in a frame")
   (next :initarg :next
         :initform nil
         :accessor next
         :documentation "The next cell in a frame")
   (value :initarg :value
          :initform ""
          :accessor value
          :documentation "The text or volume value of a cell")))

(defclass match ()
  ((frame :initarg :frame
          :initform nil
          :accessor frame
          :documentation "The matching frame")
   ;; Note: this can be easily inferred from the frame
   (volume :initarg :volume
           :initform nil
           :accessor volume
           :documentation "The matching volume")
   (offset :initarg :offset
           :initarg nil
           :accessor offset
           :documentation "The index of a match relative to a volume"))
  (:documentation "Information about matching frames across volumes in a registry"))

(defclass blob ()
  ((nid :initarg :nid
        :initform -1
        :accessor nid
        :documentation "The cell ID where this object belongs to")
   (value :initarg :value
          :initform nil
          :accessor value
          :documentation "The value of the processed text")
   (source :initarg :source
           :initform nil
           :accessor source
           :documentation "The original raw text of a cell"))
  (:documentation "The summary of a text body"))

(defclass bow ()
  ((source :initarg :source
           :initform nil
           :accessor source
           :documentation "The original raw text of the input.")
   (table :initarg :table
          :initform (make-hash-table :test #'equal)
          :accessor table
          :documentation "Table for bag-of-words, wherein the key is a word and the value is the occurence count."))
  (:documentation "Information about a text passage and its corresponding bag-of-words."))
