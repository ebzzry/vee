;;;; classes.lisp

(uiop:define-package #:muso/classes
    (:use #:cl
          #:trivia
          #:muso/globals))

(in-package #:muso/classes)

(defclass entry ()
  ((id :initarg :id
       :initform -1
       :accessor id)
   (text :initarg :text
         :initform nil
         :accessor text)
   (pos :initarg :pos
        :initform nil
        :accessor pos)
   (dep :initarg :dep
        :initform nil
        :accessor dep))
  (:documentation "Information about entries."))

(defclass column (entry)
  ((prev :initarg prev
         :initform nil
         :accessor prev)
   (curr :initarg curr
        :initform nil
        :accessor curr)
   (next :initarg next
          :initform nil
          :accessor next))
  (:documentation "Linkage between entries."))

(defclass connection (entry)
  ((left :initarg :left
         :initform nil
         :accessor left)
   (mid :initarg :mid
         :initform nil
         :accessor mid)
   (right :initarg :right
         :initform nil
         :accessor right))
  (:documentation "Linkage between columns."))

(defclass grouping (entry)
  ((ldata :initarg ldata
          :initform *empty-entry*
          :accessor ldata)
   (rdata :initarg rdata
          :initform *empty-entry*
          :initarg rdata))
  (:documentation "Horizontal set of entries."))
