;;;; classes.lisp

(in-package #:muso)

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
  (:documentation "The class to contain information about entries."))

(defclass connection (entry)
  ((prev :initarg :prev
         :initform nil
         :accessor prev)
   (curr :initarg :curr
         :initform nil
         :accessor curr)
   (next :initarg :next
         :initform nil
         :accessor next))
  (:documentation "The class to contain links between entries."))

(defclass column (entry)
  ((left :initarg left
         :initform nil
         :accessor left)
   (mid :initarg mid
        :initform nil
        :accessor mid)
   (right :initarg right
          :initform nil
          :accessor right))
  (:documentation "The class to contain links between columns"))
