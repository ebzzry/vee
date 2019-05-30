;;;; classes.lisp

(in-package #:muso/core)

;;; hash table:
;;; key: data id
;;; val: data instance

(defclass data ()
  ((id :initarg :id
       :initform -1
       :reader id)
   (val :initarg :val
        :initform nil
        :reader val))
  (:documentation ""))

(defclass entry (data)
  ((prev :initarg prev
         :initform nil
         :reader prev)
   (curr :initarg curr
         :initform nil
         :reader curr)
   (next :initarg next
         :initform nil
         :reader next))
  (:documentation "Points to entries inside a column."))

(defclass column ()
  ((left :initarg left
         :initform nil
         :reader left)
   (mid :initarg mid
        :initform nil
        :reader mid)
   (right :initarg right
          :initform nil
          :reader right))
  (:documentation ""))
