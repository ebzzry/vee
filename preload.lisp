;;;; preload.lisp

(in-package #:muso/core)

(defmacro defselectors (limit)
  "Define selectors."
  `(progn
     ,@(loop :for n :from 0 :to limit
             :for name = (read-from-string (mof:cat "elt" (write-to-string n)))
             :collect `(defun ,name (list) (elt list ,n)))))
;;; Note: this may be a performance bottleneck
(defselectors 100)

(defmacro build-selectors (indexes)
  "Define selectors."
  `(progn
     ,@(loop :for index :in indexes
             :for name = (read-from-string (mof:cat "select-" (write-to-string index)))
             :collect `(defun ,name (list) (elt list ,(1- index))))))

(defun string-volume-equal-p (&rest args)
  "Return true if OBJECT-1 and OBJECT-2 are equal to one another."
  (cond ((every #'stringp args) (apply #'string-equal args))
        ((every #'volumep args) (apply #'simple-volume-matching-p args))
        (t (error "unknown types"))))
