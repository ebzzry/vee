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
