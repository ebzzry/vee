;;;; preload.lisp

(in-package #:muso/core)

(defmacro defselectors (limit)
  "Define selectors."
  `(progn
     ,@(loop :for n :from 0 :to limit
             :for name = (read-from-string (mof:cat "elt" (write-to-string n)))
             :collect `(defun ,name (list) (elt list ,n)))))
(defselectors 100)
