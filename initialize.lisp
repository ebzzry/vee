;;;; initialize.lisp

(in-package #:muso/core)

(defmacro defselectors (limit)
  "Define selectors."
  `(progn
     ,@(loop :for n :from 0 :to limit
             :for name = (read-from-string (mof:cat "elt" (write-to-string n)))
             :collect `(defun ,name (list) (elt list ,n)))))
(defselectors 100)

(defun string-equal-p (string-1 string-2)
  "Return true if STRING-1 and STRING-2 are equal either by strict strict matching or by Levenshtein distance."
  (or (string-equal string-1 string-2)
      (and *levenshtein*
           (levenshtein-equal-p string-1 string-2))))

(defun field-equal-p (&rest args)
  "Return true if OBJECT-1 and OBJECT-2 are equal to one another."
  (cond ((every #'stringp args) (apply #'string-equal-p args))
        ((every #'blobp args) (apply #'blob-equal-p args))
        ((every #'volumep args) (apply #'simple-volume-matching-p args))
        ;; ((every #'volumep args) (apply #'extended-volume-matching-p args))
        (t (apply #'eql args))))

(defvar *field-test* #'field-equal-p
  "The test function that will be used to compare fields.")

(defun initialize-lparallel ()
  "Initialize the lparallel kernel and set the number of workers."
  (let ((cpus (cpus:get-number-of-processors)))
    (setf lparallel:*kernel* (lparallel:make-kernel cpus))))
