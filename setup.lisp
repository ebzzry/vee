;;;; setup.lisp

(in-package #:veda/core)

(defmacro defselectors (prefix count)
  "Define list selectors prefixed with PREFIX that will act as sequence accessors."
  `(progn
     ,@(loop :for n :from 0 :to count
             :for name = (read-from-string (cat prefix (write-to-string n)))
             :collect `(defun ,name (list) (elt list ,n)))))
(defselectors "elt" 100)

(defun string-equal-p (string1 string2)
  "Return true if STRING1 and STRING2 are equal either by strict strict matching or by Levenshtein distance."
  (or (string-equal string1 string2)
      (and *levenshtein*
           (levenshtein-equal-p string1 string2))))

(defun cell-equal-p (&rest args)
  "Return true if OBJECT1 and OBJECT2 are equal to one another."
  (cond ((every #'stringp args) (apply #'string-equal-p args))
        ((every #'blobp args) (apply #'blob-matching-p args))
        ;; ((every #'volumep args) (apply #'simple-volume-matching-p args))
        ((every #'volumep args) (apply #'extended-volume-matching-p args))
        (t (apply #'eql args))))

(defvar *cell-test* #'cell-equal-p
  "The test function that will be used to compare cells.")

(defun initialize-lparallel ()
  "Initialize the lparallel kernel and set the number of workers."
  (let ((cpus (cpus:get-number-of-processors)))
    (setf lparallel:*kernel* (lparallel:make-kernel cpus))))
