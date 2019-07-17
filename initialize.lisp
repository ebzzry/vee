;;;; initialize.lisp

(in-package #:muso/core)

(defvar *default-delimiter* #\,
  "The default delimiter used to separate fields.")

(defvar *threshold* 5
  "The amount of lines to consider when trying to perform resoultion, backwards and forwards.")

(defvar *join-limit* 2
  "The amount of lines forward to join.")

(defvar *pad* nil
  "Padding data for items.")

(defvar *similarity-threshold* 40.0
  "The minimum amount of similarity between sets so that they can be considered similar.")

(defvar *initial-rcounter* 100
  "The initial registry counter value")

(defvar *initial-vcounter* 1000
  "The initial volume counter value")

(defvar *initial-ecounter* 10000
  "The initial entry counter value")

(defvar *initial-ucounter* (- *initial-ecounter*)
  "The initial unit counter value")

(defvar *initial-fcounter* 1000000
  "The initial field counter value.")

(defvar *selector-limit* 1000
  "The amount of ELT-selectors to define.")

(defparameter *world* nil
  "The top-level structure which contains all the registries.")

(defvar *default-constraints* '(0)
  "The default list of functions that are used for specifying clusters.")

(defvar *matching-threshold* 75.0
  "A percentage value of what is considered matching volumes.")

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

(defun levenshtein (a b)
  "Return the Levenshtein distance between A and B."
  (let* ((la (length a))
         (lb (length b))
         (rec (make-array (list (1+ la) (1+ lb)) :initial-element nil)))
    (labels ((fn (x y)
               (cond ((zerop x) y)
                     ((zerop y) x)
                     ((aref rec x y) (aref rec x y))
                     (t (setf (aref rec x y)
                              (+ (if (char= (char a (- la x)) (char b (- lb y))) 0 1)
                                 (min (fn (1- x) y)
                                      (fn x (1- y))
                                      (fn (1- x) (1- y)))))))))
      (fn la lb))))

(defvar *levenshtein* nil
  "Whether to use the Levenshtein distance algorithm for computing sttring similarity.")

(defvar *levenshtein-threshold* 1
  "The maximum Levenstein distance to use.")

(defun levenshtein-equal-p (string-1 string-2)
  "Return true if STRING-1 and STRING-2 are equal within a certain Levenshtein distance."
  (let ((length-1 (length string-1))
        (length-2 (length string-2)))
    (when (= length-1 length-2)
      (<= (levenshtein string-1 string-2) *levenshtein-threshold*))))

(defmacro with-levenshtein (&body body)
  "Run BODY while enabling the Levenshtein distance algorithm."
  `(let ((*levenshtein* t))
     (progn ,@body)))

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
(initialize-lparallel)
