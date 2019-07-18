;;;; nlp.lisp

(in-package #:muso/core)

(defvar *levenshtein* nil
  "Whether to use the Levenshtein distance algorithm for computing sttring similarity.")

(defvar *levenshtein-threshold* 1
  "The maximum Levenstein distance to use.")

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

(defvar *mem-dict* nil
  "The memory dictionary to use for lemmatization.")

(defvar *base-path* (asdf:system-relative-pathname :muso nil)
  "The filesystem path upon which this system resides in.")

(defvar *default-dictionary* (uiop:subpathname *base-path* "dicts/wikt-dict.txt")
  "The default dictionary to use for lemmatization.")

(defun initialize-dictionary ()
  "Set an appropriate value for the lemmatization dictionary."
  (setf *mem-dict* (nlp.lexics:load-mem-dict *default-dictionary*)))

(defun stem-word (word)
  "Return a stemmed version of WORD."
  (nlp.lexics:stem nlp.lexics:<porter-stemmer> word))

(defun lemmatize-word (word)
  "Return a lemmatized version of WORD."
  (nlp.lexics:lemmatize *mem-dict* word))
