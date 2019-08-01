;;;; nlp.lisp

(in-package #:ujo/core)

(defvar *levenshtein* nil
  "Whether to use the Levenshtein distance algorithm for computing string similarity.")

(defvar *levenshtein-threshold* 1
  "The maximum Levenstein distance to use.")

(defvar *mem-dict* nil
  "The memory dictionary to use for lemmatization.")

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

(defun initialize-dictionary ()
  "Set an appropriate value for the lemmatization dictionary."
  (setf *mem-dict* (nlp.lexics:load-mem-dict (nlp.util:lang-file :en "wikt-dict.txt"))))

(defun stem-word (word)
  "Return the stemmed version of WORD."
  (nlp.lexics:stem nlp.lexics:<porter-stemmer> word))

(defun lemmatize-word (word)
  "Return the lemmatized version of WORD."
  (nlp.lexics:lemmatize *mem-dict* word))

(defun normalize-word (word)
  "Return a normalized version of WORD."
  (stem-word (lemmatize-word word)))

(defun normalize-words (words)
  "Return a normalized version of WORDS."
  (loop :for word :in words
        :collect (normalize-word word)))
