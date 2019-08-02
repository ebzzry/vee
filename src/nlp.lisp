;;;; nlp.lisp

(in-package #:ujo/core)

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

(defun bury-duplicates (volume)
  "Bury the duplicate entries found in VOLUME."
  (loop :for entry :in (walk-down volume :skip #'unitp)
        :for matches = (gethash '(0) (matches entry))
        :when (> (length matches) 1)
        :do (loop :for entry :in (rest matches) :do (bury entry))))

(defun blobp (object)
  "Return true if OBJECT is a BLOB."
  (when (typep object 'blob)
    t))

(defun make-blob (field)
  "Return a BLOB instance from field data."
  (if (blobp (value field))
      (value field)
      (let ((text (filter-text (value field))))
        (make-instance 'blob :fid (id field) :value text :source (value field)))))

(defun blob-convert-fields (volume constraints)
  "Replace the fields in VOLUME specified by CONSTRAINT, to BLOB objects."
  (let ((constraints (ensure-list constraints)))
    (loop :for constraint :in constraints
          :do (loop :for entry :in (walk-down volume :skip #'unitp)
                    :for field = (first (apply-constraints entry constraint))
                    :for value = (value field)
                    :do (setf (value field) (make-blob field))))))

(defun blob-equal-p (field-1 field-2 &key (test #'string-equal))
  "Return true if two BLOB objects are considered equal to one another, by computing its Jaccard similarity."
  (let ((value-1 (value field-1))
        (value-2 (value field-2)))
    (>= (float (* (/ (length (intersection value-1 value-2 :test test))
                     (length (union value-1 value-2 :test test)))
                  100))
        *matching-threshold*)))

(defun unbury-duplicates (volume)
  "Unbury the duplicate entries found in VOLUME."
  (loop :for entry :in (walk-down volume :skip #'unitp)
        :for matches = (gethash '(0) (matches entry))
        :when (> (length matches) 1)
        :do (loop :for entry :in (rest matches) :do (unbury entry))))

(defun make-bow (text)
  "Return a BOW object from TEXT, creating and deleting temporary stores."
  (let* ((bow (make-instance 'bow :source text))
         (volume (import-flat-text text))
         (registry (find-registry (rid volume))))
    (bind-self volume)
    (bury-duplicates volume)
    (lparallel:pmapc #'(lambda (entry)
                         (setf (gethash (first (fields-values entry)) (table bow))
                               (length (gethash '(0) (matches entry)))))
                     (walk-down volume :skip #'unitp))
    (delete-volume volume registry)
    (delete-registry registry)
    bow))

(defun dump-bow (bow)
  "Print information about a BOW."
  (maphash #'(lambda (k v)
               (format t "~&~10S => ~S" k v ))
           (table bow)))

(defun bow-get (entry bow)
  "Retrieve BOW count from BOW under ENTRY."
  (gethash entry (table bow)))

(defun bow-count (item text)
  "Retrieve the number of times ITEM appears in TEXT."
  (let ((count (bow-get item (make-bow text))))
    (if count
        count
        0)))

;;; Note: find ways to use BOW for phrasal similarity
;;; Note: implement n-grams?
