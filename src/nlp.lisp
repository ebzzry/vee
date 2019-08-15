;;;; nlp.lisp

(in-package #:ujo/core)

(defun levenshtein (a b)
  "Return the Levenshtein distance between A and B."
  (let* ((la (length a))
         (lb (length b))
         (frm (make-array (list (1+ la) (1+ lb)) :initial-element nil)))
    (labels ((fn (x y)
                 (cond ((zerop x) y)
                       ((zerop y) x)
                       ((aref frm x y) (aref frm x y))
                       (t (setf (aref frm x y)
                                (+ (if (char= (char a (- la x)) (char b (- lb y))) 0 1)
                                   (min (fn (1- x) y)
                                        (fn x (1- y))
                                        (fn (1- x) (1- y)))))))))
      (fn la lb))))

(defun levenshtein-equal-p (string1 string2)
  "Return true if STRING1 and STRING2 are equal within a certain Levenshtein distance."
  (let ((length1 (length string1))
        (length2 (length string2)))
    (when (= length1 length2)
      (<= (levenshtein string1 string2) *levenshtein-threshold*))))

(defmacro with-levenshtein (&body body)
  "Run BODY while enabling the Levenshtein distance algorithm."
  `(let ((*levenshtein* t))
     (progn ,@body)))

;;; cosine similarity
(defun magnitude (hash)
  "Magnitude or `l2 norm` or a word hash vector (table bow)."
  (let ((result 0))
    (maphash (lambda (k v)
               (setf result (+ result (* v v))))
             hash)
    (sqrt result)))

(defun dot (hash1 hash2)
  "Dot product of two word hash vectors (table bow)."
  (let ((result 0))
    (maphash (lambda (k v)
               (setf result (+ result (* v (gethash k hash2 0)))))
             hash1)
    result))

(defun cosine-similarity (hash1 hash2)
  "Cosine similary of two word hash vectors (table bow)."
  (lparallel:plet ((product (dot hash1 hash2))
                   (magnitude1 (magnitude hash1))
                   (magnitude2 (magnitude hash2)))
    (/ product (* magnitude1 magnitude2))))

(defun cosine-similarity-of-strings (string1 string2)
  "Return cosine similarity of two strings."
  (lparallel:pfuncall #'cosine-similarity
                      (table (make-bow string1))
                      (table (make-bow string2))))

(defun cosine-similarity-of-bows (bow1 bow2)
  "Return cosine similarity of two bows."
  (lparallel:pfuncall #'cosine-similarity
                      (table bow1)
                      (table bow2)))

;;; euclidean similarity
(defun hash-difference (hash1 hash2)
  "Give value/count difference between hash1 and hash2."
  (lparallel:plet ((in-h1-or-diff (maphash (lambda (k v)
                                             (- v (gethash k hash2 0)))
                                           hash1))
                   (only-in-h2 (loop for k being the hash-keys in hash2
                                        using (hash-value v)
                                        appending (if (gethash k hash1)
                                                      nil
                                                      (list (- v))))))
    (append in-h1-or-diff only-in-h2)))

(defun euclidean-similarity (hash1 hash2)
  "Euclidean similarity between to hashes (table bow)."
  (/ 1 (1+ (magnitude (hash-difference hash1 hash2)))))

;;; euclidean cosine similarity
(defun euclidean-cosine-similarity (hash1 hash2)
  "Geometric mean of euclidean and cosine similarity."
  (lparallel:plet ((cos-sim (cosine-similarity hash1 hash2))
                   (euc-sim (euclidean-similarity hash1 hash2)))
    (sqrt (* cos-sim euc-sim))))

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
  "Bury the duplicate pools found in VOLUME."
  (loop :for pool :in (walk-down volume :skip #'unitp)
        :for matches = (gethash '(0) (matches pool))
        :when (> (length matches) 1)
        :do (loop :for pool :in (rest matches) :do (bury pool))))

(defun unbury-duplicates (volume)
  "Unbury the duplicate pools found in VOLUME."
  (loop :for pool :in (walk-down volume :skip #'unitp)
        :for matches = (gethash '(0) (matches pool))
        :when (> (length matches) 1)
        :do (loop :for pool :in (rest matches) :do (unbury pool))))

(defun make-bow (text)
  "Return a BOW object from TEXT, creating and deleting temporary stores."
  (let* ((bow (make-instance 'bow :source text))
         (volume (import-flat-text text))
         (registry (find-registry (rid volume))))
    (bind-self volume)
    (bury-duplicates volume)
    (lparallel:pmapc #'(lambda (pool)
                         (setf (gethash (first (nodes-values pool)) (table bow))
                               (length (gethash '(0) (matches pool)))))
                     (walk-down volume :skip #'unitp))
    (delete-volume volume registry)
    (delete-registry registry)
    bow))

(defun dump-bow (bow)
  "Print information about a BOW."
  (maphash #'(lambda (k v)
               (format t "~&~10S => ~S" k v ))
           (table bow)))

(defun bow-get (pool bow)
  "Retrieve BOW count from BOW under POOL."
  (gethash pool (table bow)))

(defun bow-count (item text)
  "Retrieve the number of times ITEM appears in TEXT."
  (let ((count (bow-get item (make-bow text))))
    (if count
        count
        0)))

(defun make-n-gram (text size &optional (regex "\\s+"))
  "Build an n-gram sequence from TEXT, as collection of pool groups. SIZE is the size of the grouping, while REGEX is the separator between the items inside TEXT."
  (let* ((volume (import-flat-text text :regex regex))
         (registry (find-registry (rid volume))))
    (prog1 (when (> size 0)
             (let ((value (loop :for pool :in (walk-down volume :skip #'unitp)
                                :when (range pool size)
                                :collect it)))
               (remove nil value)))
      (delete-volume volume registry)
      (delete-registry registry))))

(defun make-n-gram-text (&rest args)
  "Apply MAKE-N-GRAM to TEXT and return string groups."
  (loop :for group :in (apply #'make-n-gram args)
        :collect (mapcan #'nodes-values group)))
