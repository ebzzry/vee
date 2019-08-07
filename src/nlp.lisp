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

;; cosine similarity

(defun magnitude (hash)
  "Magnitude or `l2 norm` or a word hash vector (table bow)."
  (let ((result 0))
    (maphash (lambda (k v)
	       (setq result (+ result (* v v))))
	     hash)
    (sqrt result)))

(defun dot (hash1 hash2)
  "Dot product of two word hash vectors (table bow)."
  (let ((result 0))
    (maphash (lambda (k v)
	       (setq result (+ result (* v (gethash k hash2 0)))))
	     hash1)
    result))

(defun cosine-similarity (hash1 has2)
  "Cosine similary of two word hash vectors (table bow)."
  (lparallel:plet ((product    (dot hash1 hash2))
		   (magnitude1 (magnitude hash1))
		   (magnitude2 (magnitude hash2)))
    (/ product (* magnitude1 magnitude2))))

(defun cosine-similarity-of-strings (string-1 string-2)
  "Return cosine similarity of two strings."
  (lparallel:pfuncall #'cosine-similarity
		      (table (make-bow string-1))
		      (table (make-bow string-2))))

(defun cosine-similarity-of-bows (bow-1 bow-2)
  "Return cosine similarity of two bows."
  (lparallel:pfuncall #'cosine-similarity
		      (table bow-1)
		      (table bow-2)))

;; euclidean-similarity

(defun hash- (hash1 hash2)
  "Give value/count difference between hash1 and has2."
  (lparallel:plet ((in-h1-or-diff (maphash (lambda (k v)
					     (- v (gethash k hash2 0)))
					   hash1))
		   (only-in-h2    (loop for k being the hash-keys in hash2
					  using (hash-value v)
					appending (if (gethash k hash1)
						      nil
						      (list (- v))))))
    (append in-h1-or-diff only-in-h2)))

(defun euclidean-similarity (hash1 hash2)
  "Euclidean similarity between to hashes (table bow)."
  (/ 1 (1+ (magnitude (hash- hash1 hash2)))))

;; euclidean-cosine-similarity

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
  "Bury the duplicate entries found in VOLUME."
  (loop :for entry :in (walk-down volume :skip #'unitp)
        :for matches = (gethash '(0) (matches entry))
        :when (> (length matches) 1)
        :do (loop :for entry :in (rest matches) :do (bury entry))))

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

(defun make-n-gram (text size &optional (regex "\\s+"))
  "Build an n-gram sequence from TEXT, as collection of entry groups. SIZE is the size of the grouping, while REGEX is the separator between the items inside TEXT."
  (let* ((volume (import-flat-text text :regex regex))
         (registry (find-registry (rid volume))))
    (prog1 (when (> size 0)
             (let ((value (loop :for entry :in (walk-down volume :skip #'unitp)
                                :when (range entry size)
                                :collect it)))
               (remove nil value)))
      (delete-volume volume registry)
      (delete-registry registry))))

(defun make-n-gram-text (&rest args)
  "Apply MAKE-N-GRAM to TEXT and return string groups."
  (loop :for group :in (apply #'make-n-gram args)
        :collect (mapcan #'fields-values group)))