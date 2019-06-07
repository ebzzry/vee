;;;; common.lisp

(in-package #:muso/core)

(defun slurp-file (path)
  "Read entire file as lines."
  (uiop:read-file-lines path))

(defun file-string (path)
  "Read entire file as byte sequence."
  (with-open-file (stream path)
    (let ((val (make-string (file-length stream))))
      (read-sequence val stream)
      val)))

(defun normalize (item)
  "Return a normalized version of ITEM."
  (string-downcase item))

(defun normalize-items (subcol)
  "Return normalized items from SUBCOL."
  (mapcar #'normalize subcol))

(defun read-csv-string (string &optional (separator #\tab))
  "Read a CSV from string."
  (with-input-from-string (stream string)
    (fare-csv:with-rfc4180-csv-syntax ()
      (let ((fare-csv:*separator* separator))
        (fare-csv:read-csv-stream stream)))))

(defun read-csv-file (file &optional (separator #\tab))
  "Read a CSV from file."
  (fare-csv:with-rfc4180-csv-syntax ()
    (let ((fare-csv:*separator* separator))
      (fare-csv:read-csv-file file))))

(defun delimit (list)
  "Returns items in LIST as tab-separated values."
  (format nil "~{~a~^	~}" list))

(defmacro defselectors (limit)
    "Define selectors."
    `(progn
       ,@(loop :for n :from 0 :to limit
               :for name = (read-from-string (mof:cat "elt" (write-to-string n)))
               :collect `(defun ,name (list) (elt list ,n)))))
(defselectors 1000)

(defun join (items)
  "Return a string from items in LIST."
  (reduce #'mof:cat items))

(defun join-n (items n &key (selector #'elt0))
  "Join strings from items with N count."
  (join (take-if selector items n)))

(defun join-next (items)
  "Join with the next item."
  (join-n items *join-limit*))

(defun count-entries (feed)
  "Return the number of entries in feed."
  (length feed))

(defun count-entries-file (file)
  "Return the number of entries in FILE."
  (with-open-file (in file)
    (loop :for line = (read-line in nil)
          :while line
          :count line)))

(defun max-feed (feed-1 feed-2)
  "Return the bigger feed between FEED-1 and FEED-2."
  (let ((size-1 (count-entries feed-1))
        (size-2 (count-entries feed-2)))
    (cond ((= size-1 size-2) feed-1)
          ((> size-1 size-2) feed-1)
          (t feed-2))))

(defun read-tsv-string (string)
  "Read a TSV string and return lists from it."
  (read-csv-string string #\tab))

(defun read-tsv-file (file)
  "Like READ-TSV but from a disk file."
  (read-csv-file file #\tab))

(defun take (feed &optional (limit 1))
  "Return LIMIT amount of items from FEED."
  (loop :for s :in feed
        :for n = 0 :then (1+ n)
        :while (< n limit)
        :collect s))

(defun take-if (fn feed &optional (limit 1))
  "Return LIMIT amount of items from FEED."
  (loop :for s :in feed
        :for n = 0 :then (1+ n)
        :while (< n limit)
        :collect (funcall fn s)))

(defun peek (feed limit &key (selector #'elt0))
  "Return a string formed by looking ahead LIMIT number of entries in FEED."
  (mof:join-strings (mapcar selector (take (read-tsv-string feed) limit))))

(defun peek-file (file limit)
  "Like PEEK but from a disk file."
  (with-open-file (in file)
    (peek in limit)))

(defun prefixedp (string prefixes)
  "Return true if STRING is prefixed with PREFIX."
  (member (elt string 0) prefixes))

(defun underscoredp (string)
  "Return true if STRING is prefixed with an underscore."
  (prefixedp string '(#\ZERO_WIDTH_NO-BREAK_SPACE #\_)))

(defun fix-feed (feed)
  "Make corrections to feed due to the effects of file reading."
  (destructuring-bind (head &rest body)
      feed
    (destructuring-bind (h &rest b)
        head
      (if (underscoredp h)
          (cons (cons (subseq h 1) b)
                body)
          feed))))

(defun clean-feed (feed)
  "Remove extraneous entries in FEED."
  (remove-if #'(lambda (item)
                 (some #'mof:empty-string-p item))
             feed))

(defun read-feed-file (file)
  "Read feed from file and perform clean-ups as necessary."
  (fix-feed (clean-feed (read-tsv-file file))))

(defun read-file (&rest args)
  "An alias to READ-FEED-FILE"
  (apply #'read-feed-file args))

(defun separators (string)
  "Return the separator used in STRING."
  (loop :for char :across (remove-if #'alphanumericp string)
        :collecting char :into chars
        :finally (return (remove-duplicates chars))))

(defun strict-substring-p (x y)
  "Return true if X is part of Y, and that X is found from the start of Y."
  (and (not (= (length x) (length y)))
       (let ((val (search x y)))
         (when val
           (zerop val)))))

(defun map-append (fn sequence-1 sequence-2)
  "Apply APPEND to the result of applying FN to sequence-1 and sequence-2."
  (append (mapcar fn sequence-1) (mapcar fn sequence-2)))

(defun map-nappend (fn sequence-1 sequence-2)
  "Apply NCONC to the result of applying FN to sequence-1 and sequence-2."
  (nconc (mapcar fn sequence-1) (mapcar fn sequence-2)))

(defun genstring (string)
  "Return a GENSYM’d string."
  (string (gensym string)))

(defun pad-feed (feed)
  "Add starting and ending padding for column based on the first element."
  (let* ((initial (elt0 feed))
         (pad (make-empty initial)))
    (append (list pad) feed (list pad))))

(defun slots (object)
  "Return the slot names of an object."
  (mapcar #'sb-mop:slot-definition-name
          (sb-mop:class-direct-slots (class-of object))))
