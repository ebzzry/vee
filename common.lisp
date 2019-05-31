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

(defun current-item (feed)
  "Return the current item text in FEED."
  (first feed))

(defun next-item (feed)
  "Return the next item text in FEED."
  (second feed))

(defun current-head (feed &key (selector #'elt0))
  "Return the head of the curent item in FEED."
  (funcall selector (current-item feed)))

(defun next-head (feed &key (selector #'elt0))
  "Return the head of the next item in FEED."
  (funcall selector (next-item feed)))

(defun top (&rest args)
  "Return the top-most item in feed."
  (apply #'current-item args))

(defun compact-string (string)
  "Return a new string without whitespaces."
  (cl-ppcre:regex-replace-all "\\s+" string ""))

(defun partial-match-p (text-1 text-2)
  "Return true if X is a partial match against Y. String compaction will happen with the longer feed, which is usually Y."
  (strict-substring-p (compact-string text-1) (compact-string text-2)))

(defun complete-match-p (text-1 item-2 &key (case-sensitive nil))
  "Return true if X and Y match completely."
  (and (= (length text-1) (length item-2))
       (if case-sensitive
           (string= text-1 item-2)
           (string-equal text-1 item-2))))

(defun advance (&rest items)
  "Return the other items from ITEMS. A wrapper around REST. This function will be converted to a method."
  (apply #'rest items))

(defun empty-left (datum)
  "Return a grouping wherein the left side is empty."
  (append *empty-item* datum))

(defun empty-right (datum)
  "Return a grouping wherein the right side is empty."
  (append datum *empty-item*))

(defun fill-blanks (data side)
  "Create blanks from DATA on side SIDE."
  (loop :for n :from 0 :to (length data)
        :for datum :in data
        :collect (ecase side
                   (left (empty-left datum))
                   (right (empty-right datum)))))

(defun merge-equal (lfeed rfeed)
  "Merge equal feeds."
  (assert (= (length lfeed) (length rfeed)) (lfeed rfeed))
  (loop :for n :from 0 :to (length lfeed)
        :for (l1 l2) :in lfeed
        :for (r1 r2) :in rfeed
        :collect (list l1 l2 r1 r2)))

(defun merge-feeds (lfeed rfeed)
  "Merge together the feedumns in LFEED and RFEED."
  (let ((length-1 (length lfeed))
        (length-2 (length rfeed)))
    (cond ((= length-1 length-2) (merge-equal lfeed rfeed))
          ((< length-1 length-2)
           (append (merge-equal lfeed (subseq rfeed 0 length-1))
                   (fill-blanks (subseq rfeed (1- length-1)) 'left)))
          (t
           (append (merge-equal (subseq lfeed 0 length-2) rfeed)
                   (fill-blanks (subseq lfeed (1- length-2)) 'right))))))

(defun uniques (feed &key (selector #'elt0) (test #'string-equal))
  "Return the unique items from FEED."
  (delete-duplicates (normalize-items (mapcar selector feed)) :test test))

(defun minimal-common-p (lcol rcol &key (selector #'elt0) (test #'string-equal))
  "Return true if there are common lines between LCOL and RCOL."
  (flet ((fn (arg) (uniques arg :selector selector :test test)))
    (when (intersection (fn lcol) (fn rcol) :test test)
      t)))

(defun map-append (fn sequence-1 sequence-2)
  "Apply APPEND to the resultr of applying FN to sequence-1 and sequence-2."
  (append (mapcar fn sequence-1) (mapcar fn sequence-2)))

(defun map-nappend (fn sequence-1 sequence-2)
  "Apply APPEND to the resultr of applying FN to sequence-1 and sequence-2."
  (nconc (mapcar fn sequence-1) (mapcar fn sequence-2)))

(defun similarity (lcol rcol &key (selector #'elt0) (test #'string-equal))
  "Return in % how similar are LCOL and RCOL."
  (let ((common (length (nintersection (mapcar selector lcol)
                                       (mapcar selector rcol)
                                       :test test)))
        (total (length (map-nappend selector lcol rcol))))
    (* (/ common (/ total 1.0)) 100)))
