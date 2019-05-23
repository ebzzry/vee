;;;; core.lisp

(uiop:define-package #:muso/core
    (:use #:cl
          #:trivia
          #:muso/globals)
  (:export #:*empty-entry*
           #:read-source-file))

(in-package #:muso/core)

(defun slurp-file (path)
  "Read entire file as lines."
  (uiop:read-file-lines path))

(defun file-string (path)
  "Read entire file as byte sequence."
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun normalize (entry)
  "Return a normalized version of an entry."
  (string-downcase entry))

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

(defun join (items)
  "Return a string from items in LIST."
  (reduce #'mof:cat items))

(defun join-n (items n)
  "Join strings from items with N count."
  (join (take-if #'first items n)))

(defun join-next (items)
  "Join with the next item."
  (join-n items *join-limit*))

(defun count-entries (column)
  "Return the number of entries in COLUMN."
  (with-open-file (in column)
    (loop :for line = (read-line in nil)
          :while line
          :count line)))

(defun count-entries-file (file)
  "Return the number of entries in FILE."
  (count-entries file))

(defun max-column (column-1 column-2)
  "Return the bigger column between COLUMN-1 and COLUMN-2."
  (let ((size-1 (count-entries column-1))
        (size-2 (count-entries column-2)))
    (cond ((= size-1 size-2) column-1)
          ((> size-1 size-2) column-1)
          (t column-2))))

(defun read-tsv-string (string)
  "Read a TSV string and return lists from it."
  (read-csv-string string #\tab))

(defun read-tsv-file (file)
  "Like READ-TSV but from a disk file."
  (read-csv-file file #\tab))

(defun take (column &optional (limit 1))
  "Return LIMIT amount of items from COLUMN."
  (loop :for s :in column
        :for n = 0 :then (1+ n)
        :while (< n limit)
        :collect s))

(defun take-if (fn column &optional (limit 1))
  "Return LIMIT amount of items from COLUMN."
  (loop :for s :in column
        :for n = 0 :then (1+ n)
        :while (< n limit)
        :collect (funcall fn s)))

(defun peek (column limit &key (selector #'first))
  "Return a string formed by looking ahead LIMIT number of entries in COLUMN."
  (mof:join-strings (mapcar selector (take (read-tsv-string column) limit))))

(defun peek-file (file limit)
  "Like PEEK but from a disk file."
  (with-open-file (in file)
    (peek in limit)))

(defun head (file)
  "Return the first element of from FILE."
  (multiple-value-bind (value result)
      (with-open-file (in file)
        (read-line in nil))
    (declare (ignore result))
    (let ((val (read-tsv-string value)))
      (when (consp val)
        (first val)))))

(defun clean-spacy-column (data)
  "Remove extraneous entries in a spaCy source."
  (remove-if #'(lambda (entry) (mof:empty-string-p (first entry))) data))

(defun read-source-file (file &optional (spacy nil))
  "Read source from file and perform clean-ups as necessary."
  (let ((data (read-tsv-file file)))
    (if spacy
        (clean-spacy-column data)
        data)))

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

(defun current-entry (column)
  "Return the current item text in COLUMN."
  (first column))

(defun next-entry (column)
  "Return the next item text in COLUMN."
  (second column))

(defun current (column)
  "Return the head of the curent item in COLUMN."
  (first (current-entry column)))

(defun next (column)
  "Return the head of the next item in COLUMN."
  (first (next-entry column)))

(defun top (&rest args)
  "Return the top-most entry in column."
  (apply #'current-entry args))

(defun compact-string (string)
  "Return a new string without whitespaces."
  (cl-ppcre:regex-replace-all "\\s+" string ""))

(defun partial-match-p (text-1 text-2)
  "Return true if X is a partial match against Y. String compaction will happen
with the longer column, which is usually Y."
  (strict-substring-p (compact-string text-1) (compact-string text-2)))

(defun complete-match-p (text-1 entry-2 &key (case-sensitive nil))
  "Return true if X and Y match completely."
  (and (= (length text-1) (length entry-2))
       (if case-sensitive
           (string= text-1 entry-2)
           (string-equal text-1 entry-2))))

;;; Notes
;;;
;;; - When a hyphen-separated text is broken down, the hyphen is part of the
;;;   breakdown.
;;; - When a space-separated text is broken down, the space is not part of the
;;;   breakdown.

;;; Notes
;;;
;;; - Write the walker
;;; - When a complete match is found, add entries to destination.
;;; - When a partial match is found, check to see if the next item also matches.
;;; - If text matches, create new entry to TSV list, then pop both columns.
;;; - Empty strings will be used for blanks:
;;;
;;;   ("﻿NOTRE" "NNP" "﻿NOTRE-DAME" "PROPER-MODIFIER")
;;;   ("-" "HYPH" "" "")
;;;   ("DAME" "NN" "" "")

(defun add (left right acc)
  "Add LEFT and RIGHT to ACC."
  (cons (append left right) acc))

(defun add-top (left right acc)
  "Add the top of LEFT and RIGHT to ACC."
  (add (top left) (top right) acc))

(defun advance (&rest items)
  "Return the other items from ITEMS. A wrapper around REST. This function will
be converted to a method."
  (apply #'rest items))

(defun empty-left (datum)
  "Return a grouping wherein the left side is empty."
  (append *empty-entry* datum))

(defun empty-right (datum)
  "Return a grouping wherein the right side is empty."
  (append datum *empty-entry*))

(defun fill-blanks (data side)
  "Create blanks from DATA on side SIDE."
  (loop :for n :from 0 :to (length data)
        :for datum :in data
        :collect (ecase side
                   (left (empty-left datum))
                   (right (empty-right datum)))))

(defun merge-equal (ldata rdata)
  "Merge equal columns."
  (assert (= (length ldata) (length rdata)) (ldata rdata))
  (loop :for n :from 0 :to (length ldata)
        :for (l1 l2) :in ldata
        :for (r1 r2) :in rdata
        :collect (list l1 l2 r1 r2)))

(defun merge-columns (ldata rdata)
  "Merge together the columns in LDATA and RDATA."
  (let ((length-1 (length ldata))
        (length-2 (length rdata)))
    (cond ((= length-1 length-2) (merge-equal ldata rdata))
          ((< length-1 length-2)
           (append (merge-equal ldata (subseq rdata 0 length-1))
                   (fill-blanks (subseq rdata (1- length-1)) 'left)))
          (t
           (append (merge-equal (subseq ldata 0 length-2) rdata)
                   (fill-blanks (subseq ldata (1- length-2)) 'right))))))

(defun remove-leading-garbage (column)
  "Remove leading information that is not needed from column."
  column)

(defun walk (ldata rdata acc &key (lcarry nil) (rcarry nil))
  "Walk through the columns and build value."
  (let ((lhead (or lcarry (current ldata)))
        (rhead (or rcarry (current rdata))))
    ;; NOTE: the usual case is that LDATA is longer than RDATA, so that LDATA
    ;;       will serve as the wall
    (cond
      ;; NOTE: handle cases wherein one of the data columns is already null
      ;; NOTE: this means that one of the coulmns still has trailing data

      ;; NOTE: complete the length of rdata. This will be the amount of empty
      ;; entries that will be added on the left side, to acc, which will then be
      ;; returned
      ((null ldata)
       (let ((rlength (length rdata)))
         ;; FIXME
         rlength))

      ((and (null ldata) (null rdata))
       (nreverse acc))

      ;; NOTE: The amount of look ahead is the amount of pair combinations
      ;;       that must be tested.

      ;; NOTE: It also determines the amount of jump to the next subset of
      ;;       column

      ;; complete
      ;; When both LHEAD and RHEAD match, they must be matching. Both LDATA
      ;; and RDATA will move. LCARRY and RCARRY must be cleared out while
      ;; advancing.
      ((complete-match-p lhead rhead)
       (walk (advance ldata)
             (advance rdata)
             (add (top ldata) (top rdata) acc)
             :lcarry nil
             :rcarry nil))

      ;; and partial partial
      ;; When the LHEAD and RHEAD partially match,
      ;; and the join and RHEAD partially match,
      ;; it means that there is a partial match but won’t complete.

      ;; NOTE: when a partial match is found, should both coulmns also move,
      ;;       with the other side having empty entries?
      ;; NOTE: should RCARRY should also be updated?
      ;; NOTE: should RCARRY be updated with empty entries as long as they do
      ;;       not match

      ((and (partial-match-p lhead rhead)
            (partial-match-p (join-next ldata) rhead))
       (walk (advance (advance ldata))
             rdata
             acc
             :lcarry (join-next ldata)
             :rcarry nil))

      ;; and partial complete
      ;; When the LHEAD and RHEAD match,
      ;; and the join and RHEAD completely match,
      ;; it means that there will be a complete match. This complete
      ;; matching will be handled in the next iteration.

      ;; NOTE: how is this condition different from the one above?

      ((and (partial-match-p lhead rhead)
            (complete-match-p (join-next ldata) rhead))
       ;; TODO: verify for correctness
       (walk (advance (advance ldata))
             rdata
             acc
             :lcarry (join-next ldata)
             :rcarry nil))

      ;; inverse and partial partial

      ;; inverse and partial complete

      ;; TODO: describe fallback
      (t nil))))

;;; Notes
;;;
;;; - A column will only move when it is done processing
;;; - Insert the previous head into the next head?
;;; - There is a partial match if and only if the current tips match and merging
;;;   with the next entry is either a partial or complete match ***
;;; - The entries themselves will be put to the accumulator, however, it is only
;;;   the text that will be compared.

;;; Notes
;;;
;;; - Whenever a join is made on one side, an empty pair is created on the other
;;;   side
;;; - So, it’s viable that they both move, but an empty entry will be created
;;; - When a ‘move’ is made, no destructive modifications are made to any of the
;;;   coulmns
;;; - Should a partially matching text be carried and built forward until it either
;;;   matches or longer matches?

;;; Notes
;;;
;;; - Build a new column where the first two items are joined
;;; - At the moment, the walker will only work with two simultaneous columns

;;; Notes
;;;
;;; - Design a scheme for properly instantiating the classes
;;; - Handle the case wherein there are no matches at all
;;; - Handle garbage

(defun stats ()
  "Display information about similarities, differences, holes, inconsistencies,
etc."
  nil)

;;; Legend
;;;
;;; - Entry
;;;   + The smallest unit of information
;;;   + It contains information about a text, like POS, UD, etc
;;;
;;; - Column
;;;   + A list of entries
;;;   + It must have a uniform size
;;;
;;; - Connection
;;;    + The linkage between columns
;;;
;;; - Grouping
;;;   + A horizontal set of entries across columns

;;; Notes
;;;
;;; - Find proper locations to instantiate
;;; - Should all data transactions go through the methods and classes?
;;; - Use a selector when specifying the key when specifying the TSVs
;;; - By default it is the first item

;;; Notes
;;;
;;; - Convert to a generic delimiter library
