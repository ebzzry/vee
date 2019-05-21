;;;; muso.lisp

(in-package #:muso)

(defvar *threshold* 5
  "The amount of lines to consider when trying to perform resoultion, backwards
  and forwards.")

(defvar *join-limit* 2
  "The amount of lines forward to join.")

(defun slurp-file (path)
  "Read entiore file as lines."
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

(defun read-csv-string (source &optional (separator #\tab))
  "Read a CSV from string."
  (with-input-from-string (stream source)
    (fare-csv:with-rfc4180-csv-syntax ()
      (let ((fare-csv:*separator* separator))
        (fare-csv:read-csv-stream stream)))))

(defun read-csv-file (file &optional (separator #\tab))
  "Read a CSV from file."
  (fare-csv:with-rfc4180-csv-syntax ()
    (let ((fare-csv:*separator* separator))
      (fare-csv:read-csv-file file))))

(defun stack-top (source)
  "Return the current entry being processed."
  (first source))

(defun stack-pop (source)
  "Pop the next entry to be processed."
  (pop source)
  source)

(defun *active-map*
  nil
  "The active map being used for translation.")

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
  (join-n items 2))

(defun count-entries (source)
  "Return the number of entries in SOURCE."
  (with-open-file (in source)
    (loop :for line = (read-line in nil)
          :while line
          :count line)))

(defun count-entries-file (file)
  "Return the number of entries in FILE."
  (count-entries file))

(defun max-source (source-1 source-2)
  "Return the bigger source between SOURCE-1 and SOURCE-2."
  (let ((size-1 (count-entries source-1))
        (size-2 (count-entries source-2)))
    (cond ((= size-1 size-2) source-1)
          ((> size-1 size-2) source-1)
          (t source-2))))

(defun read-tsv-string (string)
  "Read a TSV string and return lists from it."
  (read-csv-string string #\tab))

(defun read-tsv-file (file)
  "Like READ-TSV but from a disk file."
  (read-csv-file file #\tab))

(defun take (source &optional (limit 1))
  "Return LIMIT amount of items from SOURCE."
  (loop :for s :in source
        :for n = 0 :then (1+ n)
        :while (< n limit)
        :collect s))

(defun take-if (fn source &optional (limit 1))
  "Return LIMIT amount of items from SOURCE."
  (loop :for s :in source
        :for n = 0 :then (1+ n)
        :while (< n limit)
        :collect (funcall fn s)))

(defun peek (source limit &key (selector #'first))
  "Return a string formed by looking ahead LIMIT number of entries in SOURCE."
  (mof:join-strings (mapcar selector (take (read-tsv-string source) limit))))

(defun peek-file (file limit)
  "Like PEEK but from a disk file."
  (with-open-file (in file)
    (peek in limit)))

(defun head (file)
  "Return the first element of source from FILE."
  (multiple-value-bind (value result)
      (with-open-file (in file)
        (read-line in nil))
    (declare (ignore result))
    (let ((val (read-tsv-string value)))
      (when (consp val)
        (first val)))))

(defun clean-spacy-source (data)
  "Remove extraneous entries in a spaCy source."
  (remove-if #'(lambda (entry) (mof:empty-string-p (first entry))) data))

(defun read-source-file (file &optional (spacy nil))
  "Read source from file and perform clean-ups as necessary."
  (let ((data (read-tsv-file file)))
    (if spacy
        (clean-spacy-source data)
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

(defun current-entry (source)
  "Return the current item text in SOURCE."
  (first source))

(defun next-entry (source)
  "Return the next item text in SOURCE."
  (second source))

(defun current (source)
  "Return the head of the curent item in SOURCE."
  (first (current-entry source)))

(defun next (source)
  "Return the head of the next item in SOURCE."
  (first (next-entry source)))

(defun top (&rest args)
  "Return the top-most entry in source."
  (apply #'current-entry args))

(defun compact-string (string)
  "Return a new string without whitespaces."
  (cl-ppcre:regex-replace-all "\\s+" string ""))

(defun partial-match-p (text-1 text-2)
  "Return true if X is a partial match against Y. String compaction will happen
with the longer source, which is usually Y."
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
;;; - If text matches, create new entry to TSV list, then pop both sources.
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

(defun walk (ldata rdata acc &key (lcarry nil) (rcarry nil))
  "Walk through the sources and build value."
  (let ((lhead (or lcarry (current ldata)))
        (rhead (or rcarry (current rdata))))
    (cond ((and (null ldata) (null rdata))
           (nreverse acc))

          ;; NOTE: The amount of look ahead is the amount of pair combinations
          ;;       that must be tested.

          ;; NOTE: It also determines the amount of jump to the next subset of
          ;;       source

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
;;;   sources
;;; - Should a partially matching text be carried and built forward until it either
;;;   matches or longer matches?

;;; Notes
;;;
;;; - Build a new source where the first two items are joined
;;; - At the moment, the walker will only work with two simultaneous sources

;;; Notes
;;;
;;; - Design a scheme for properly instantiating the classes
;;; - Handle the case wherein there are no matches at all
;;; - Handle garbage

(defun stats ()
  "Display information about similarities, differences, holes, inconsistencies,
etc."
  nil)
