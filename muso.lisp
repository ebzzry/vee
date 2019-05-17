;;;; muso.lisp

(in-package #:muso)

;;; General
;;;
;;; - The goal is to create a new TSV file wherein the first column of each
;;;   source match to each other
;;; - This version ignores the tags

;;; Destination format
;;;
;;; 1. the text from first source
;;; 2. the tag from first source
;;; 3. the text from third source
;;; 4. the tag from second source
;;; 5. optional flags
;;;
;;; Essentially, there will be two column pairs.

;;; Row format
;;;
;;; Rows are created with the text. For example:
;;;
;;;     Hunchback       | PROPER-NAME           | Hunchback     | NNP
;;;
;;; However, column pair can be empty if an equivalent text is broken down into
;;; more rows. For example:
;;;
;;;     Notre-Dame      | PROPER-MODIFIER       | Notre         | NNP
;;;                     |                       | -             | HYPH
;;;                     |                       | dame          | NN
;;;

;;; Preprocesses
;;;
;;; - If the line doesn’t match the TEXT-SEPARATOR-TEXT format, remove that line.
;;; - If the line is empty, that is it contains only a newline, remove that line.
;;; - If the first character of a file line is the separator, remove that line.
;;; - If the line only contains the separator, remove that line.

;;; Notes
;;;
;;; - File processing should be done only once, and the rest will be done on
;;;   streams.
;;; - Create fallback row creation
;;; - It seems that rules have to be encoded, at least with the case of spaCy,
;;;   because it breaks down hyphenated works into multiple entries.
;;; - Determine the main source that comparisons will be made against.
;;; - Is the longer file going to be used the the ’wall’?
;;; - Should the longer source be set already, then the entries from the shorter
;;;   file will just be inserted into the location based on the longer source?
;;; - If this is the case, then as entries from the shorter source are inserted
;;;   to the longer source, the longer source gets popped.

;;; Caveats
;;;
;;; Determining which source to set against may not make sense because it
;;; make the assumption, that one has a generally longer source cluster than the
;;; other, which is not the case. Consider the following sets:
;;;
;;;     Notre-Dame      | PROPER-MODIFIER       | Notre         | NNP
;;;                     |                       | -             | HYPH
;;;                     |                       | dame          | NN
;;;     also known as   | DANGLING-ADVERB       | Also          | RB
;;;                     |                       | known         | VBN
;;;                     |                       | as            | IN
;;;
;;; In the first column pair, ‘Notre-Dame’ and ‘also known as’ are contiguous
;;; items, while that is not the case with the second column pair.
;;;
;;; Problem arises when sets are structured as such:
;;;
;;;     Notre-Dame      | PROPER-MODIFIER       | Notre         | NNP
;;;                     |                       | -             | HYPH
;;;                     |                       | dame          | NN
;;;     also            | DANGLING-ADVERB       | Also known as | RB
;;;     known           | DANGLING-ADVERB       |               |
;;;     as              | DANGLING-ADVERB       |               |
;;;
;;; This means, that we can’t reliable use a ‘wall’ with the assumption that it
;;; supersets the other set.
;;;
;;; Unless, it can be clearly stated that one source is generally longer than
;;; the other.

(defvar *threshold* 5
  "The amount of lines to consider when trying to perform resoultion, backwards
  and forwards.")

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

(defun join (&rest items)
  "Return a string from items in LIST."
  (reduce #'mof:cat items))

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

(defun take (source limit)
  "Return LIMIT amount of items from SOURCE."
  (loop :for s :in source
        :for n = 0 :then (1+ n)
        :while (< n limit)
        :collect s))

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

(defun top (source)
  "Return the top-most entry in source."
  (first source))

(defun current (source)
  "Return the current item text in SOURCE."
  (first (first source)))

(defun next (source)
  "Return the next item text in SOURCE."
  (first (second source)))

(defun join-ahead (source)
  "Return the current and next items from SOURCE."
  (join (current source) (next source)))

(defun compact-string (string)
  "Return a new string without whitespaces."
  (cl-ppcre:regex-replace-all "\\s+" string ""))

(defun partial-match-p (text-1 text-2)
  "Return true if X is a partial match against Y. String compaction will happen
with the longer source, which is usually Y."
  (strict-substring-p (compact-string text-1) (compact-string text-2)))

(defun reverse-partial-match-p (text-1 text-2)
  "Like PARTIAL-MATCH-P but the second arg will be compared against the first."
  (partial-match-p text-2 text-1))

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

(defun walk (source-1 source-2 acc)
  "Walk through the sources and build value."
  (cond ((and (null source-1)
              (null source-2))
         (nreverse acc))
        ((complete-match-p (current source-1) (current source-2))
         (walk (rest source-1)
               (rest source-2)
               (cons (append (top source-1) (top source-2)) acc)))
        ((partial-match-p (current source-1) (current source-2))
         ;; source-2 will not move
         nil)
        (t nil)))
;;; - Build a new source where the first two items are joined