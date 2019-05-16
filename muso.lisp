;;;; muso.lisp

(in-package #:muso)

(defvar *threshold* 5
  "The amount of lines to consider when trying to perform resoultion, backwards
  and forwards.")

(defun slurp-file (path)
  "Read entire file as lines."
  (uiop:read-file-lines path))

(defun file-string (path)
  "Read entire file as byte sequence."
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun read-entry (entry)
  "Read a single line stripping unnecessary information."
  nil)

(defun normalize (entry)
  "Return a normalized version of an entry."
  (string-downcase entry))

(defun read-csv (entry &key (separator #\Tab))
  "Read a CSV from string."
  (fare-csv:with-rfc4180-csv-syntax ()
    (let ((fare-csv:*separator* separator))
      (fare-csv:read-csv-file source))))

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

;;; General
;;;
;;; - The goal is to create a new TSV file wherein the first column of each
;;;   source match to each other
;;; - This version ignores the tags
;;; - Is a stack a good way to treat handle the sources when removing entries

;;; Matching
;;;
;;; - A match is considered to be found when the word matches, case not
;;;   considered
;;; - Tag matching is not reliable since parsers treat entries differently, for
;;;   a given cluster of text groupings.
;;; - When a match is found, delete entries from both sources.

;;; Destination format
;;;
;;; 1. the text from first source
;;; 2. the tag from first source
;;; 3. the text from third source
;;; 4. the tag from second source
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

;;; Tracking methods
;;;
;;; - text equivalence, case insensitive
;;; - current row

;;; Preprocesses
;;;
;;; - If the line doesn’t match the TEXT-SEPARATOR-TEXT format, remove that line.
;;; - If the line is empty, that is it contains only a newline, remove that line.
;;; - If the first character of a file line is the separator, remove that line.
;;; - If the line only contains the separator, remove that line.

;;; Rules
;;;
;;; - If text matches, create new entry to TSV list, then pop both sources.

;;; Notes
;;;
;;; - Create fallback row creation
;;; - It seems that rules have to be encoded, at least with the case of spaCy,
;;;   because it breaks down hyphenated works into multiple entries.
;;; - Determine the main source that comparisons will be made against.
;;; - Is the longer file going to be used the the ’wall’?
;;; - Should the longer source be set already, then the entries from the shorter
;;;   file will just be inserted into the location based on the longer source?
;;; - If this is the case, then as entries from the shorter source are inserted
;;;   to the longer source, the longer source gets popped.

(defun delimit (list)
  "Returns items in LIST as tab-separated values."
  (format nil "~{~a~^	~}" list))

(defun join-strings (list)
  "Return a string from items in LIST."
  (reduce #'mof:cat list))

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

(defun read-tsv (source)
  "Read a TSV source and return lists from it."
  (read-csv source #\tab))

(defun read-tsv-file (file)
  "Like READ-TSV but from a disk file."
  (with-open-file (in file)
    (read-tsv in)))

(defun take (source limit)
  "Return LIMIT amount of items from SOURCE."
  (loop :for s :in source
        :for n = 0 :then (1+ n)
        :while (< n limit)
        :collect s))

(defun peek (source limit &key (selector #'first))
  "Return a string formed by looking ahead LIMIT number of entries in SOURCE."
  (join-strings (mapcar selector (take (read-tsv source) limit))))

(defun peek-file (file limit)
  "Like PEEK but from a disk file."
  (with-open-file (in file)
    (peek in limit)))

(defun clean-spacy-source (data)
  "Remove extraneous entries in a spaCy source."
  (remove-if #'(lambda (entry) (mof:empty-string-p (first entry))) data))

(defun read-source-file (file &optional (spacy nil))
  "Read source from file and perform clean-ups as necessary."
  (let ((data (read-tsv-file file)))
    (if spacy
        (clean-spacy-source data)
        data)))

;;; Notes
;;;
;;; - Write the test functions.
;;; - Write the file-creation functions.
;;; - Sources should only be read once.
;;; - If a complete match is found, proceed.
;;; - If a partial match is found, peek.
;;; - File processing should be done only once, and the rest will be done on
;;;   streams.

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

(defun strict-substring-p (x y)
  "Return true if X is part of Y, and that X is found from the start of Y."
  (and (not (= (length x) (length y)))
       (zerop (search x y))))

;;; Should this perform peeking?
(defun partial-match-p (x y)
  "Return true if X is a partial match against Y."
  nil)

(defun complete-match-p (x y)
  "Return true if x y match completely."
  (and (= (length x) (length y))
       (string-equal x y)))

;;; Notes
;;;
;;; - Break down word components into words. For example, "notre-dame" ->
;;;   "notre" "-" "dame"
;;; - The amount of lookahead can be determined by the number of components that
;;;   an entry from the shorter source resulted to.
;;; - "Victor Hugo" -> 2

(defun separators (string)
  "Return the separator used in STRING."
  (loop :for char :across (remove-if #'alphanumericp string)
        :collecting char :into chars
        :finally (return (remove-duplicates chars))))

;;; Notes
;;;
;;; - When a hyphen-separated text is broken down, the hyphen is part of the
;;;   breakdown.
;;; - When a space-separated text is broken down, the space is not part of the
;;;   breakdown.
