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

(defun noisep (entry)
  "Return true if entry is considered garbage."
  nil)

(defun normalize (entry)
  "Return a normalized version of an entry."
  (string-downcase entry))

(defun lookahead (line)
  "Return lines ahead using *threshold*"
  nil)

(defun clean-up (entry)
  "Return a cleaned up version of a parsed entry."
  nil)

(defun read-csv (entry &key (separator #\Tab))
  "Read a CSV from string."
  (cl-csv:read-csv entry :separator separator))

(defun merge-entries (entries)
  "Merge together entries into a single lump."
  nil)

(defun stack-top ()
  "Return the current entry being processed."
  nil)

(defun stack-pop ()
  "Pop the next entry to be processed."
  nil)

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
