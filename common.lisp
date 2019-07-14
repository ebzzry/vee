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

(defun normalize-items (subvol)
  "Return normalized items from SUBVOL."
  (mapcar #'normalize subvol))

(defun read-csv-file (file &key (delimiter *default-delimiter*))
  "Read CSV data from FILE."
  (fare-csv:with-rfc4180-csv-syntax ()
    (let ((fare-csv:*separator* delimiter))
      (fare-csv:read-csv-file file))))

(defun read-csv-stream (stream &key (delimiter *default-delimiter*))
  "Read CSV data from STREAM."
  (fare-csv:with-rfc4180-csv-syntax ()
    (let ((fare-csv:*separator* delimiter))
      (fare-csv:read-csv-stream stream))))

(defun read-csv-string (string &rest args)
  "Read CSV data from STRING."
  (with-input-from-string (s string)
    (apply #'read-csv-stream s args)))

(defun delimit (list)
  "Returns items in LIST as tab-separated values."
  (format nil "~{~a~^	~}" list))

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
  (read-csv-string string :delimiter #\tab))

(defun read-tsv-file (file)
  "Like READ-TSV but from a disk file."
  (read-csv-file file :delimiter #\tab))

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
  (prefixedp string '(#\U+FEFF #\_)))

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

;;; Note: generalize to streams
(defun read-feed-file (file &key (delimiter *default-delimiter*))
  "Read feed from file and perform clean-ups as necessary."
  (fix-feed (read-csv-file (mof:expand-pathname file) :delimiter delimiter)))
(mof:defalias read-file read-feed-file)

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
(defun make-volume-name ()
  "Return a unique volume name."
  (genstring "VOLUME"))
(defun make-registry-name ()
  "Return a unique registry name."
  (genstring "REGISTRY"))

(defun make-empty (datum)
  "Return an empty item based from DATUM."
  (let ((length (length datum)))
    (make-list length :initial-element *pad*)))

(defun pad-feed (feed)
  "Add starting and ending padding for volume based on the first element."
  (let* ((initial (elt0 feed))
         (pad (make-empty initial)))
    (append (list pad) feed (list pad))))

(defun slots (object)
  "Return the slot names of an object."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(defun build-name (template fallback)
  "Return a generate name from TEMPLATE; otherwise use FALLBACK."
  (if (null (mof:empty-string-p fallback))
      fallback
      (let ((string (string-upcase template)))
        (genstring string))))

(defun true-false-p (x y)
  "Return true if X is true and Y is false."
  (if (and x (null y)) t nil))

(defun false-true-p (x y)
  "Return true if X is false and Y is true."
  (true-false-p y x))

(defun true-true-p (x y)
  "Return true if X is true and Y is true."
  (and x y t))

(defun ensure-list (object)
  "Return OBJECT if it a list, otherwise return OBJECT inside a list."
  (if (listp object) object (list object)))

(defun nil-wrap (list)
  "Add NIL items in the start and end of LIST."
  (append (list nil) list (list nil)))

(defun equalize-lists (list-1 list-2 &optional (padding ""))
  "Make LIST-1 and LIST-2 of the same length by adding PADDING."
  (let ((length-1 (length list-1))
        (length-2 (length list-2)))
    (flet ((fn (x y)
             (make-list (- x y) :initial-element padding)))
      (cond ((= length-1 length-2) (list list-1 list-2))
            ((> length-1 length-2) (list list-1
                                         (append list-2 (fn length-1 length-2))))
            ((< length-1 length-2) (list (append list-1 (fn length-2 length-1))
                                         list-2))))))

(defun function-integer-p (object)
  "Return true if OBJECT is either a function or integer."
  (when (or (functionp object)
            (integerp object))
    t))

(defun every-list-p (object)
  "Return true if OBJECT is a list and all members are lists."
  (and (listp object)
       (every #'listp object)))

(defun every-string-p (object)
  "Return true if OBJECT is a list and all members are strings."
  (and (listp object)
       (every #'stringp object)))

(defun true (arg)
  "Return true for anything."
  (declare (ignore arg))
  t)

(defun false (arg)
  "Return false for anything."
  (declare (ignore arg))
  nil)

(defun split-text (text &optional (regex "\\s+"))
  "Split text by REGEX."
  (cl-ppcre:split regex text))

(defun basename (path)
  "Return the base name of PATH."
  (pathname-name path))

(defun basedir (path)
  "Return the parent directory of PATH."
  (mof:last* (pathname-directory path)))

(defun bangedp (string)
  "Return true if STRING ends with a bang character."
  (let ((length (length string)))
    (char= (elt string (1- length)) #\!)))

(defun strip-bang (string)
  "Return a new string without the bang character."
  (if (bangedp string)
      (let ((length (length string)))
        (subseq string 0 (1- length)))
      string))

(defmacro with-time (&body body)
  "Execute BODY then return timing information."
  `(time (progn ,@body (values))))
