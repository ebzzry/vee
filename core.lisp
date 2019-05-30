;;;; core.lisp

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
  "Return a normalized version of ENTRY."
  (string-downcase entry))

(defun normalize-items (subcol)
  "Return normalized items from COLUMN."
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

(defun count-entries (column)
  "Return the number of entries in column."
  (length column))

(defun count-entries-file (file)
  "Return the number of entries in COLUMN."
  (with-open-file (in file)
    (loop :for line = (read-line in nil)
          :while line
          :count line)))

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

(defun peek (column limit &key (selector #'elt0))
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

(defun prefixedp (string prefixes)
  "Return true if STRING is prefixed with PREFIX."
  (member (elt string 0) prefixes))

(defun underscoredp (string)
  "Return true if STRING is prefixed with an underscore."
  (prefixedp string '(#\ZERO_WIDTH_NO-BREAK_SPACE #\_)))

(defun fix-column (column)
  "Make corrections to entry due to the effects of file reading."
  (destructuring-bind (head &rest body)
      column
    (destructuring-bind (h &rest b)
        head
      (if (underscoredp h)
          (cons (cons (subseq h 1) b)
                body)
          column))))

(defun clean-column (column)
  "Remove extraneous entries in COLUMN."
  (remove-if #'(lambda (entry)
                 (some #'mof:empty-string-p entry))
             column))

(defun read-column-file (file)
  "Read source from file and perform clean-ups as necessary."
  (fix-column (clean-column (read-tsv-file file))))

(defun read-file (&rest args)
  "An alias to READ-COLUMN-FILE"
  (apply #'read-column-file args))

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

(defun current-head (column &key (selector #'elt0))
  "Return the head of the curent item in COLUMN."
  (funcall selector (current-entry column)))

(defun next-head (column &key (selector #'elt0))
  "Return the head of the next item in COLUMN."
  (funcall selector (next-entry column)))

(defun top (&rest args)
  "Return the top-most entry in column."
  (apply #'current-entry args))

(defun compact-string (string)
  "Return a new string without whitespaces."
  (cl-ppcre:regex-replace-all "\\s+" string ""))

(defun partial-match-p (text-1 text-2)
  "Return true if X is a partial match against Y. String compaction will happen with the longer column, which is usually Y."
  (strict-substring-p (compact-string text-1) (compact-string text-2)))

(defun complete-match-p (text-1 entry-2 &key (case-sensitive nil))
  "Return true if X and Y match completely."
  (and (= (length text-1) (length entry-2))
       (if case-sensitive
           (string= text-1 entry-2)
           (string-equal text-1 entry-2))))

(defun add (left right acc)
  "Add LEFT and RIGHT to ACC."
  (cons (append left right) acc))

(defun advance (&rest items)
  "Return the other items from ITEMS. A wrapper around REST. This function will be converted to a method."
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

(defun merge-equal (lcol rcol)
  "Merge equal columns."
  (assert (= (length lcol) (length rcol)) (lcol rcol))
  (loop :for n :from 0 :to (length lcol)
        :for (l1 l2) :in lcol
        :for (r1 r2) :in rcol
        :collect (list l1 l2 r1 r2)))

(defun merge-columns (lcol rcol)
  "Merge together the columns in LCOL and RCOL."
  (let ((length-1 (length lcol))
        (length-2 (length rcol)))
    (cond ((= length-1 length-2) (merge-equal lcol rcol))
          ((< length-1 length-2)
           (append (merge-equal lcol (subseq rcol 0 length-1))
                   (fill-blanks (subseq rcol (1- length-1)) 'left)))
          (t
           (append (merge-equal (subseq lcol 0 length-2) rcol)
                   (fill-blanks (subseq lcol (1- length-2)) 'right))))))

(defun uniques (column &key (selector #'elt0) (test #'string-equal))
  "Return the unique items from COLUMN."
  (delete-duplicates (normalize-items (mapcar selector column)) :test test))

(defun minimal-common-p (lcol rcol &key (selector #'elt0) (test #'string-equal))
  "Return true if there are common lines between LCOL and RCOL."
  (flet ((fn (data) (uniques data :selector selector :test test)))
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

(defun make-registry ()
  "Create an instance of the registry class."
  (make-instance 'registry))

(defgeneric reset-counter (registry)
  (:documentation "Reset the counter found in registry."))
(defmethod reset-counter ((r registry))
  (setf (counter r) *initial-counter*))

(defgeneric gen-counter (registry)
  (:documentation "Generate a new counter value from REGISTRY."))
(defmethod gen-counter ((r registry))
  (incf (counter r))
  (counter r))

(defgeneric add-entry (entry registry)
  (:documentation "Add entry to registry"))
(defmethod add-entry ((e entry) (r registry))
  (setf (gethash (counter r) (table r)) e))

(defun pad-column (column &optional (pad ""))
  "Add starting and ending padding for column based on the first element."
  (let* ((initial (elt0 column))
         (length (length initial))
         (pad (make-list length :initial-element pad)))
    (append (list pad) column (list pad))))

(defparameter *registry* (make-registry)
  "Initialize the global registry.")

(defgeneric reset-registry (registry)
  (:documentation "Reset the contents of registry."))
(defmethod reset-registry ((r registry))
  (setf (counter r) *initial-counter*)
  (setf (table r) (make-hash-table))
  (values))

(defgeneric dump-registry (registry)
  (:documentation "Dump the contents of table from REGISTRY."))
(defmethod dump-registry ((r registry))
  (maphash #'(lambda (k v)
               (format t "~S => ~S~%" k (list (id v) (prev v) (curr v) (next v) (column v))))
           (table r)))

(defun make-entry (id prev curr next)
  "Create an instance of the entry class."
  (make-instance 'entry :id id :prev prev :curr curr :next next))

(defun add-entries (column registry)
  "Add entries from COLUMN."
  (let ((col (pad-column column)))
    (loop :for prev :in col
          :for curr :in (rest col)
          :for next :in (rest (rest col))
          :do (add-entry (make-entry (gen-counter registry) prev curr next)
                         registry))
    (values)))
