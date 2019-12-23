;;;; unused.lisp

(in-package #:muse/core)

;;; feeds

(defun make-empty (datum)
  "Return an empty item based from DATUM."
  (let ((length (length datum)))
    (make-list length :initial-element *pad*)))

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

(defun add (left right acc)
  "Add LEFT and RIGHT to ACC."
  (cons (append left right) acc))

(defun advance (&rest items)
  "Return the other items from ITEMS. A wrapper around REST. This function will be converted to a method."
  (apply #'rest items))

(defun empty-left (datum)
  "Return a grouping wherein the left side is empty."
  (append (make-empty datum) datum))

(defun empty-right (datum)
  "Return a grouping wherein the right side is empty."
  (append datum (make-empty datum)))

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
  "Merge together the feeds in LFEED and RFEED."
  (let ((length1 (length lfeed))
        (length2 (length rfeed)))
    (cond ((= length1 length2) (merge-equal lfeed rfeed))
          ((< length1 length2)
           (append (merge-equal lfeed (subseq rfeed 0 length1))
                   (fill-blanks (subseq rfeed (1- length1)) 'left)))
          (t
           (append (merge-equal (subseq lfeed 0 length2) rfeed)
                   (fill-blanks (subseq lfeed (1- length2)) 'right))))))

(defun uniques (feed &key (selector #'elt0) (test #'string-equal))
  "Return the unique items from FEED."
  (delete-duplicates (normalize-items (mapcar selector feed)) :test test))

(defun minimal-common-p (lvol rvol &key (selector #'elt0) (test #'string-equal))
  "Return true if there are common lines between LVOL and RVOL."
  (flet ((fn (arg) (uniques arg :selector selector :test test)))
    (when (intersection (fn lvol) (fn rvol) :test test)
      t)))

(defun similarity (lvol rvol &key (selector #'elt0) (test #'string-equal))
  "Return in % how similar are LVOL and RVOL."
  (let ((common (length (nintersection (mapcar selector lvol)
                                       (mapcar selector rvol)
                                       :test test)))
        (total (length (map-nappend selector lvol rvol))))
    (* (/ common (/ total 1.0)) 100)))


;;; constraints

(defclass column ()
  ((value :initarg :value
          :initform ()
          :reader value
          :documentation "The value of a column"))
  (:documentation "A collection of cells from a volume"))

(defun make-column (value)
  "Return a column object."
  (make-instance 'column :value value))

(defun extract-column (index volume)
  "Return a column object from VOLUME specified by 1-indexed INDEX."
  (flet ((fn (pool)
           (nth index (cells pool))))
    (let* ((pools (walk-down volume :skip #'unitp))
           (value (mapcar #'fn pools)))
      (make-column value))))

(defun view-column (index volume)
  "Return a column from volume in a readable form."
  (let ((value (mapcar #'value (cells (extract-column index volume)))))
    (format t "~{~S~^ ~}" value)))

(defgeneric full (object)
  (:documentation "Return the full, unsectioned version of OBJECT."))
(defmethod full ((object list)) object)
(defmethod full ((object pool)) object)

(defgeneric apply-selectors (query selectors)
  (:documentation "Apply items in selectors to create a new value."))
(defmethod apply-selectors ((query list) selectors)
  (loop :for fn :in selectors :collect (funcall fn query)))
(defmethod apply-selectors ((query pool) selectors)
  (apply-selectors (mapcar #'value (cells query)) selectors))

(defun cell-index (cell header)
  "Return the index of CELL in HEADER."
  (loop :for h :in header
        :for count = 0 :then (1+ count)
        :when (string-equal cell h)
          :return count))

;;; propagation

(defun nth-frame (n volume &key (origin #'volume-start))
  "Return the 0-indexed Nth frame in VOLUME."
  (when (linkedp volume)
    (loop :for rec = (point origin volume) :then (next rec)
          :for count = 0 :then (1+ count)
          :when (= count n)
            :return rec)))
