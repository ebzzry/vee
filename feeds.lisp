;;;; feeds.lisp

(in-package #:muso/core)

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

(defun similarity (lcol rcol &key (selector #'elt0) (test #'string-equal))
  "Return in % how similar are LCOL and RCOL."
  (let ((common (length (nintersection (mapcar selector lcol)
                                       (mapcar selector rcol)
                                       :test test)))
        (total (length (map-nappend selector lcol rcol))))
    (* (/ common (/ total 1.0)) 100)))
