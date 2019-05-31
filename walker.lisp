;;;; walker.lisp

(in-package #:muso/core)

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

(defun similarity (lcol rcol &key (selector #'elt0) (test #'string-equal))
  "Return in % how similar are LCOL and RCOL."
  (let ((common (length (nintersection (mapcar selector lcol)
                                       (mapcar selector rcol)
                                       :test test)))
        (total (length (map-nappend selector lcol rcol))))
    (* (/ common (/ total 1.0)) 100)))

;;; Notes
;;;
;;; - A feed will only move when it is done processing
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
;;; - Build a new feed where the first two items are joined
;;; - At the moment, the walker will only work with two simultaneous feeds

;;; Notes
;;;
;;; - At what part should aligning happen, wherein no information will be lost
;;; - Handle garbage

;;; Notes
;;;
;;; - Inspect PREV and NEXT
;;; - The walker should operate on entry or column instances

(defun walk (lfeed rfeed acc &key (lcarry nil) (rcarry nil))
  "Walk through the columns and build value."
  (let ((lhead (or lcarry (current-head lfeed)))
        (rhead (or rcarry (current-head rfeed))))
    (cond
      ;; NOTE: handle cases wherein one of the data feeds is already null
      ;; NOTE: this means that one of the coulmns still has trailing data

      ;; NOTE: complete the length of rfeed. This will be the amount of empty
      ;; entries that will be added on the left side, to acc, which will then be
      ;; returned
      ((null lfeed)
       (let ((rlength (length rfeed)))
         ;; FIXME
         rlength))

      ((and (null lfeed) (null rfeed))
       (nreverse acc))

      ;; NOTE: The amount of look ahead is the amount of pair combinations
      ;;       that must be tested.

      ;; NOTE: It also determines the amount of jump to the next subset of
      ;;       feed

      ;; complete
      ;; When both LHEAD and RHEAD match, they must be matching. Both LFEED
      ;; and RFEED will move. LCARRY and RCARRY must be cleared out while
      ;; advancing.
      ((complete-match-p lhead rhead)
       (walk (advance lfeed)
             (advance rfeed)
             (add (current-item lfeed) (current-item rfeed) acc)
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
            (partial-match-p (join-next lfeed) rhead))
       (walk (advance (advance lfeed))
             rfeed
             acc
             :lcarry (join-next lfeed)
             :rcarry nil))

      ;; and partial complete
      ;; When the LHEAD and RHEAD match,
      ;; and the join and RHEAD completely match,
      ;; it means that there will be a complete match. This complete
      ;; matching will be handled in the next iteration.

      ;; NOTE: how is this condition different from the one above?

      ((and (partial-match-p lhead rhead)
            (complete-match-p (join-next lfeed) rhead))
       ;; TODO: verify for correctness
       (walk (advance (advance lfeed))
             rfeed
             acc
             :lcarry (join-next lfeed)
             :rcarry nil))

      ;; inverse and partial partial

      ;; inverse and partial complete

      ;; TODO: describe fallback
      (t nil))))

(defun stats ()
  "Display information about similarities, differences, holes, inconsistencies,etc."
  nil)
