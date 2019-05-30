;;;; walker.lisp

(in-package #:muso/core)

(defun walk (lcol rcol acc &key (lcarry nil) (rcarry nil))
  "Walk through the columns and build value."
  (let ((lhead (or lcarry (current-head lcol)))
        (rhead (or rcarry (current-head rcol))))
    (cond
      ;; NOTE: handle cases wherein one of the data columns is already null
      ;; NOTE: this means that one of the coulmns still has trailing data

      ;; NOTE: complete the length of rcol. This will be the amount of empty
      ;; entries that will be added on the left side, to acc, which will then be
      ;; returned
      ((null lcol)
       (let ((rlength (length rcol)))
         ;; FIXME
         rlength))

      ((and (null lcol) (null rcol))
       (nreverse acc))

      ;; NOTE: The amount of look ahead is the amount of pair combinations
      ;;       that must be tested.

      ;; NOTE: It also determines the amount of jump to the next subset of
      ;;       column

      ;; complete
      ;; When both LHEAD and RHEAD match, they must be matching. Both LCOL
      ;; and RCOL will move. LCARRY and RCARRY must be cleared out while
      ;; advancing.
      ((complete-match-p lhead rhead)
       (walk (advance lcol)
             (advance rcol)
             (add (top lcol) (top rcol) acc)
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
            (partial-match-p (join-next lcol) rhead))
       (walk (advance (advance lcol))
             rcol
             acc
             :lcarry (join-next lcol)
             :rcarry nil))

      ;; and partial complete
      ;; When the LHEAD and RHEAD match,
      ;; and the join and RHEAD completely match,
      ;; it means that there will be a complete match. This complete
      ;; matching will be handled in the next iteration.

      ;; NOTE: how is this condition different from the one above?

      ((and (partial-match-p lhead rhead)
            (complete-match-p (join-next lcol) rhead))
       ;; TODO: verify for correctness
       (walk (advance (advance lcol))
             rcol
             acc
             :lcarry (join-next lcol)
             :rcarry nil))

      ;; inverse and partial partial

      ;; inverse and partial complete

      ;; TODO: describe fallback
      (t nil))))

(defun stats ()
  "Display information about similarities, differences, holes, inconsistencies,etc."
  nil)

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
;;; - At what part should I start aligning, wherein no information will be lost
;;; - Handle garbage
