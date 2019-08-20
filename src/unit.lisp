;;;; unit.lisp

(in-package #:ujo/core)

(defun make-unit (vid registry)
  "Create an instance of the unit class."
  (make-instance 'unit :vid vid :registry registry))

(defun forge-unit (volume registry)
  "Create a unit under VOLUME within REGISTRY."
  (let* ((vid (vid volume))
         (unit (make-unit vid registry)))
    (add-object unit registry)
    (add-object unit volume)
    unit))

(defun link-unit-before (frame volume registry)
  "Link a UNIT before FRAME in VOLUME within REGISTRY."
  (let ((unit (forge-unit volume registry)))
    (cond ((volume-start-p frame)
           (progn (setf (next unit) frame)
                  (setf (prev frame) unit)))
          (t (progn (setf (prev unit) (prev frame))
                    (setf (next unit) frame)
                    (setf (next (prev frame)) unit)
                    (setf (prev frame) unit))))
    unit))

(defun link-unit-after (frame volume registry)
  "Link a UNIT after FRAME in VOLUME within REGISTRY."
  (let ((unit (forge-unit volume registry)))
    (cond ((volume-end-p frame)
           (progn (setf (prev unit) frame)
                  (setf (next frame) unit)))
          (t (progn (setf (prev unit) frame)
                    (setf (next unit) (next frame))
                    (setf (prev (next frame)) unit)
                    (setf (next frame) unit))))
    unit))

(defun link-unit (frame volume registry &key (position :after))
  "Link a unit POSITION FRAME in VOLUME within REGISTRY."
  (when (keywordp position)
    (let ((fn (intern (m:cat "LINK-UNIT-" (string position)))))
      (funcall fn frame volume registry))))

(defun link-units (frame volume registry count &key (position :after))
  "Link COUNT units POSITION FRAME in VOLUME within REGISTRY."
  (flet ((link (frm vol reg pos)
           (link-unit frm vol reg :position pos)))
    (loop :for count :from 1 :to count
          :for unit = (link frame volume registry position)
          :then (link unit volume registry position)
          :finally (return volume))))
