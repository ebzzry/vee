;;;; unit.lisp

(in-package #:muso/core)

(defun make-unit (vid registry)
  "Create an instance of the unit class."
  (make-instance 'unit :vid vid :registry registry))

(defun forge-unit (volume registry)
  "Create a unit under VOLUME within REGISTRY."
  (let* ((vid (vid volume))
         (unit (make-unit vid registry)))
    (add-record unit registry)
    (add-record unit volume)
    unit))

(defun link-unit-before (record volume registry)
  "Link a UNIT before RECORD in VOLUME within REGISTRY."
  (let ((unit (forge-unit volume registry)))
    (cond ((volume-start-p record)
           (progn (setf (next unit) record)
                  (setf (prev record) unit)))
          (t (progn (setf (prev unit) (prev record))
                    (setf (next unit) record)
                    (setf (next (prev record)) unit)
                    (setf (prev record) unit))))
    unit))

(defun link-unit-after (record volume registry)
  "Link a UNIT after RECORD in VOLUME within REGISTRY."
  (let ((unit (forge-unit volume registry)))
    (cond ((volume-end-p record)
           (progn (setf (prev unit) record)
                  (setf (next record) unit)))
          (t (progn (setf (prev unit) record)
                    (setf (next unit) (next record))
                    (setf (prev (next record)) unit)
                    (setf (next record) unit))))
    unit))

(defun link-unit (record volume registry &key (position :after))
  "Link a unit POSITION RECORD in VOLUME within REGISTRY."
  (when (keywordp position)
    (let ((fn (intern (mof:cat "LINK-UNIT-" (string position)))))
      (funcall fn record volume registry))))

(defun link-units (record volume registry count &key (position :after))
  "Link COUNT units POSITION RECORD in VOLUME within REGISTRY."
  (flet ((link (rec vol reg pos)
           (link-unit rec vol reg :position pos)))
    (loop :for count :from 1 :to count
          :for unit = (link record volume registry position)
            :then (link unit volume registry position)
          :finally (return volume))))
