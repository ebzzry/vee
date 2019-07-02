;;;; bindings.lisp

(in-package #:muso/core)

;;; Note: snaking bindings
(defun bind-all-matches (registry entry &rest args)
  "Bind all matching records."
  (let ((matches (apply #'find-similar-entries registry entry args)))
    (when matches
      (setf (matches entry) matches))))

;;; Note: non-snaking bindings
(defun bind-first-matches ()
  "Bind all first matching records."
  nil)

(defun bind-volume (registry volume &rest args)
  "Bind VOLUME to other volumes of REGISTRY."
  (loop :for entry :in (walk-down volume :skip #'unitp)
        :do (apply #'bind-all-matches registry entry args)))

(defun bind-wall (registry &rest args)
  "Bind the wall in REGISTRY to the other volumes."
  (let ((wall (wall registry)))
    (apply #'bind-volume wall registry :exclusive t args)))

(defun bind-volumes (registry &rest args)
  "Bind all the volumes in REGISTRY to one another."
  (let ((volumes (find-volumes registry)))
    (loop :for volume :in volumes :do (apply #'bind-volume volume registry args))))
