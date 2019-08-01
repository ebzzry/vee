;;; dump.lisp

(in-package #:ujo/core)

(defun dump-registry (registry &key simple)
  "Dump the contents of the tables from REGISTRY."
  (if simple
      (with-slots (rid name ecounter etable ucounter utable vcounter vtable xid control) registry
        (format t "~&RID: ~A~%NAME: ~S~%ECOUNTER: ~A~%ETABLE: ~A~%UCOUNTER: ~A~%UTABLE: ~A~%VCOUNTER: ~A~%VTABLE: ~A~%XID: ~A~%CONTROL: ~A~%"
                rid name ecounter etable ucounter utable vcounter vtable xid control))
      (progn (format t "~&** ENTRIES~%")
             (maphash #'(lambda (k v)
                          (with-slots (vid id prev next fields buriedp) v
                            (let ((fmt "~S => ~S~%")
                                  (slots (list vid id prev next fields buriedp)))
                              (format t fmt k slots))))
                      (etable registry))
             (format t "~&** VOLUMES~%")
             (maphash #'(lambda (k v)
                          (with-slots (rid vid name table prev next) v
                            (format t "~S => ~S~%" k
                                    (list rid vid name table prev next))))
                      (vtable registry)))))

(defun dump-registries ()
  "Display inforamtion about the registries."
  (format t "~&** REGISTRIES~%")
  (maphash #'(lambda (k v)
               (with-slots (rid name vcounter ecounter ucounter) v
                 (format t "~S => ~S~%" k (list rid name vcounter ecounter ucounter))))
           (rtable *world*)))

(defun dump-world ()
  "Dump the contents of the world."
  (let ((registries (loop :for v :being :the :hash-values :in (rtable *world*)
                          :collect v)))
    (loop :for registry :in registries
          :do (dump-registry registry))
    (when registries
      (dump-registries))
    (values)))

(defun dump-volume (volume &key complete (constraint '(0)))
  "Print information about VOLUME."
  (let ((registry (find-registry (rid volume))))
    (with-slots (rid vid name table prev next) volume
      (if complete
          (loop :for k :being :the :hash-keys :in (table volume)
                :for entry = (find-record k registry)
                :do (let ((match (first (gethash '(0) (matches entry))))
                          (values (fields-values (first (gethash constraint (matches entry))))))
                      (if match
                          (format t "~&~5A => ~20S = ~5A => ~20S ~%" k (fields-values entry)
                                  (id match)
                                  values)
                          (format t "~&~A => ~S~%" k (fields-values entry)))))
          (format t "~&RID: ~A~%VID: ~A~%NAME: ~A~%TABLE: ~A~%PREV: ~A~%NEXT: ~A~%"
                  rid vid name table prev next))
      (values))))

(defun dump-entry (entry &key complete)
  "Print information about an entry."
  (with-slots (vid id prev next fields buriedp) entry
    (if complete
        (format t "~&VID: ~S~%ID: ~S~%PREV: ~S~%NEXT: ~S~%FIELDS: ~S~%BURIEDP: ~S~%"
                vid id prev next (mapcar #'value fields) buriedp)
        (format t "~&~S~%" (mapcar #'value fields)))
    (values)))

(defun list-registries ()
  "List all the registries."
  (format t "~&~{~A~^~%~}" (find-registries)))

(defun list-volumes (&optional registry)
  "List all the volumes in all the registries."
  (if registry
      (format t "~&~{~A~^~%~}" (find-volumes (find-registry registry)))
      (loop :for registry :in (find-registries)
            :do (list-volumes registry))))

(defun list-stores ()
  "List all the registries and volumes."
  (list-registries)
  (list-volumes))
