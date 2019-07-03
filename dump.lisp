;;;; dump.lisp

(in-package #:muso/core)

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

(defgeneric dump-volume (volume &key &allow-other-keys)
  (:documentation "Print information about VOLUME."))
(defmethod dump-volume ((v volume) &key complete)
  (let ((registry (find-registry (rid v))))
    (with-slots (rid vid name table prev next) v
      (if complete
          (loop :for k :being :the :hash-keys :in (table v)
                :for entry = (find-record k registry)
                :do (format t "~&~A => ~A~%" k entry))
          (format t "~&RID: ~A~%VID: ~A~%NAME: ~A~%TABLE: ~A~%PREV: ~A~%NEXT: ~A~%"
                  rid vid name table prev next))
      (values))))

(defgeneric dump-entry (entry &key &allow-other-keys)
  (:documentation "Print information about an entry."))
(defmethod dump-entry ((e entry) &key simple)
  (with-slots (vid id prev next fields buriedp) e
    (if simple
        (format t "~&PREV: ~S~%NEXT: ~S~%FIELDS: ~S~%BURIEDP: ~S~%"
                prev next fields buriedp)
        (format t "~&VID: ~S~%ID: ~S~%PREV: ~S~%NEXT: ~S~%FIELDS: ~S~%BURIEDP: ~S~%"
                vid id prev next fields buriedp))
    (values)))

(defun display-volume (query name)
  "Display the contents of volume QUERY in registry NAME."
  (dump-volume (find-volume query (find-registry name))))

(defun display-entry (query name)
  "Display the contents of entry QUERY in registry NAME."
  (dump-entry (find-record query (find-registry name))))
