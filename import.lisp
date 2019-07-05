;;;; import.lisp

(in-package #:muso/core)

(defun import-feed (feed &key volume-name registry-name extract-header header (return 'REGISTRY))
  "Import items from FEED to REGISTRY with VOLUME-NAME and REGISTRY-NAME as names for the volume and the registry, respectively. If EXTRACT-HEADER is used, the header found in FEED will be used as volume header. If HEADER is used, "
  (let* ((volume-name (build-name "VOLUME" volume-name))
         (registry-name (build-name "REGISTRY" registry-name))
         (registry (spawn-registry registry-name))
         (volume (forge-volume registry volume-name))
         (body (if extract-header (rest feed) feed)))
    (setf (header volume) (if extract-header (first feed) header))
    (forge-entries volume registry body)
    (link-records volume)
    (ecase return
      ((volume) volume)
      ((registry) registry))))

(defun import-file (path &key (volume-name (basename path))
                              (registry-name (basedir path))
                              (delimiter *default-delimiter*)
                              extract-header
                              header)
  "Import a disk file into the registry."
  (let* ((file (mof:expand-pathname path))
         (feed (read-file file :delimiter delimiter))
         (registry (find-registry registry-name))
         (name (if (find-volume volume-name registry)
                   (make-volume-name)
                   volume-name)))
    (import-feed feed :volume-name name :registry-name registry-name
                      :extract-header extract-header :header header)))

(defun split-field (field &optional (regex "\\s+"))
  "Split a field into components."
  (split-text (value field) regex))

(defun make-feed (field &key remove-duplicates)
  "Create a feed from the text value stored in FIELD using predefined rules."
  (let* ((items (split-field field))
         (list (if remove-duplicates
                   (remove-duplicates items :test #'string-equal)
                   items)))
    (mapcar #'list list)))

(defun import-field (field &key (volume-name (make-volume-name))
                                registry-name
                                header
                                (return 'REGISTRY)
                                remove-duplicates)
  "Import a single FIELD to a new volume VOLUME-NAME and registry REGISTRY-NAME."
  (import-feed (make-feed field :remove-duplicates remove-duplicates)
               :volume-name volume-name
               :registry-name registry-name
               :header header
               :return return))

(defun import-fields (constraint volume &key volume-name
                                             (registry-name (make-registry-name))
                                             (header (list constraint))
                                             (return 'REGISTRY)
                                             remove-duplicates)
  "Import the texts specified by CONSTRAINT from the volume and registry indicators."
  (loop :for field :in (apply-constraints volume (list constraint) :merge t)
        :do (import-field field :volume-name volume-name
                                :registry-name registry-name
                                :header header
                                :return return
                                :remove-duplicates remove-duplicates)))
