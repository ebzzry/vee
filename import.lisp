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
                      :extract-header extract-header :header header)
    (find-volume name (find-registry registry-name))))

(defun split-field (field &optional (regex "\\s+"))
  "Split a field into components."
  (normalize-words (split-text (value field) regex)))

(defun make-feed (field)
  "Create a feed from the text value stored in FIELD using predefined rules."
  (let* ((items (split-field field))
         (list (sort (remove-duplicates items :test #'string-equal) #'string<)))
    (mapcar #'list list)))

(defun import-field (field &key (volume-name (make-volume-name))
                                registry-name
                                header
                                (return 'REGISTRY))
  "Import a single FIELD to a new volume VOLUME-NAME and registry REGISTRY-NAME."
  (when (stringp (value field))
    (import-feed (make-feed field)
                 :volume-name volume-name
                 :registry-name registry-name
                 :header header
                 :return return)))

