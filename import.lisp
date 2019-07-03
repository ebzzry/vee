;;;; import.lisp

(in-package #:muso/core)

(defun import-feed (feed &key volume-name registry-name extract-header header)
  "Import items from FEED to REGISTRY with VOLUME-NAME and REGISTRY-NAME as names for the volume and the registry, respectively. If EXTRACT-HEADER is used, the header found in FEED will be used as volume header. If HEADER is used, "
  (let* ((volume-name (build-name "VOLUME" volume-name))
         (registry (spawn-registry registry-name))
         (volume (forge-volume registry volume-name))
         (body (if extract-header (rest feed) feed)))
    (setf (header volume) (if extract-header (first feed) header))
    (forge-entries volume registry body)
    (link-records volume)
    registry))

(defun import-file (path &key (volume-name (pathname-name path))
                              (registry-name *default-registry-name*)
                              (delimiter *default-delimiter*)
                              extract-header
                              header)
  "Import a disk file into the registry."
  (let* ((file (mof:expand-pathname path))
         (feed (read-file file :delimiter delimiter))
         (registry (find-registry registry-name))
         (name (if (find-volume volume-name registry)
                   (genstring "VOLUME")
                   volume-name)))
    (import-feed feed :volume-name name :registry-name registry-name
                      :extract-header extract-header :header header)))

(defun split-field (field &optional (regex "\\s+"))
  "Split a field into components."
  (cl-ppcre:split regex (value field)))

(defun make-feed (field &rest args)
  "Create a feed from the text value stored in FIELD using predefined rules."
  (mapcar #'list (apply #'split-field field args)))

(defun import-field (constraint &key volume-name registry-name header)
  "Import the texts specified by CONSTRAINT from the volume and registry indicators."
  (loop :for field :in (apply-constraints
                        (find-volume volume-name (find-registry registry-name))
                        (list constraint)
                        :merge t)
        :do (import-feed (make-feed field)
                         :volume-name volume-name
                         :registry-name registry-name
                         :header header)))
