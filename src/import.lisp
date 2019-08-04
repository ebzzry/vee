;;;; import.lisp

(in-package #:ujo/core)

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

(defun import-csv-file (path &key (volume-name (basename path))
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

(defun filter-text (text &optional (regex "\\s+"))
  "Run TEXT through a pre-defined filter."
  (sort (remove-duplicates (mapcar #'string-downcase (normalize-words (split-text text regex)))
                           :test #'string-equal)
        #'string<))

(defun split-field (field &optional (regex "\\s+"))
  "Split a field into components."
  (normalize-words (split-text (value field) regex)))

(defun make-feed (field &key transform)
  "Create a feed from the text value stored in FIELD using predefined rules."
  (if transform
      (mapcar #'list (filter-text (value field)))
      (mapcar #'list (split-text (value field)))))

(defun import-field (field &key (volume-name (make-volume-name))
                                registry-name
                                header
                                transform
                                (return 'REGISTRY))
  "Import a single FIELD to a new volume VOLUME-NAME and registry REGISTRY-NAME."
  (when (stringp (value field))
    (import-feed (make-feed field :transform transform)
                 :volume-name volume-name
                 :registry-name registry-name
                 :header header
                 :return return)))

(defun field-text (entry constraints)
  "Return processed text from ENTRY under CONSTRAINTS."
  (let ((text (value (first (apply-constraints entry constraints)))))
    (split-text text "\\s+")))

(defun import-flat-file (path &key (volume-name (basename path))
                                   (registry-name (basedir path))
                                   (header '("element")))
  "Import a plain text file containing prose text into the registry."
  (let* ((data (slurp-file path))
         (items (split-text data))
         (feed (mapcar #'list items))
         (registry (find-registry registry-name))
         (name (if (find-volume volume-name registry)
                   (make-volume-name)
                   volume-name)))
    (import-feed feed :volume-name name :registry-name registry-name :header header)
    (find-volume name (find-registry registry-name))))

(defun filter-flat-file ()
  "Perform filtering on plain file."
  nil)

(defvar *punctuations* '(#\. #\? #\! #\, #\: #\;)
  "List of common punctuations marks.")

(defun depunctuate (string)
  "Remove trailing punctuation mark from string."
  (let* ((length (length string))
         (last (aref string (- length 1))))
    (if (member last *punctuations*)
        (subseq string 0 (- length 1))
        string)))

(defun depunctuate-strings (strings)
  "Remove trailing punctuation marks from strings."
  (mapcar #'depunctuate strings))

(defun filter-flat-text (text &optional (regex "\\s+"))
  "Run TEXT through a pre-defined filter."
  ;; (sort (mapcar #'string-downcase (depunctuate-strings (split-text text regex)))
  ;;       #'string<)
  (depunctuate-strings (split-text text regex)))

(defun import-flat-text (text &key volume-name
                                   registry-name
                                   (header '("element")))
  "Import a plain text containing prose text into the registry."
  (let* ((items (filter-flat-text text))
         (feed (mapcar #'list items))
         (registry (or (find-registry registry-name) (build-registry)))
         (rname (name registry))
         ;; (vname (handler-case (find-volume volume-name registry)
         ;;          (error () (make-volume-name))))
         (vname (or volume-name (make-volume-name))))
    (import-feed feed :volume-name vname :registry-name rname :header header)
    (find-volume vname (find-registry rname))))

;;; Note: display stats about the matches
(defun stats ()
  ""
  nil)
