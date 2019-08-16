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
    (forge-pools volume registry body)
    (link-frames volume)
    (ecase return
      ((volume) volume)
      ((registry) registry))))

(defun import-csv-file (path &key (volume-name (basename path))
                                  (registry-name (basedir path))
                                  (delimiter *default-delimiter*)
                                  extract-header
                                  header)
  "Import a CSV file into the world."
  (let* ((file (mof:expand-pathname path))
         (feed (read-csv-file file :delimiter delimiter))
         (registry (find-registry registry-name))
         (name (if (find-volume volume-name registry)
                   (make-volume-name)
                   volume-name)))
    (import-feed feed :volume-name name :registry-name registry-name
                      :extract-header extract-header :header header)
    (find-volume name (find-registry registry-name))))

(defun import-xlsx-file (path &key (volume-name (basename path))
                                   (registry-name (basedir path))
                                   extract-header
                                   header)
  "Import an XLSX file into the world."
  (let* ((file (mof:expand-pathname path))
         (feeds (read-xlsx-file file))
         (registry (find-registry registry-name)))
    (loop :for feed :in feeds
          :for count :from 1 :to (length feeds)
          :collect (let ((name (if (find-volume volume-name registry)
                                   (make-volume-name)
                                   (format nil "~A#~A" volume-name count))))
                     (import-feed feed :volume-name name
                                       :registry-name registry-name
                                       :extract-header extract-header
                                       :header header)
                     (find-volume name (find-registry registry-name))))))

(defun filter-text (text &optional (regex "\\s+"))
  "Run TEXT through a pre-defined filter."
  (sort (remove-duplicates (mapcar #'string-downcase (normalize-words (split-text text regex)))
                           :test #'string-equal)
        #'string<))

(defun split-cell (cell &optional (regex "\\s+"))
  "Split a cell into components."
  (normalize-words (split-text (value cell) regex)))

(defun make-feed (cell &key transform)
  "Create a feed from the text value stored in CELL using predefined rules."
  (if transform
      (mapcar #'list (filter-text (value cell)))
      (mapcar #'list (split-text (value cell)))))

(defun import-cell (cell &key (volume-name (make-volume-name))
                              registry-name
                              header
                              transform
                              (return 'REGISTRY))
  "Import a single CELL to a new volume VOLUME-NAME and registry REGISTRY-NAME."
  (when (stringp (value cell))
    (import-feed (make-feed cell :transform transform)
                 :volume-name volume-name
                 :registry-name registry-name
                 :header header
                 :return return)))

(defun cell-text (pool constraints)
  "Return processed text from POOL under CONSTRAINTS."
  (let ((text (value (first (apply-constraints pool constraints)))))
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
  (depunctuate-strings (split-text text regex)))

(defun import-flat-text (text &key volume-name
                                   registry-name
                                   (header '("element"))
                                   (regex "\\s+"))
  "Import a plain text containing prose text into the registry."
  (let* ((items (filter-flat-text text regex))
         (feed (mapcar #'list items))
         (registry (or (find-registry registry-name) (build-registry)))
         (rname (name registry))
         (vname (or volume-name (make-volume-name))))
    (import-feed feed :volume-name vname :registry-name rname :header header)
    (find-volume vname (find-registry rname))))

;;; Note: display stats about the matches
(defun stats ()
  ""
  nil)
