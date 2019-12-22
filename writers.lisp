;;;; writers.lisp

(in-package #:honeycomb/core)

(defun write-file (volume file &key expand)
  "Print the contents of VOLUME to FILE. If EXPAND is true, blobs and volumes will expand to their original forms."
  (with-open-file (out (m:expand-pathname file)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format out "~&~{~S~^,~}" (header volume))
    (loop :for pool :in (walk-down volume :skip #'unitp)
          :do (format out "~&~{~S~^,~}" (cells-values pool :expand expand)))))

(defun filter-csv-file (infile outfile terms)
  "Remove duplicates in INFILE under TERMS then save the changes to OUTFILE."
  (let* ((volume-name (basename infile))
         (registry-name (basedir infile))
         (volume (import-csv-file infile :extract-header t
                                         :volume-name volume-name
                                         :registry-name registry-name)))
    (expunge-duplicates volume terms)
    (write-file volume outfile :expand t)))
