;;;; init.lisp

(defvar *column-1* (read-file (mof:expand-pathname "~/l/testado/c-1.tsv")))
(defvar *column-2* (read-file (mof:expand-pathname "~/l/testado/c-2.tsv")))

(defvar *column-3* (read-file (mof:expand-pathname "~/l/testado/notre-dame.spacy.tsv")))
(defvar *column-4* (read-file (mof:expand-pathname "~/l/testado/notre-dame.linkup.tsv")))
