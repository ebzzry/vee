;;;; init.lisp

(in-package #:muso)

(defvar *source-1* (read-source-file (mof:expand-pathname "~/l/testado/notre-dame.spacy.50.tsv") t))
(defvar *source-2* (read-source-file (mof:expand-pathname "~/l/testado/notre-dame.linkup.50.tsv")))
