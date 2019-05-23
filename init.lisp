;;;; init.lisp

(uiop:define-package #:muso/init
    (:use #:cl
          #:trivia
          #:muso/core)
  (:export #:*source-1*
           #:*source-2*))

(in-package #:muso/init)

(defvar *source-1* (read-source-file (mof:expand-pathname "~/l/testado/notre-dame.spacy.50.tsv") t))
(defvar *source-2* (read-source-file (mof:expand-pathname "~/l/testado/notre-dame.linkup.50.tsv")))
