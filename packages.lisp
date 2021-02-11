;;;; packages.lisp

(in-package #:cl-user)

(uiop:define-package #:veda/core
  (:use #:cl
        #:marie)
  (:nicknames #:veda)
  (:export #:import-csv-file
           #:filter-csv-file))

(uiop:define-package #:veda/tests
  (:use #:cl
        #:veda/core))
