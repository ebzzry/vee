;;;; packages.lisp

(in-package #:cl-user)

(uiop:define-package #:vee/core
  (:use #:cl
        #:marie)
  (:nicknames #:vee)
  (:export #:import-csv-file
           #:filter-csv-file))

(uiop:define-package #:vee/tests
  (:use #:cl
        #:vee/core))
