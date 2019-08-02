;;;; packages.lisp

(in-package #:cl-user)

(uiop:define-package #:ujo/core
    (:use #:cl)
  (:nicknames #:ujo)
  (:export #:import-csv-file
           #:filter-csv-file))

(uiop:define-package #:ujo/tests
    (:use #:cl
          #:ujo/core))
