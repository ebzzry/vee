;;;; packages.lisp

(in-package #:cl-user)

(uiop:define-package #:muso/core
    (:use #:cl
          #:trivia)
  (:export #:boot-world
           #:read-file
           #:import-feed))

(uiop:define-package #:muso/tests
    (:use #:muso/core)
  ;; (:export #:run-tests)
  )
