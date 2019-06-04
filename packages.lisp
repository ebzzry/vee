;;;; packages.lisp

(in-package #:cl-user)

(uiop:define-package #:muso/core
    (:use #:cl
          #:trivia)
  (:nicknames #:muso)
  (:export #:boot-world
           #:read-file
           #:import-feed))

(uiop:define-package #:muso/tests
    (:use #:cl
          #:muso/core)
  (:export #:run-tests))
