;;;; packages.lisp

(in-package #:cl-user)

(uiop:define-package #:muso/core
    (:use #:cl
          #:trivia)
  (:export #:read-file))

(uiop:define-package #:muso/tests
    (:use #:muso/core)
  ;; (:export #:run-tests)
  )
