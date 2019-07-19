;;;; packages.lisp

(in-package #:cl-user)

(uiop:define-package #:muso/core
    (:use #:cl)
  (:nicknames #:muso)
  (:export #:filter-file))

(uiop:define-package #:muso/tests
    (:use #:cl
          #:muso/core))
