;;;; packages.lisp

(in-package #:cl-user)

(defpackage #:muso
  (:use #:cl #:trivia))

(defpackage #:muso-test
  (:use #:muso))
