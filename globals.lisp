;;;; globals.lisp

(uiop:define-package #:muso/globals
    (:use #:cl #:trivia)
  (:export #:*threshold*
           #:*join-limit*
           #:*empty-entry*))

(in-package #:muso/globals)

(defvar *threshold* 5
  "The amount of lines to consider when trying to perform resoultion, backwards
  and forwards.")

(defvar *join-limit* 2
  "The amount of lines forward to join.")

(defvar *empty-entry* '("" "")
  "An empty data set.")

