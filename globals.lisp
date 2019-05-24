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

(defvar *empty-entry* (make-list 2 :initial-element "")
  "An empty data set.")

(defvar *similarity-threshold* 40.0
  "The minimum amount of similarity between sets so that they can be considered similar.")