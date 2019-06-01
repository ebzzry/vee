;;;; globals.lisp

(in-package #:muso/core)

(defvar *threshold* 5
  "The amount of lines to consider when trying to perform resoultion, backwards and forwards.")

(defvar *join-limit* 2
  "The amount of lines forward to join.")

;;; Note: fix or remove this abomination
(defvar *empty-item* (make-list 2 :initial-element "")
  "An empty data set.")

(defvar *similarity-threshold* 40.0
  "The minimum amount of similarity between sets so that they can be considered similar.")

(defvar *initial-rcounter* 100
  "The initial rcounter value")

(defvar *initial-ccounter* 1000
  "The initial ccounter value")

(defvar *initial-counter* 10000
  "The initial counter value")

(defvar *selector-limit* 1000
  "The amount of ELT-selectors to define.")

(defparameter *world* nil
  "The top-level structure which contains all the registries.")
