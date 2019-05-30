;;;; globals.lisp

(in-package #:muso/core)

(defvar *threshold* 5
  "The amount of lines to consider when trying to perform resoultion, backwards and forwards.")

(defvar *join-limit* 2
  "The amount of lines forward to join.")

(defvar *empty-entry* (make-list 2 :initial-element "")
  "An empty data set.")

(defvar *similarity-threshold* 40.0
  "The minimum amount of similarity between sets so that they can be considered similar.")

(defvar *initial-id* 1000
  "The initial value of *ID*")

(defvar *initial-counter* 1000
  "The initial counter value")

(defvar *id* *initial-id*
  "The running id for the entries.")

(defvar *selector-limit* 1000
  "The amount of ELT-selectors to define.")
