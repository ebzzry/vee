;;;; globals.lisp

(in-package #:muso/core)

(defvar *threshold* 5
  "The amount of lines to consider when trying to perform resoultion, backwards and forwards.")

(defvar *join-limit* 2
  "The amount of lines forward to join.")

(defvar *pad* nil
  "Padding data for items.")

(defvar *similarity-threshold* 40.0
  "The minimum amount of similarity between sets so that they can be considered similar.")

(defvar *initial-rcounter* 100
  "The initial registry counter value")

(defvar *initial-ccounter* 1000
  "The initial column counter value")

(defvar *initial-ecounter* 10000
  "The initial entry counter value")

(defvar *initial-ucounter* 1000000
  "The initial unit counter value")

(defvar *selector-limit* 1000
  "The amount of ELT-selectors to define.")

(defparameter *world* nil
  "The top-level structure which contains all the registries.")

(defvar *field-test* #'string=
  "The test function that will be used to compare fields.")

(defvar *default-selectors* (list #'elt0)
  "The default list of functions that are used for specifying clusters.")
