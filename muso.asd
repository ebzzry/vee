;;;; muso.asd

#-ASDF3.1 (error "ASDF 3.1 or bust!")

(defpackage #:muso-system
  (:use #:cl #:asdf))

(in-package #:muso-system)

(defsystem :muso
  :name "muso"
  :version "0.0.1"
  :description ""
  :license ""
  :author "Rommel MARTINEZ <ebzzry@ebzzry.io>"
  :depends-on (#:uiop
               #:cl-ppcre
               #:fare-csv
               #:mof)
  :serial t
  :components ((:file "packages")
               (:file "globals")
               (:file "classes")
               (:file "common")
               (:file "world")
               (:file "propagation")
               (:file "feeds")
               ;; (:file "walker")
               (:file "tests")))
