;;;; muso.asd

#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage #:muso-system
  (:use #:cl #:asdf))

(in-package #:muso-system)

(defsystem :muso
  :name "muso"
  :version "0.0.1"
  :description ""
  :author "Rommel MARTINEZ <ebzzry@ebzzry.io>"
  :depends-on (#:uiop
               #:cl-ppcre
               #:trivia
               #:fare-csv
               #:mof
               #:trivia)
  :serial t
  :components ((:file "packages")
               (:file "globals")
               (:file "classes")
               (:file "core")
               (:file "registry")
               (:file "walker")
               (:file "tests")))
