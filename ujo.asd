;;;; ujo.asd

#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage #:ujo-system
  (:use #:cl #:asdf))

(in-package #:ujo-system)

(defsystem :ujo
  :name "ujo"
  :version "0.0.2"
  :license "MIT"
  :description "An experimental graph system"
  :author "Rommel MARTINEZ <ebzzry@ebzzry.io>"
  :depends-on (#:uiop
               #:cl-ppcre
               #:cl-csv
               #:cl-nlp
               #:mof
               #:closer-mop
               #:cl-cpus
               #:lparallel)
  :serial t
  :components ((:file "src/packages")
               (:file "src/globals")
               (:file "src/classes")
               (:file "src/nlp")
               (:file "src/setup")
               (:file "src/common")
               (:file "src/world")
               (:file "src/clos")
               (:file "src/dump")
               (:file "src/void")
               (:file "src/unit")
               (:file "src/constraints")
               (:file "src/matching")
               (:file "src/import")
               (:file "src/initialize")
               (:file "src/tests")))
