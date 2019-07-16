;;;; muso.asd

#-asdf3.1 (error "ASDF 3.1 or bust!")

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
               #:mof
               #:closer-mop
               #:cl-cpus
               #:lparallel)
  :serial t
  :components ((:file "packages")
               (:file "preload")
               (:file "globals")
               (:file "classes")
               (:file "common")
               (:file "world")
               (:file "clos")
               (:file "dump")
               (:file "void")
               (:file "unit")
               (:file "constraints")
               (:file "levenshtein")
               (:file "import")
               (:file "startup")
               (:file "tests")))
