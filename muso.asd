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
               #:cl-csv
               #:cl-nlp
               #:mof
               #:closer-mop
               #:cl-cpus
               #:lparallel)
  :serial t
  :components ((:file "packages")
               (:file "globals")
               (:file "nlp")
               (:file "setup")
               (:file "classes")
               (:file "common")
               (:file "world")
               (:file "clos")
               (:file "dump")
               (:file "void")
               (:file "unit")
               (:file "constraints")
               (:file "import")
               (:file "initialize")
               (:file "tests")))
