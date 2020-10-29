;;;; vee.asd

#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage #:vee-system
  (:use #:cl #:asdf))

(in-package #:vee-system)

(defsystem :vee
  :name "vee"
  :version "1.0.0"
  :license "MIT"
  :description "An experimental hybrid knowledge system"
  :author "Rommel MARTINEZ <ebzzry@ebzzry.io>"
  :depends-on (#:uiop
               #:cl-ppcre
               #:cl-csv
               #:cl-nlp
               #:marie
               #:closer-mop
               #:cl-cpus
               #:lparallel
               #:cl-xlsx)
  :serial t
  :components ((:file "packages")
               (:file "globals")
               (:file "classes")
               (:file "nlp")
               (:file "setup")
               (:file "common")
               (:file "world")
               (:file "clos")
               (:file "dump")
               (:file "void")
               (:file "unit")
               (:file "constraints")
               (:file "matching")
               (:file "import")
               (:file "writers")
               (:file "initialize")
               (:file "tests")))
