;;;; honeycomb.asd

#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage #:honeycomb-system
  (:use #:cl #:asdf))

(in-package #:honeycomb-system)

(defsystem :honeycomb
  :name "honeycomb"
  :version "0.0.3"
  :license "MIT"
  :description "An experimental hybrid knowledge system"
  :author "Rommel MARTINEZ <ebzzry@ebzzry.io>"
  :depends-on (#:uiop
               #:cl-ppcre
               #:cl-csv
               #:cl-nlp
               #:mof
               #:closer-mop
               #:cl-cpus
               #:lparallel
               #:cl-xlsx
               #+sbcl #:sb-sprof)
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
