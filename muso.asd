;;;; muso.asd

(asdf:defsystem #:muso
  :description "Describe muso here"
  :author "Rommel MARTINEZ <ebzzry@ebzzry.io>"
  :license ""
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop
               #:cl-ppcre
               #:trivia
               #:fare-csv
               #:mof
               #:trivia)
  :components ((:file "packages")
               (:file "maps")
               (:file "init")
               (:file "classes")
               (:file "muso")))
