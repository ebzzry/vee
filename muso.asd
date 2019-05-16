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
               #:mof)
  :components ((:file "package")
               (:file "maps")
               (:file "muso")))
