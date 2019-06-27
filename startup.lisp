;;;; startup.lisp

(in-package #:muso/core)

(boot-world)

(setf *default-registry-name* (genstring "REGISTRY"))
