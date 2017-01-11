(in-package #:asdf)
(defsystem #:named-read-macros
  :depends-on ("named-readtables")
  :serial t
  :components ((:file "packages")
               (:file "named-read-macros")
               (:file "readtables")))
