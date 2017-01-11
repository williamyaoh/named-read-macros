(in-package #:cl-user)
(defpackage #:named-read-macros
  (:use #:cl)
  (:shadow #:readtable
           #:readtable-mixin)
  (:export #:readtable
           #:readtable-mixin
           #:define))
