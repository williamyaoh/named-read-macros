(in-package #:named-read-macros)

(named-readtables:defreadtable readtable-mixin
  (:macro-char #\( #'read-maybe-read-macro))
(named-readtables:defreadtable readtable
  (:fuse :standard readtable-mixin))
