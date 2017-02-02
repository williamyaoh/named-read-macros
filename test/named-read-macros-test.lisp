(in-package #:cl-user)

(defpackage #:named-read-macros-test
  (:use #:cl #:fiveam)
  (:export #:nrm-test))
(in-package #:named-read-macros-test)

;; Trying to test this package is a little bit weird, since there's the
;; possibility of errors at _read_ time, which means they'll happen before
;; our testing framework actually sees them. Instead, we'll read from a file,
;; so that our read-time errors happen at run-time.

;; Each file in the subdirectory `test-cases/` should be a text file, containing
;; two Lisp forms. The second of these is a form using a named read macro, which
;; is what we're testing.

;; The _first_ is a form which should evaluate to another form, which is then
;; compared EQUAL against the results of READing the named read macro.

(defun trim-whitespace (string)
  (string-trim
   #.(format nil "~{~C~}" (mapcar #'code-char '(#x09 #x0A #x0B #x0C #x0D #x20)))
   string))

(named-read-macros:define escapify
  (trim-whitespace
   (with-output-to-string (out)
     (loop for char = (read-char *standard-input* nil nil t)
           while char
           do (write-char char out)))))

(defparameter *test-case-directory*
  (uiop:subpathname (asdf:system-source-directory '#:named-read-macros-test)
                    "test-cases/"))

(test nrm-test
  (let ((*readtable* (named-readtables:find-readtable 'named-read-macros:readtable))
        (*package* (find-package '#:named-read-macros-test)))
    (dolist (filename (uiop:directory-files *test-case-directory*))
      (with-open-file (*standard-input* filename :direction :input)
        (is (equal (eval (read)) (read))
            "Forms don't READ to same thing in test case~A"
            (file-namestring filename))))))
