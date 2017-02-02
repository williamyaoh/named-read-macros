(in-package #:cl-user)

(defpackage #:named-read-macros-test
  (:use #:cl #:fiveam)
  (:export #:named-read-macros-tests))
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

(def-suite named-read-macros-tests)
(in-suite named-read-macros-tests)

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

(defmacro finishes-nowarn (&body body)
  `(finishes (handler-case (progn ,@body)
               (warning (c)
                 (declare (ignore c))
                 (error "A warning occured while evaluating ~A."
                        ',body)))))

(defun unbind-read-macro% (symbol)
  (setf (get symbol 'named-read-macros::read-macro) nil))

(test redefinition-test
  (finishes-nowarn
   (named-read-macros:define *foo*
     nil                                ; nothing...
     ))

  (defparameter *bar* :bar)
  (signals warning
    (named-read-macros:define *bar*
      nil                               ; nothing...
      ))

  (defun baz ())
  (signals warning
    (named-read-macros:define baz
      nil                               ; nothing...
      ))

  (signals warning
    (named-read-macros:define *foo*
      nil                               ; nothing...
      ))

  (dolist (symbol '(*foo* *bar* baz))
    (unbind-read-macro% symbol))
  (makunbound '*bar)
  (fmakunbound 'baz))

;; For the same reason as above, we read forms from a file instead of
;; placing them in here. Test cases for readtable case are in `case-cases/`.
;; Before the two forms that get compared is a symbol indicating the readtable
;; case that these should get parsed in; if the first of the two compared forms
;; is a symbol, it's a condition type to get checked for instead.

(defparameter *case-case-directory*
  (uiop:subpathname (asdf:system-source-directory '#:named-read-macros-test)
                    "case-cases/"))

(named-read-macros:define |eScApIfY|
  (trim-whitespace
   (with-output-to-string (out)
     (loop for char = (read-char *standard-input* nil nil t)
           while char
           do (write-char char out)))))

(named-read-macros:define |escapify|
  (trim-whitespace
   (with-output-to-string (out)
     (loop for char = (read-char *standard-input* nil nil t)
           while char
           do (write-char char out)))))

(test readtable-case-test
  (let* ((*readtable* (named-readtables:find-readtable 'named-read-macros:readtable))
         (old-case (readtable-case *readtable*))
         (*package* (find-package '#:named-read-macros-test)))
    (dolist (filename (uiop:directory-files *case-case-directory*))
      (with-open-file (*standard-input* filename :direction :input)
        (let ((case (read))
              (spec (read)))
          (setf (readtable-case *readtable*) case)
          (if (symbolp spec)
              (eval `(signals ,spec (read)))
              (is (equal (eval spec) (read))
                  "Forms don't READ to same thing in test case~A"
                  (file-namestring filename))))
        ;; reset readtable case between each test
        (setf (readtable-case *readtable*) old-case)))))
