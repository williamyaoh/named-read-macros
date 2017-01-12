;; Copyright (c) 2017, William Yao 
;; All rights reserved. 

;; Redistribution and use in source and binary forms, with or without 
;; modification, are permitted provided that the following conditions are met: 

;;  * Redistributions of source code must retain the above copyright notice, 
;;    this list of conditions and the following disclaimer. 
;;  * Redistributions in binary form must reproduce the above copyright 
;;    notice, this list of conditions and the following disclaimer in the 
;;    documentation and/or other materials provided with the distribution. 
;;  * Neither the name of  nor the names of its contributors may be used to 
;;    endorse or promote products derived from this software without specific 
;;    prior written permission. 

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
;; POSSIBILITY OF SUCH DAMAGE. 

(in-package #:named-read-macros)

(defun readtable-case-transform-fn (&optional (readtable *readtable*))
  "Return a function which will transform any string per the given
   readtable's case-sensitivity."
  (ecase (readtable-case readtable)
    (:upcase #'string-upcase)
    (:downcase #'string-downcase)
    (:preserve #'identity)
    (:invert (lambda (str)
               (let ((alphas (remove-if-not #'alpha-char-p str)))
                 (cond
                   ((every #'upper-case-p alphas) (string-downcase str))
                   ((every #'lower-case-p alphas) (string-upcase str))
                   (:otherwise str)))))))

(defun read-string-end (stream symbol)
  (let ((end-name (coerce (string-downcase
                           (concatenate 'string "END-" (symbol-name symbol)))
                          'list))
        (case-transform-fn (readtable-case-transform-fn)))
    (with-output-to-string (contents)
      (loop (let ((end-str (with-output-to-string (end-str)
                             (loop with pos = end-name
                                   while pos
                                   for char = (read-char stream t nil t)
                                   do (if (char= (char-downcase char) (car pos))
                                          (progn
                                            (write-char char end-str)
                                            (setf pos (cdr pos)))
                                          (progn
                                            ;; wipe END-STR contents
                                            (write-string (get-output-stream-string end-str)
                                                          contents)
                                            (write-char char contents)
                                            (setf pos end-name)))))))
              (if (and (string= (subseq (funcall case-transform-fn end-str) 4)
                                (symbol-name symbol))
                       (char= (peek-char nil stream t nil t) #\) ))
                  (progn (read-char) (return))
                  (write-string end-str contents)))))))

(defun check-if-bound (symb)
  (when (fboundp symb)
    (warn "Redefining function ~S as read macro." symb)
    (fmakunbound symb))
  (when (boundp symb)
    (warn "Redefining variable ~S as read macro." symb)
    (makunbound symb))
  (when (get symb 'read-macro)
    (warn "Redefining existing read macro ~S." symb)))

(defun read-maybe-read-macro (stream &optional char)
  (declare (ignorable char))
  (if (char= (peek-char t stream t nil t) #\) )
      (read-delimited-list #\) stream t)
      (let ((head (read stream t nil t)))
        (if (and (symbolp head) (get head 'read-macro))
            (progn (peek-char t stream t nil t)
                   (with-input-from-string (stream* (read-string-end stream head))
                     (funcall (get head 'read-macro) stream*)))
            (cons head (read-delimited-list #\) stream t))))))

(defmacro define (name &body body)
  "Allows for read macros to look the same as normal macros.
   That is, (NAME <CONTENT>) will be able to read CONTENT as a stream
   of characters, not as already-read Lisp forms, if NAME is defined
   as a named read macro.

   BODY should return a Lisp form, which will be used the same as a return
   from any other read macro. BODY will be executed in a context where
   *STANDARD-INPUT* is bound to the stream containing CONTENT."
  (let ((stream (gensym "STREAM")))
    `(eval-when (:load-toplevel :execute)
       (check-if-bound ',name)
       (setf (get ',name 'read-macro)
             (lambda (,stream)
               (let ((*standard-input* ,stream))
                 ,@body)))
       ',name)))
