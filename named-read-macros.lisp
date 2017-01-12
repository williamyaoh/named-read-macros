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
              (if (and (string= (funcall case-transform-fn (subseq end-str 4))
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
