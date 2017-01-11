(in-package #:named-read-macros)

(defun read-maybe-read-macro (stream &optional char)
  (declare (ignorable char))
  (if (char= (peek-char t stream t nil t) #\) )
      (read-delimited-list #\) stream t)
      (let ((head (read stream t nil t)))
        (if (and (symbolp head) (get head 'dispatch-macro))
            (progn (peek-char t stream t nil t)
                   (funcall (get head 'dispatch-macro) stream))
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
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'dispatch-macro)
             (lambda (,stream)
               (let ((*standard-input* ,stream))
                 ,@body)))
       ',name)))
