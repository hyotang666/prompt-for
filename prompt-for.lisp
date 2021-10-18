(in-package :cl-user)

(defpackage :prompt-for
  (:use :cl)
  (:export #:prompt-for #:do-with-prompt-input))

(in-package :prompt-for)

(defgeneric prompt-for
    (target &rest args))

(defmethod prompt-for :around (target &rest args)
  (declare (ignore args target))
  (let* ((*standard-input* *query-io*)
         (*standard-output* *query-io*)
         (*print-array* t)
         (*print-base* 10)
         (*print-case* :upcase)
         (*print-circle* t)
         (*print-escape* t)
         (*print-gensym* t)
         (*print-length* nil)
         (*print-level* nil)
         (*print-lines* nil)
         (*print-miser-width* nil)
         (*print-pretty* nil)
         (*print-radix* nil)
         (*print-readably* nil)
         (*print-right-margin* nil))
    (call-next-method)))

(defmacro do-with-prompt-input
          (((out &rest format-args) (var reader)) &body body)
  `(prog ((,out *standard-output*) ,var)
    :rec
     (apply #'format t ,@format-args)
     (force-output)
     (handler-bind ((error
                      (lambda (c)
                        (format t "~&Invalid input. ~S" c)
                        (clear-input) ; ecl need especially symbol is input
                                      ; with unknown package specified.
                        (go :rec))))
       (setf ,var (funcall ,reader *query-io*)))
    ,@body
     (go :rec)))

(defmethod prompt-for ((target symbol) &rest args)
  "Ensure user input is type of TARGET."
  (multiple-value-bind (reader args)
      (retrieve-keyword-arg :by args #'read)
    (do-with-prompt-input ((out args) (in reader))
      (if (typep in target)
          (return in)
          (format out "~&~S is type of ~S, not ~S." in (type-of in) target)))))

(defun retrieve-keyword-arg (key args &optional default)
  (if (eq key (car args)) ; for short cut.
      (values (second args) (cddr args))
      (labels ((rec (list)
                 (if (endp (cdr list))
                     (values default args)
                     (if (eq key (second list))
                         (values (third list)
                                 (progn (rplacd list (cdddr list)) args))
                         (rec (cdr list))))))
        (rec args))))

(defmethod prompt-for ((target list) &rest args)
  "Ensure user input is type of TARGET."
  (multiple-value-bind (reader args)
      (retrieve-keyword-arg :by args #'read)
    (do-with-prompt-input ((out args) (in reader))
      (if (typep in target)
          (return in)
          (format out "~&~S is type of ~S, not ~S." in (type-of in) target)))))

(defmethod prompt-for ((pred function) &rest args)
  "Ensure user input satisfies PRED."
  (multiple-value-bind (reader args)
      (retrieve-keyword-arg :by args #'read)
    (do-with-prompt-input ((out args) (in reader))
      (handler-case
          (if (funcall pred in)
              (return in)
              (format out "~&~S is type of ~S, not satisfies ~S." in
                      (type-of in) pred))
        (error (condition)
          (format out "~&~A, breaks ~S." condition pred))))))

(defmethod prompt-for ((s (eql :secret)) &rest args)
  "Get user input string silently."
  (multiple-value-bind (reader args)
      (retrieve-keyword-arg :by args #'read)
    (do-with-prompt-input ((out args) (in (secret-reader reader)))
      (if (equal "" in)
          (format out "~&empty string is not allowed.")
          (return in)))))

;;; Stolen from https://stackoverflow.com/questions/39797560/common-lisp-how-to-mask-keyboard-input

#.(or ; in order to avoid #-
      #+sbcl
      '(progn
        (defun echo-off ()
          (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
            (setf (sb-posix:termios-lflag tm)
                    (logandc2 (sb-posix:termios-lflag tm) sb-posix:echo))
            (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm)))
        (defun echo-on ()
          (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
            (setf (sb-posix:termios-lflag tm)
                    (logior (sb-posix:termios-lflag tm) sb-posix:echo))
            (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm)))
        (defun secret-reader (reader)
          (lambda (stream)
            (declare (ignore stream))
            (echo-off)
            (unwind-protect
                (progn
                 (clear-input sb-sys:*tty*)
                 (funcall reader sb-sys:*tty*))
              (echo-on)))))
      ;; as default
      (warn "PROMPT-FOR: the method specialized :SECRET is not supported in ~A"
            (lisp-implementation-type))
      '(defun secret-input ()
         (error "not supported in ~A" (lisp-implementation-type))))