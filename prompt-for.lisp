(in-package :cl-user)

(defpackage :prompt-for
  (:use :cl)
  (:export #:prompt-for #:do-with-prompt-input))

(in-package :prompt-for)

(defgeneric prompt-for (target &rest args))

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
          (((out format-args) (var reader)) &body body)
  "(DO-WITH-PROMPT-INPUT ((out { format-args }*) (var reader)) { body }*)
  out := SYMBOL which will be bound by the output stream.
  format-args := An expression to generate a list of format control and its arguments for prompt.
  var := SYMBOL which will be bound by the return value of the READER.
  reader := An expression to generate (FUNCTION (STREAM) T) to read user input.
  body := implicit progn.
Achieving the context to handle any errors, especially about user input,
evaluate the body form iteratively until CL:RETURN is successfully called."
  (let ((?reader (gensym "READER")) (?format-args (gensym "FORMAT-ARGS")))
    `(prog ((,out *standard-output*) ,var (,?reader ,reader)
            (,?format-args ,format-args))
      :rec
       (apply #'format t ,?format-args)
       (force-output)
       (handler-bind ((error
                        (lambda (c)
                          (format t "~&Invalid input. ~S" c)
                          (clear-input) ; ecl need especially symbol is input
                                        ; with unknown package specified.
                          (go :rec))))
         (setf ,var (funcall ,?reader *query-io*)))
      ,@body
       (go :rec))))

(defmethod prompt-for ((target symbol) &rest args)
  "Ensure user input is type of TARGET."
  (multiple-value-bind (reader args)
      (retrieve-keyword-arg :by args #'read)
    (do-with-prompt-input ((out args) (in reader))
      (if (typep in target)
          (return in)
          (format out "~&~S is type of ~S, not ~S." in (type-of in) target)))))

(defun retrieve-keyword-arg (key args &optional default)
  "Return two values.
  1. The value of the KEY in ARGS if exists, otherwise DEFAULT.
  2. ARGS which is lacking the KEY value pair.
NOTE: Only check last two conses. Any KEY in front of it is ignored.
e.g. (retrieve-keyword-arg :by '(:by 1 :by 2 :by 3)) => 3, (:by 1 :by 2)"
  (labels ((rec (list)
             (if (not (endp (cdddr list)))
                 (rec (cdr list))
                 (if (eq key (cadr list))
                     (values (caddr list) (progn (rplacd list nil) args))
                     (values default args)))))
    (rec args)))

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
      #+(and :sbcl :unix)
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
      (warn
        "PROMPT-FOR: the method specialized :SECRET is not supported ~A in ~S."
        (lisp-implementation-type) (uiop:detect-os))
      '(defun secret-reader ()
         (error "not supported ~A in ~S" (lisp-implementation-type)
                (uiop:detect-os))))
