(in-package :cl-user)
(defpackage :prompt-for(:use :cl)
  (:export
    #:prompt-for
    #:do-with-prompt-input
    ))
(in-package :prompt-for)

(defgeneric prompt-for(target &rest args))

(defmethod prompt-for :around (target &rest args)
  (declare(ignore args))
  (LET*((*STANDARD-INPUT* 	*QUERY-IO*)
	(*STANDARD-OUTPUT* 	*QUERY-IO*)
	(*PRINT-ARRAY*		t)
	(*PRINT-BASE*		10)
	(*PRINT-CASE*		:UPCASE)
	(*PRINT-CIRCLE*		t)
	(*PRINT-ESCAPE*		t)
	(*PRINT-GENSYM*		t)
	(*PRINT-LENGTH*		nil)
	(*PRINT-LEVEL*		nil)
	(*PRINT-LINES*		nil)
	(*PRINT-MISER-WIDTH*	nil)
	(*PRINT-PRETTY*		nil)
	(*PRINT-RADIX*		nil)
	(*PRINT-READABLY*	nil)
	(*PRINT-RIGHT-MARGIN*	nil))
    (call-next-method)))

(defmacro do-with-prompt-input(((out &rest format-args)
				(var read-form))
			       &body body)
  `(PROG((,out *STANDARD-OUTPUT*)
	 ,var)
     :REC
     (APPLY #'FORMAT T ,@format-args)
     (FORCE-OUTPUT)
     (HANDLER-BIND((ERROR(LAMBDA(C)
			   (FORMAT T "~&Invalid input. ~S"C)
			   (CLEAR-INPUT) ; ecl need especially symbol is input with unknown package specified.
			   (GO :REC))))
       (SETF ,var ,read-form))
     (CLEAR-INPUT)
     ,@body
     (GO :REC)))

(defmethod prompt-for((target symbol)&rest args)
  "Ensure user input is type of TARGET."
  (multiple-value-bind(reader args)(retrieve-keyword-arg :by args #'read)
    (do-with-prompt-input((out args)
			  (in(funcall reader)))
      (if(typep in target)
	(return in)
	(format out"~&~S is type of ~S, not ~S."in(type-of in)target)))))

(defun retrieve-keyword-arg(key args &optional default)
  (if(eq key (car args)) ; for short cut.
    (values(second args)(cddr args))
    (labels((rec(list)
	      (if(endp(cdr list))
		(values default args)
		(if(eq key (second list))
		  (values(third list)(progn (rplacd list (cdddr list))
					    args))
		  (rec(cdr list))))))
      (rec args))))

(defmethod prompt-for((target list)&rest args)
  "Ensure user input is type of TARGET."
  (multiple-value-bind(reader args)(retrieve-keyword-arg :by args #'read)
    (do-with-prompt-input((out args)
			  (in(funcall reader)))
      (if(typep in target)
	(return in)
	(format out"~&~S is type of ~S, not ~S."in(type-of in)target)))))

(defmethod prompt-for((pred function)&rest args)
  "Ensure user input satisfies PRED."
  (multiple-value-bind(reader args)(retrieve-keyword-arg :by args #'read)
    (do-with-prompt-input((out args)
			  (in(funcall reader)))
      (if(funcall pred in)
	(return in)
	(format out"~&~S is type of ~S, not satisfies ~S."
		in(type-of in) pred)))))
