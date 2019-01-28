(defpackage :prompt-for.spec
  (:use :cl :jingoh :prompt-for))
(in-package :prompt-for.spec)
(setup :prompt-for)

(requirements-about PROMPT-FOR)

;;;; Description:
; Type safe user input.

#+syntax
(PROMPT-FOR &rest sb-pcl::args) ; => result

#?(with-input-from-string(in "0 foo")
    (let((*query-io*(make-two-way-stream in *query-io*)))
      (prompt-for 'symbol "Type symbol >> ")))
=> FOO
,:stream NIL
; In example above, first input `0` is discarded.
; Second input `foo` satisfies type.
;;;; Argument Precedence Order:
; target

;;;; Method signature:

#+signature(PROMPT-FOR (PRED FUNCTION) &REST ARGS)
; Prompt untill user input value which satisifes PRED comes.
#?(with-input-from-string(in "0 foo")
    (let((*query-io*(make-two-way-stream in *query-io*)))
      (prompt-for #'symbolp "Type symbol >> ")))
=> FOO
,:stream NIL

#+signature(PROMPT-FOR (TARGET LIST) &REST ARGS)
; Specify compound type specifier.
#?(with-input-from-string(in "0 foo")
    (let((*query-io*(make-two-way-stream in *query-io*)))
      (prompt-for '(eql foo) "Type foo >> ")))
=> FOO
,:stream NIL

#+signature(PROMPT-FOR (TARGET SYMBOL) &REST ARGS)
; Specify user input type name.
#?(with-input-from-string(in "0 foo")
    (let((*query-io*(make-two-way-stream in *query-io*)))
      (prompt-for 'integer "Type integer >> ")))
=> 0
,:stream NIL

;;;; Arguments and Values:

; args := {format-control format-argument* [{:by reader+}]}

; format-control := string

; format-argument ;= T

; reader := function as (function()T)
; Specify reader function which consume from `*standard-input*`.
; The default is #'READ.
#?(with-input-from-string(in "0 foo")
    (let((*query-io*(make-two-way-stream in *query-io*)))
      (prompt-for 'string "Type string >> " :by #'read-line)))
=> "0 foo"
,:stream NIL
,:test string=

; result := T ; user input which satisifes TARGET.

;;;; Affected By:
; `*QUERY-IO*`

;;;; Side-Effects:
; Reading/Writing `*QUERY-IO*`.
#?(with-input-from-string(in "0 foo")
    (let((*query-io*(make-two-way-stream in *query-io*)))
      (prompt-for 'integer "Type integer >> ")))
:outputs "Type integer >> "
,:stream *query-io*

#?(with-input-from-string(in "0 foo")
    (let((*query-io*(make-two-way-stream in *query-io*)))
      (prompt-for 'symbol "Type symbol >> ")))
:outputs #.(format nil "Type symbol >> ~%0 is type of ~S, not SYMBOL.Type symbol >> "(type-of 0))
,:stream *query-io*

;;;; Notes:
; Return value is just user input value.
#?(with-input-from-string(in "foo/bar.lisp")
    (let((*query-io*(make-two-way-stream in *query-io*)))
      (prompt-for (lambda(x)(pathname x))
		  "Type input file >> "
		  :by #'read-line)))
=> "foo/bar.lisp"
,:stream NIL
,:test string=

#?(with-input-from-string(in "foo/bar.lisp")
    (let((*query-io*(make-two-way-stream in *query-io*)))
      (prompt-for #'pathnamep "Type input file >> "
		  :by (lambda()(pathname(read-line))))))
=> #P"foo/bar.lisp"
,:stream NIL
,:test equalp

;;;; Exceptional-Situations:

(requirements-about DO-WITH-PROMPT-INPUT)

;;;; Description:
; Dsl macro for writing prompt-for.

#+syntax
(DO-WITH-PROMPT-INPUT ((out &rest format-args) (var read-form)) &body body) ; => result

#?(do-with-prompt-input((0 1)(2 3))4)
:expanded-to
(PROG((0 *STANDARD-OUTPUT*)
      2)
  :REC
  (APPLY #'FORMAT T 1)
  (FORCE-OUTPUT)
  (HANDLER-BIND((ERROR(LAMBDA(PROMPT-FOR::C)
			(FORMAT T "~&Invalid input. ~S"PROMPT-FOR::C)
			(CLEAR-INPUT)
			(GO :REC))))
    (SETF 2 3))
  (CLEAR-INPUT)
  4
  (GO :REC))

;;;; Arguments and Values:

; out := symbol, whith will be bound output stream.
; Not evaluated.

; format-args := { format-control+ format-argument*}
; format-control := string, evaluated.
; format-argument := T, evaluated.

; var := symbol, not evaluated.
; Bound by result of READ-FORM.

; read-form := form

; body := implicit-progn

; result := T, as user input.

;;;; Affected By:
; `*query-io*`

;;;; Side-Effects:
; Reading/Writing `*query-io*`.

;;;; Notes:
; Body must CL:RETURN explicitly.

;;;; Exceptional-Situations:

