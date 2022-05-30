# PROMPT-FOR 2.0.0
## What is this?
Type safe user input.

## Alternatives and differences.

|            | [duologue]       | prompt-for             |
| ---        | ----------       | ----------             |
| coloring   | \*               |                        |
| completion | \*               |                        |
| choosing   | \*               |                        |
| email      | \*               |                        |
| url        | \*               |                        |
| datetime   | \*               |                        |
| secret     |                  | sbcl with unix os only |
| types      | integer pathname | any-lisp-types         |
| interact   | standard-io      | query-io               |

[duologue]: https://github.com/mmontone/duologue/

## Usage

```lisp
* (prompt-for:prompt-for 'integer "~&Input integer> ")

Input integer> not-integer

NOT-INTEGER is type of SYMBOL, not INTEGER
Input integer> 0
0
```
For detail, see [spec file.](spec/prompt-for.lisp)

### TIPS

PROMPT-FOR is a generic function.
If the first argument is a function, it checks user input with it.

```lisp
* (prompt-for:prompt-for #'evenp "~&Input even integer>> ")

Input even integer>> 1

1 is type of BIT, not satisfies #<FUNCTION EVENP>.
Input even integer>> 2
2
```

Note that the lambda function will be useless as a message for users when failed.

```lisp
* (prompt-for:prompt-for (lambda (x) (evenp x)) "~&Input even integer>> ")

Input even integer>> 1

1 is type of BIT, not satisfies #<FUNCTION (LAMBDA (X)) {...}>.
```

In such a case, the local function may help you.

```lisp
* (flet ((my-evenp (x) (evenp x)))
    (prompt-for:prompt-for #'my-evenp "~&Input even integer>> "))

Input even integer>> 1

1 is type of BIT, not satisfies #<FUNCTION (FLET MY-EVENP) {...}>.
```

Note that these examples are in SBCL.
Other implementations may prints function in other ways.

If you want maximum portability, or customize the message,
you need to use the intermediate object and override method.

```lisp
(defstruct fun sym)

(defmethod prompt-for:prompt-for ((f fun) &rest args)
  (loop :for input = (progn (apply #'uiop:format! *query-io* args)
                            (read *query-io*))
        :when (funcall (fun-sym f) input)
	  :return input
	:else :do (format *query-io* "~&WTF dude! It does not satisfy ~S!" (fun-sym f))))

* (prompt-for:prompt-for (make-fun :sym 'evenp) "~&Input even integer>> ")

Input even integer>> 1

WTF dude! It does not satisfy EVENP!
Input even integer>>
```

### TIPS
`prompt-for` returns user input.

```lisp
* (prompt-for:prompt-for #'find-class "~&Input class name >> ")
Input class name >> stream
STREAM
```

If you want e.g. stream object rather than a name even if user input is a class name, you can use `prompt-for:*default-reader*`.

```lisp
* (let ((prompt-for:*default-reader*
          (lambda (input) (find-class (read input)))))
    (prompt-for:prompt-for t "~&Input class name >> "))
Input class name >> stream
#<SB-PCL:SYSTEM-CLASS COMMON-LISP:STREAM>
```

## From developer

### Product's goal
Already?
### License
MIT

### Tested with
* SBCL/2.2.4
* CCL/1.12
* CLISP/2.49
* ECL/21.2.1
* Allegro/10.1
* CMUCL/21D
* ABCL/1.9.0
