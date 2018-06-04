; vim: ft=lisp et
(in-package :asdf)

(defsystem :prompt-for
  :author "Shinichi Sato"
  :description "Type safe user input."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :components((:file "prompt-for")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "prompt-for"))))
  (append (call-next-method)'((test-op "prompt-for.test"))))
