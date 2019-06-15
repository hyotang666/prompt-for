; vim: ft=lisp et
(in-package :asdf)

(defsystem :prompt-for
  :version "0.0.1"
  :author "Shinichi Sato"
  :description "Type safe user input."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :components((:file "prompt-for")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "prompt-for"))))
  (append (call-next-method)'((test-op "prompt-for.test"))))

(defmethod operate :around(o (c (eql (find-system "prompt-for")))
                             &key ((:compile-print *compile-print*))
                             ((:compile-verbose *compile-verbose*))
                             &allow-other-keys)
  (call-next-method))
