; vim: ft=lisp et
(in-package :asdf)
(defsystem :prompt-for.test
  :version "1.0.3"
  :depends-on
  (:jingoh "prompt-for")
  :components
  ((:file "prompt-for"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :prompt-for)))
