; vim: ft=lisp et
(in-package :asdf)

(defsystem :prompt-for
  :author "Shinichi Sato"
  :components((:file "prompt-for")))

;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "prompt-for"))))
  (test-system :prompt-for.test))
