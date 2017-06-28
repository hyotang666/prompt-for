; vim: ft=lisp et
(in-package :asdf)

(defsystem :prompt-for
  :author "Shinichi Sato"
  :description "Type safe user input."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :components((:file "prompt-for")))

;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "prompt-for"))))
  (test-system :prompt-for.test))
