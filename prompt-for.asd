; vim: ft=lisp et
(in-package :asdf)

(defsystem :prompt-for
  :version "3.0.0"
  :author "SATO Shinichi"
  :description "Type safe user input."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :source-control (:git "git@github.com:hyotang666/prompt-for")
  :bug-tracker "https://github.com/hyotang666/prompt-for/issues"
  :license "MIT"
  :depends-on
  (
   (:feature :sbcl (:require "sb-posix")) ; POSIX interface.
   "uiop"       ; For detecting the OS. Implicitly depending on via asdf.
   )
  :components((:file "prompt-for")))

;; These forms below are added by JINGOH.GENERATOR.
(in-package :asdf)
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "prompt-for"))))
  (append (call-next-method) '((test-op "prompt-for.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "prompt-for")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
