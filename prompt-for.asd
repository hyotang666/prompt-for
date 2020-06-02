; vim: ft=lisp et
(in-package :asdf)

(defsystem :prompt-for
  :version "2.0.3"
  :author "Shinichi Sato"
  :description "Type safe user input."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :depends-on
  (
   (:feature :sbcl (:require "sb-posix")) ; POSIX interface.
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
