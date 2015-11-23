;;; -*- lisp -*-

(cl:in-package :asdf-user)

(defsystem :clouseau
    :depends-on (:mcclim)
    :serial t
    :components
    ((:module "Apps/Inspector"
              :pathname #.(make-pathname :directory '(:relative "Apps" "Inspector"))
              :components
	      ((:file "package")
	       (:file "disassembly" :depends-on ("package"))
	       (:file "inspector" :depends-on ("disassembly"))))))
