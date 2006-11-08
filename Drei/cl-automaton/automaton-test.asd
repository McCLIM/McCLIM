;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package #:asdf)

(defsystem #:automaton-test
    :depends-on ("automaton")
    :components
    ((:file "automaton-test-package")
     (:file "eqv-hash-test" :depends-on ("automaton-test-package"))
     (:file "state-and-transition-test" :depends-on ("automaton-test-package"))
     (:file "automaton-test" :depends-on ("automaton-test-package"))
     (:file "regexp-test" :depends-on ("automaton-test-package"))))
