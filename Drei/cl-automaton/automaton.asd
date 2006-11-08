;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package #:asdf)

(defsystem #:automaton
  :depends-on ("rt")
  :components
  ((:file "automaton-package")
   (:file "eqv-hash" :depends-on ("automaton-package"))
   (:file "state-and-transition" :depends-on ("eqv-hash"))
   (:file "automaton" :depends-on ("state-and-transition" "eqv-hash"))
   (:file "regexp" :depends-on ("automaton"))))