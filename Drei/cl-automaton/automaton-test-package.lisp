;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(defpackage #:eqv-hash-user
  (:use :cl :rtest #:eqv-hash))
(defpackage #:automaton-user
  (:use :cl :rtest #:automaton #:eqv-hash))