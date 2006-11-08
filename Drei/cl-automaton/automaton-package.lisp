;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(defpackage #:eqv-hash
  (:use :cl)
  (:export
   #:hash
   #:eqv
   #:key-situation
   #:builtin-key-situation
   #:eq-key-situation
   #:+eq-key-situation+
   #:eql-key-situation
   #:+eql-key-situation+
   #:equal-key-situation
   #:+equal-key-situation+
   #:equalp-key-situation
   #:+equalp-key-situation+
   #:case-sensitive-key-situation
   #:+case-sensitive-key-situation+
   #:case-insensitive-key-situation
   #:+case-insensitive-key-situation+
   #:make-generalized-hash-table
   #:generalized-hash-table
   #:ht
   #:cnt
   #:situation
   #:htref
   #:htadd
   #:htremove
   #:htpresent
   #:with-ht
   #:with-ht-collect))
(defpackage #:automaton
  (:nicknames #:cl-automaton)
  (:use :cl #:eqv-hash)
  (:export
   #:string-regexp #:regexp-automaton
   #:run #:run-to-first-match #:run-to-first-unmatch
   #:state-equal #:automaton-equal #:regexp-equal))