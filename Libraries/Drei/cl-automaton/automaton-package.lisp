;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2005-2007 Aleksandar Bakic <a_bakic@yahoo.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Package definitions for the automaton system.

(cl:defpackage #:eqv-hash
  (:use #:cl)
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

(cl:defpackage #:automaton
  (:nicknames #:cl-automaton)
  (:use #:cl #:eqv-hash)
  (:export
   #:string-regexp #:regexp-automaton
   #:run #:run-to-first-match #:run-to-first-unmatch
   #:state-equal #:automaton-equal #:regexp-equal))
