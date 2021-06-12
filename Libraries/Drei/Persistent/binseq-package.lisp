;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;; (c) copyright 2005 Aleksandar Bakic <a_bakic@yahoo.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Package definition for the binseq system.

(cl:defpackage #:binseq
  (:use #:common-lisp)
  (:export
   #:binseq-p
   #:list-binseq
   #:binseq-list
   #:vector-binseq
   #:binseq-vector
   #:binseq-empty
   #:binseq-length
   #:binseq-front
   #:binseq-back
   #:binseq-get
   #:binseq-set
   #:binseq-sub
   #:binseq-cons
   #:binseq-snoc
   #:binseq-append
   #:binseq-insert
   #:binseq-insert*
   #:binseq-remove
   #:binseq-remove*

   #:obinseq-p
   #:list-obinseq
   #:obinseq-list
   #:vector-obinseq
   #:obinseq-vector
   #:obinseq-empty
   #:obinseq-length
   #:obinseq-front
   #:obinseq-back
   #:obinseq-get
   #:obinseq-set
   #:obinseq-sub
   #:obinseq-cons
   #:obinseq-snoc
   #:obinseq-append
   #:obinseq-insert
   #:obinseq-insert*
   #:obinseq-remove
   #:obinseq-remove*

   #:binseq2-p
   #:list-binseq2
   #:binseq2-list
   #:vector-binseq2
   #:binseq2-vector
   #:binseq2-empty
   #:binseq2-length
   #:binseq2-size
   #:binseq2-front
   #:binseq2-offset
   #:binseq2-back
   #:binseq2-front2
   #:binseq2-line2
   #:binseq2-back2
   #:binseq2-get
   #:binseq2-set
   #:binseq2-get2
   #:binseq2-set2
   #:binseq2-sub
   #:binseq2-sub2
   #:binseq2-cons
   #:binseq2-snoc
   #:binseq2-append
   #:binseq2-insert
   #:binseq2-insert2
   #:binseq2-insert*
   #:binseq2-insert*2
   #:binseq2-remove
   #:binseq2-remove2
   #:binseq2-remove*
   #:binseq2-remove*2))
