;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2005 Aleksandar Bakic <a_bakic@yahoo.com>
;;;  (c) copyright 2006 Troels Henriksen <athas@sigkill.dk>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Package definition for Drei tests.

(cl:defpackage #:drei-tests
  (:use #:clim-lisp #:it.bese.fiveam #:drei-buffer #:drei-base #:drei-motion
        #:drei-editing #:automaton #:eqv-hash #:drei-core #:drei-kill-ring
        #:drei-syntax #:drei #:esa #:esa-utils #:clim #:drei-lisp-syntax)
  (:shadowing-import-from #:it.bese.fiveam #:test)
  (:shadowing-import-from #:automaton #:run)
  (:shadowing-import-from #:drei-lisp-syntax #:form)
  (:export #:run-tests
           #:*run-self-compilation-test*))
