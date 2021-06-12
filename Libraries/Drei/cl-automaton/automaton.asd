;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2005 Aleksandar Bakic <a_bakic@yahoo.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; System definition for the automaton system.

(defsystem "automaton"
  :components
  ((:file "automaton-package")
   (:file "eqv-hash" :depends-on ("automaton-package"))
   (:file "state-and-transition" :depends-on ("eqv-hash"))
   (:file "automaton" :depends-on ("state-and-transition" "eqv-hash"))
   (:file "regexp" :depends-on ("automaton"))))
