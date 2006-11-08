;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; ASDF system definition for Drei (only tests at the moment, see
;;; mcclim.asd for the definition of the DREI-MCCLIM system.
;;; NOTE: Make sure RT is loaded before loading the DREI.TESTS system!

(defpackage :drei.system
  (:use :cl :asdf))

(in-package :drei.system)

(defsystem :drei.tests
  :depends-on (:drei-mcclim)
  :components
  (#+nil(:file "rt" :pathname #p"testing/rt.lisp")
   (:file "buffer-test" :depends-on (#+nil"rt"))
   (:file "base-test" :depends-on (#+nil"rt" "buffer-test"))
   (:file "kill-ring-test" :depends-on ("buffer-test"))
   (:module
    "cl-automaton"
    :depends-on (#+nil"rt")
    :components
    ((:file "automaton-test-package")
     (:file "eqv-hash-test" :depends-on ("automaton-test-package"))
     (:file "state-and-transition-test" :depends-on ("automaton-test-package"))
     (:file "automaton-test" :depends-on ("automaton-test-package"))
     (:file "regexp-test" :depends-on ("automaton-test-package"))))))
