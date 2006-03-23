;;; -*- Syntax: Common-lisp; Package: User -*-
#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(in-package #+ansi-cl :common-lisp-user #-ansi-cl :user)

#+mcclim
(eval-when (compile load eval)
  (pushnew :clim-2 *features*))

(eval-when (compile load eval)
  (#+genera future-common-lisp::defpackage
   #-genera defpackage
   dwim
   #-clim
   (:shadowing-import-from "CLOS" "SETF" "DOCUMENTATION")
   ;;Get the ANSI Version of Loop.
   #+lucid
   (:shadowing-import-from "LOOP" "LOOP")
   #+kcl
   (:shadowing-import-from "SLOOP" "LOOP")
   #+genera
   (:shadowing-import-from "FUTURE-COMMON-LISP" "LOOP")

   (:shadow ignore-errors handler-case restart-case handler-bind 
	    invoke-restart find-restart with-simple-restart 
	    parse-error *default-server-path*)

   #+clim
   (:use clim-lisp)
   #-clim
   (:use lisp clos)))

(eval-when (compile load eval)
  #+genera
  (import '(future-common-lisp:defpackage) 'dwim)

  ;; GUI stuff we want imported.
  (import
    #+clim
    '(clim:present-to-string clim:presentation-type clim:present)
    #-clim
    '(dw:present-to-string dw:presentation-type dw:present scl:send)
    'dwim)

  ;;import the presentation types.
  (import #+clim
	  '(clim:boolean #+clim-0.9 clim:alist-member clim:expression clim:command)
	  #-clim
	  '(dw:boolean dw:alist-member sys:expression cp:command)
	  'dwim) 

  ;;declarations we want to work.
  #+lucid
  (import '(lcl:dynamic-extent) 'dwim)
  #+allegro
  (import '(excl::dynamic-extent) 'dwim)
  #+genera
  (import '(sys:downward-funarg sys:downward-function sys:array-register) 'dwim)
)

(eval-when (compile load eval)
  (proclaim '(declaration
	       ;; March 1989, X3J13 votes to subsume downward-funarg & downward-function
	       ;; by dynamic-extent.  Get rid of the next two eventually.  jpm.
	       dwim::downward-funarg dwim::downward-function
              #-ansi-cl
	       dwim::dynamic-extent
	       dwim::array-register)))

#+openmcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(declaration values)))

#+genera
(si:enable-who-calls :new)
#+genera
(TV:ENABLE-OBSOLETE-CONSOLE-COMPILER-WARNINGS)

;;; Need to load postscript stuff because it defines a package
;;; that dwim references.
#+(and clim-2 (not mcclim))
(eval-when (compile load eval)
  (require :climps))

