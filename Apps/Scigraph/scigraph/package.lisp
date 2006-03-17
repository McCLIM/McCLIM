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

(in-package #-ansi-cl :user #+ansi-cl :common-lisp-user)

(defpackage TOOL
  #+MCL     (:shadow copy)
  (:use dwim-lisp))

(defpackage STATISTICS
  (:nicknames stat st)
  (:use dwim-lisp))

(defpackage GRAPH
  #-allegro (:nicknames gr)           ; "GR" names something already.
  (:shadow variable)                  ; shouldn't be inherited but is
  #+MCL     (:shadow copy)
  (:use dwim-lisp tool statistics))

(in-package :graph)

(declaim (declaration downward-funarg
		      downward-function
		      array-register))


(dwim:make-command-table :graph)
