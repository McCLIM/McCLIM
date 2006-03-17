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

(in-package :user)

#|
Scigraph is a system for graphing numeric data.  It is not a program per se,
but rather a toolbox that can be embedded in other Common Lisp applications.
There have been many versions of scigraph.  This version is designed to run 
both in dynamic windows and in CLIM interface environments.  Much of this is 
done with macros, therefore we must keep separate binaries representing 
instantiations of this system with and without CLIM.

The sources should be portable enough to run on any platform that runs CLIM
(e.g. Lucid Lisp).  Of course the binaries are both machine-dependent and 
GUI-dependent.

See the file scigraph.doc for user documentation.
|#

#+genera
(setq *load-pathname* 
  (make-pathname :defaults si:fdefine-file-pathname
		 :name nil :type nil :version nil))

(defsys:defsystem
    scigraph
    (:default-pathname *load-pathname*
	:default-binary-pathname (suggest-bin-directory)

	:default-package "USER"
	:default-optimizations ()
	:patch-file-pattern NIL
	:needed-systems (dwim)
	:load-before-compile (dwim))
  ("package") 
  ("copy" :load-before-compile ("package")) 
  ("dump" :load-before-compile ("copy")) 
  ("duplicate" :load-before-compile ("dump")) 
  ("random" :load-before-compile ("duplicate")) 
  ("menu-tools" :load-before-compile ("random")) 
  ("basic-classes" :load-before-compile ("menu-tools")) 
  ("draw" :load-before-compile ("basic-classes")) 
  ("mouse" :load-before-compile ("draw")) 
  ("color" :load-before-compile ("mouse")) 
  ("basic-graph" :load-before-compile ("color")) 
  ("graph-mixins" :load-before-compile ("basic-graph")) 
  ("axis" :load-before-compile ("graph-mixins")) 
  ("moving-object" :load-before-compile ("axis")) 
  ("symbol" :load-before-compile ("moving-object")) 
  ("graph-data" :load-before-compile ("symbol")) 
  ("legend" :load-before-compile ("graph-data")) 
  ("graph-classes" :load-before-compile ("legend")) 
  ("present" :load-before-compile ("graph-classes")) 
  ("annotations" :load-before-compile ("present")) 
  ("annotated-graph" :load-before-compile ("annotations")) 
  ("contour" :load-before-compile ("annotated-graph")) 
  ("equation" :load-before-compile ("contour")) 
  ("popup-accept" :load-before-compile ("equation")) 
  ("popup-accept-methods" :load-before-compile ("popup-accept")) 
  ("duplicate-methods" :load-before-compile ("popup-accept-methods")) 
  ("frame" :load-before-compile ("duplicate-methods")) 
  ("export" :load-before-compile ("frame")) 
  ("demo-frame" :load-before-compile ("export")))
