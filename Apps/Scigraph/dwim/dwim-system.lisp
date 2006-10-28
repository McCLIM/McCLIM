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
SYSTEM DWIM

DWIM is an interface to graphical user interface systems.  We are concerned (so far) 
with Dynamic Windows (DW) and CLIM.  This system is written for programs that:
 1.  Were originally written with DW in mind,
 2.  But now want to run in clim on other hardware platforms,
 3.  But want to retain backward compatibility with DW.

The policy of DWIM is to provide the most commonly used functionality from the
symbolics interface in a package from which applications may inherit symbols.  
Application packages generally would inherit from DWIM, CL, and CLOS.
Porting source code is then largely a process of removing package prefixes.  

Much of the syntax and nomenclature of CLIM is also provided by DWIM.  In many cases,
however, name conflicts occur (e.g. with-output-as-presentation), and in such cases
the DW syntax will be preferred.  The bias does not reflect a belief that DW is
better, but rather that most applications will be going from DW to CLIM and not 
the other way, therefore porting effort is minimized.

It is not our goal to define a third interface management language, but rather
to provide a tool for easing the porting process.

There are several exceptions to this rule of DW portability.
 a.  DWIM is small so far, so the more obscure functions from TV and DW may not
      yet be there.  We expect this library to grow as more applications use it.
 b.  SEND will not be supported by DWIM.  In these cases, DWIM will provide a 
      function.  So for example, (send stream :line-height) would be rewritten
      as (stream-line-height stream).  Generally, the name of the function will
      be the name used by CLIM.
 c.  Some DW macros, such as define-program-framework, are too hard to parse 
      and will not be supported in DWIM.

DWIM-LISP is what user programs should use as the lisp package.  It's purpose is to
handle all the hairy importing and shadowing constraints so that users don't have
to go through this every time they define a package that uses dwim stuff.
DWIM-LISP exports Common Lisp, CLOS, and the relevant DWIM symbols.
User package definitions should look very simple, e.g.
    (in-package 'my-package :use '(dwim-lisp))

PACKAGES:  DWIM, DWIM-LISP.
SUBSYSTEMS:  None.
|#

#-clim
(eval-when (compile load eval)
  (when (find-package 'clim) 
    (pushnew :clim *features*))) ; Add a CLIM feature.

(eval-when (compile load eval)
  ;; CLIM 1 doesn't affect the *features*.  Here's a rule of thumb
  ;; that seems to work.
  (when 
      (and (find-package 'clim)
	   (not (boundp (intern "CLIM-VERSION" 'clim))) ; from clim 0.9
	   (not (fboundp (intern "STREAM-CURSOR-POSITION" 'clim))) ; from clim 2
	   (not (member :clim-2 *features*))
	   (not (member :clim-0.9 *features*)))
    (pushnew :clim-1 *features*)
    (pushnew :clim-1.0 *features*)))

(defun file-type-for-sources ()
  #+MCL #.(pathname-type *.lisp-pathname*)
  #+genera "LISP"
  #+unix "lisp"
  #+(and (not mcl) (not genera) (not unix)) (error "Not yet implemented."))

(defun file-type-for-binaries ()
  #+MCL                      #.(pathname-type *.fasl-pathname*)
  #+genera                   si:*default-binary-file-type*
  #+(or allegro sbcl)        #.(if (fboundp 'compile-file-pathname)
				   (pathname-type (compile-file-pathname "foo"))
				 "fasl")
  #+scl                      (pathname-type (compile-file-pathname "foo"))
  #+lucid                    (car lcl:*load-binary-pathname-types*)
  #+(and (not genera)
         (not allegro)
         (not lucid)
         (not mcl)
	 (not sbcl))
  (error "Not yet implemented."))

#+genera
(setq *load-pathname* 
  (make-pathname :defaults si:fdefine-file-pathname
		 :name nil :type nil :version nil))



(defun suggest-bin-directory (&optional (base *load-pathname*)
                                        (prefix "BIN-"))
  ;; The number of different binaries you must have is
  ;; the cross product of the instruction set and the gui.
  (let ((instruction-set
         #+MCL              "MCL"
         #+GENERA           "GENERA"
         #+LUCID            "LUCID"
         #+ALLEGRO          "ALLEGRO"
	 #+SBCL             "SBCL"
	 #+scl              "SCL")
        (GUI
         #+(and mcl (not clim)) "MAC"
         #+(and genera (not clim)) "DW"
         #+clim-0.9                "CLIM-0-9"
         #+clim-1.0                "CLIM-1-0"
         #+clim-1.1                "CLIM-1-1"
         #+clim-2                  "CLIM-2"))
    (namestring (make-pathname
                 :directory
                 (append
                  (if base (pathname-directory base) '(:relative)) 
                  (list (string-downcase
                         (format nil "~A~A-~A"
                                 prefix 
                                 instruction-set
                                 gui))))))))

(defsys:defsystem dwim
    (:default-pathname *load-pathname*
	:default-binary-pathname (suggest-bin-directory)
	:default-optimizations ()
	:patch-file-pattern NIL 
	:needed-systems ()
	:load-before-compile ())
  ("package")
   ("feature-case" :load-before-compile ("package"))
   ("macros" :load-before-compile ("feature-case"))
   ("tv" :load-before-compile ("macros"))
   ("draw" :load-before-compile ("tv"))
   ("present" :load-before-compile ("draw"))
   ("extensions" :load-before-compile ("present"))
   ("wholine"  :load-before-compile ("extensions"))
   ("export"  :load-before-compile ("wholine"))
  )
