;; -*- mode: common-lisp; package: user -*-

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

#-clim
(eval-when (compile load eval)
  (when (find-package 'clim) 
    (pushnew :clim *features*))) ; Add a CLIM feature.

;;; McCLIM tries to implement the spec for CLIM 2.0, so :clim-2 should
;;; generally be appropriate. Except when clim-2 is used to
;;; conditionalize access to internal CLIM functions...
#+mcclim
(eval-when (compile load eval)
  (pushnew :clim-2 *features*))

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
         #+(and :mcl (not :openmcl)) "MCL"
         #+GENERA           "GENERA"
         #+LUCID            "LUCID"
         #+ALLEGRO          "ALLEGRO"
	 #+OPENMCL	    "OPENMCL"
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
                  (if (and base (pathname-directory base))
		      (pathname-directory base)
		      '(:relative)) 
                  (list (string-downcase
                         (format nil "~A~A-~A"
                                 prefix 
                                 instruction-set
                                 gui))))))))

(defun compile-and-load-file (name)
  (let* ((source-dir (make-pathname :defaults *load-pathname*
				    :name name))
	 (source (make-pathname :defaults source-dir
				:type (file-type-for-sources)))
	 (bin-dir (suggest-bin-directory *load-pathname*))
	 (binary (make-pathname :defaults bin-dir
				:name name
				:type (file-type-for-binaries))))
    (ensure-directories-exist bin-dir)
    (when (or (not (probe-file binary))
	      (< (file-write-date binary) (file-write-date source)))
      (compile-file source :output-file binary))
    (load binary)))

(eval-when (load eval)
  (map nil #'compile-and-load-file 
    '(
      "package"
      "feature-case"
      "macros"
      "tv"
      "draw"
      "present"
      "extensions"
      "wholine"
      "export"      
      )))

