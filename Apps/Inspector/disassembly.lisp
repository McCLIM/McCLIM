(in-package :clouseau)

;; The DISASSEMBLE function will print out implementation-specific
;; things to *STANDARD-OUTPUT* in an implementation-specific way. This
;; is generally optimized for console output, and is therefore not the
;; most aesthetically pleasing way for CLIM to display it. This file
;; is where custom disassembly printers are defined. For each
;; implementation, you can define a pretty-printer for the disassembly
;; of a function.

;; With SBCL, we want to cut out the first two characters of every
;; line, which are always going to be "; ", and if anything is left
;; print it.
#+sbcl
(defun display-disassembly (object pane)
  (let ((disassembly-string (with-output-to-string (*standard-output*)
			      (disassemble object))))
    (with-input-from-string (stream disassembly-string)
      (with-text-family (pane :fix)
	(loop for line = (read-line stream nil nil)
	      while line
	      do (let ((shortened-line (subseq line 2)))
		   (when (> (length shortened-line) 0)
		     (fresh-line pane)
		     (princ shortened-line pane))))))))

;; For Lisps that don't have their own custom display function, we
;; just print the output of DISASSEMBLE and hope it looks decent.
#-sbcl
(defun display-disassembly (object pane)
  (let ((*standard-output* pane))
    (with-text-family (pane :fix)
      (fresh-line pane)
      (disassemble object))))