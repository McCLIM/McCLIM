;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018-2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Utility functions for displaying disassembled code of functions.
;;;

(cl:in-package #:clouseau)

;;; The standard DISASSEMBLE function will print out
;;; implementation-specific things to *STANDARD-OUTPUT* in an
;;; implementation-specific way. This is generally optimized for
;;; console output, and is therefore not the most aesthetically
;;; pleasing way for CLIM to display it.

(defun display-disassembly (function stream)
  (with-style (stream :disassembly)
    (let ((*standard-output* stream))
      (disassemble function))))
