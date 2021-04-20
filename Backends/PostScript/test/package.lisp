;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2019 by Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;

(defpackage #:clim-postscript.test
  (:use
   #:cl
   #:fiveam)

  (:export
   #:run-tests))

(in-package #:clim-postscript.test)

(def-suite* :clim-postscript)

(defun run-tests ()
  (run! :clim-postscript))
