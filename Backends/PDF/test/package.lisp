;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2019 by Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;

(defpackage #:clim-pdf.test
  (:use
   #:cl
   #:fiveam)

  (:export
   #:run-tests))

(in-package #:clim-pdf.test)

(def-suite* :clim-pdf)

(defun run-tests ()
  (run! :clim-pdf))
