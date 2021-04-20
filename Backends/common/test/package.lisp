;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2020 by Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;

(defpackage #:mcclim-backend-common.test
  (:use
   #:clim-lisp
   #:clim
   #:fiveam)

  (:shadowing-import-from #:fiveam
   #:test)

  (:export
   #:run-tests))

(in-package #:mcclim-backend-common.test)

(def-suite* :mcclim-backend-common)

(defun run-tests ()
  (run! :mcclim-backend-common))
