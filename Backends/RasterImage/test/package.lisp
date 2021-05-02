;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;

(defpackage #:mcclim-raster-image.test
  (:use
   #:cl
   #:alexandria
   #:fiveam)

  (:export
   #:run-tests))

(in-package #:mcclim-raster-image.test)

(def-suite* :mcclim-raster-image)

(defun run-tests ()
  (run! :mcclim-raster-image))
