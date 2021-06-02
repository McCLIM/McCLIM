;;;; mcclim-dot.test package
;;;;
;;;; This file is part of the mcclim-dot extension. See
;;;; Extensions/dot/README.md and Extensions/dot/LICENSE for more information.

(defpackage #:mcclim-dot.test
  (:use
   #:cl
   #:fiveam)

  (:export
   #:run-tests))

(in-package #:mcclim-dot.test)

(def-suite* :mcclim-dot)

(defun run-tests ()
  (run! :mcclim-dot))
