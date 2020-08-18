;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019-2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Package definition for unit tests of the clouseau system.
;;;

(cl:defpackage #:clouseau.test
  (:use
   #:cl
   #:clouseau
   #:fiveam)

  (:shadowing-import-from #:clouseau
   #:inspect)

  (:export
   #:run-tests))

(cl:in-package #:clouseau.test)

(def-suite :clouseau)

(defun run-tests ()
  (run! :clouseau))
