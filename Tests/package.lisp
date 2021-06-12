(cl:defpackage #:clim-tests
  (:use #:clim-lisp #:clim #:clime #:fiveam)
  (:shadowing-import-from #:fiveam #:test)
  (:import-from #:climi
                #:coordinate=
                #:basic-output-record
                #:null-bounding-rectangle-p)
  (:import-from #:clim-test-util #:fails #:compilation-signals)
  (:export #:run-tests))

(cl:in-package #:clim-tests)

(def-suite :mcclim)

(defun run-tests ()
  (run! :mcclim))
