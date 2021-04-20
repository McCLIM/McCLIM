;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2019 by Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-pdf.test)

(in-suite :clim-pdf)

(clim:define-command-table output-destination-test-command-table)

(clim:define-command (com-test-output-destination
                      :command-table output-destination-test-command-table
                      :provide-output-destination-keyword t)
    ()
  (clim-test-util:print-test-page-1 *standard-output*))

(test output-destination.smoke
  "Smoke test for PDF command output destination."

  (finishes
    (let ((destination (make-instance 'clim-pdf::pdf-destination
                                      :file "pdf-output-destination.pdf")))
      (com-test-output-destination :output-destination destination))))
