;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2019 by Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-postscript.test)

(in-suite :clim-postscript)

(clim:define-command-table output-destination-test-command-table)

(clim:define-command (com-test-output-destination
                      :command-table output-destination-test-command-table
                      :provide-output-destination-keyword t)
    ()
  (clim-test-util:print-test-page-1 *standard-output*))

(test output-destination.smoke
  "Smoke test for PostScript command output destination."

  (finishes
    (let ((destination (make-instance 'clim-postscript::postscript-destination
                                      :file "postscript-output-destination.ps")))
      (com-test-output-destination :output-destination destination))))
