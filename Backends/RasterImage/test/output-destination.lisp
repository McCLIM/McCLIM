;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------

(cl:in-package #:mcclim-raster-image.test)

(in-suite :mcclim-raster-image)

(clim:define-command-table output-destination-test-command-table)

(clim:define-command (com-test-output-destination
                      :command-table output-destination-test-command-table
                      :provide-output-destination-keyword t)
    ()
  (clim-test-util:print-test-page-1 *standard-output*))

(test output-destination.smoke
  "Smoke test for raster image command output destination."

  (finishes
    (let ((destination (make-instance 'mcclim-raster-image::raster-image-destination
                                      :file "raster-image-output-destination.png"
                                      :file-format "png")))
      (com-test-output-destination :output-destination destination))))
