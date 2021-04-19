;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------

(cl:in-package #:mcclim-raster-image.test)

(in-suite :mcclim-raster-image)

(test smoke
  "Smoke test for the raster image backend."

  (loop for i from 1
        for filename = (format nil "raster-image-test-~D.png" i)
        for page in clim-test-util:*all-test-pages*
        do (finishes
             (mcclim-raster-image:with-output-to-raster-image-file
                 (stream filename)
               (funcall page stream)))))
