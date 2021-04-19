;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------

(cl:in-package #:mcclim-raster-image.test)

(in-suite :mcclim-raster-image)

(test with-output-to-raster-image.smoke
  "Smoke test for the WITH-OUTPUT-TO-RASTER-IMAGE macro."

  (flet ((do-it (width height border-width)
           (let* ((name (format nil "with-output-to-raster-image-~A-~A-~A"
                                width height border-width))
                  (pathname (make-pathname :name name :type "png"))
                  (string   (with-output-to-string (*standard-output*)
                              (room))))
             (mcclim-raster-image:with-output-to-raster-image-file
                 (stream pathname :width width
                                  :height height
                                  :border-width border-width)
               (write-string string stream)))))
    (map-product (lambda (&rest args)
                   (finishes (apply #'do-it args)))
                 '(60 :compute) '(60 :compute) '(0 20))))

(test with-output-to-image.nesting
  (finishes
    (mcclim-raster-image::with-output-to-image (str nil)
      (let ((image (mcclim-raster-image::with-output-to-image (str2 nil)
                     (clim:draw-line* str2 0 0 10 10))))
        (clim:draw-design str image)))))
