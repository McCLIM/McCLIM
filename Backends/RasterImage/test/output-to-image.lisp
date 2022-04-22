;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:mcclim-raster-image.test)

(in-suite :mcclim-raster-image)

(test with-output-to-drawing-stream.smoke
  "Smoke test for the WITH-OUTPUT-TO-RASTER-IMAGE macro."

  (flet ((do-it (width height)
           (let* ((name (format nil "with-output-to-drawing-stream-raster-~A-~A"
                                width height))
                  (pathname (make-pathname :name name :type "png"))
                  (string   (with-output-to-string (*standard-output*)
                              (room))))
             (clime:with-output-to-drawing-stream
                 (stream :raster pathname :width width :height height)
               (write-string string stream)))))
    (map-product (lambda (&rest args)
                   (finishes (apply #'do-it args)))
                 '(60 :compute) '(60 :compute))))

(test with-output-to-image.nesting
  (finishes
    (clime:with-output-to-drawing-stream (str :raster :pattern :image nil)
      (let ((image (clime:with-output-to-drawing-stream
                       (str2 :raster :pattern :image nil)
                     (clim:draw-line* str2 0 0 10 10))))
        (clim:draw-design str image)))))
