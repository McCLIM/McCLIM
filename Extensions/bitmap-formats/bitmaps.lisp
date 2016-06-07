;;;
;;; Copyright (c) 2016 Daniel Kochmański
;;;

(in-package :clim-internals)

(defun opticl-read-bitmap-file (image-pathname)
  (let* ((img (opticl:read-image-file image-pathname))
         (height (array-dimension img 0))
         (width (array-dimension img 1))
         (array (make-array (list height width)
                            :element-type '(unsigned-byte 32))))
    (opticl:do-pixels (y x) img
      (let ((red (aref img y x 0))
            (green (aref img y x 1))
            (blue (aref img y x 2)))
        (setf (aref array y x)
              (dpb red (byte 8 0)
                   (dpb green (byte 8 8)
                        (dpb blue (byte 8 16)
                             (dpb (- 255 0) (byte 8 24) 0)))))))
    array))



(define-bitmap-file-reader :png (image-pathname)
  (opticl-read-bitmap-file image-pathname))

(define-bitmap-file-reader :gif (image-pathname)
  (opticl-read-bitmap-file image-pathname))

(define-bitmap-file-reader :jpg (image-pathname)
  (opticl-read-bitmap-file image-pathname))

(define-bitmap-file-reader :tif (image-pathname)
  (opticl-read-bitmap-file image-pathname))

;;; Abberv
(define-bitmap-file-reader :jpeg (image-pathname)
  (opticl-read-bitmap-file image-pathname))

(define-bitmap-file-reader :tiff (image-pathname)
  (opticl-read-bitmap-file image-pathname))
