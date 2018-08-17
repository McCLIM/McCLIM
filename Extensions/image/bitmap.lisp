(in-package :mcclim-image)

;;; Bitmap images
;;;
;;; Based on CLIM 2.2, with an extension permitting the definition of
;;; new image formats by the user.

(define-bitmap-file-reader :xpm (pathname)
  (xpm-parse-file pathname))

(define-bitmap-file-reader :pixmap (pathname)
  (read-bitmap-file pathname :format :xpm))

(define-bitmap-file-reader :pixmap-3 (pathname)
  (read-bitmap-file pathname :format :xpm))

(define-bitmap-file-reader :gif (pathname)
  (opticl-read-gif-file pathname))

(define-bitmap-file-reader :jpg (pathname)
  (opticl-read-jpg-file pathname))

(define-bitmap-file-reader :jpeg (pathname)
  (opticl-read-jpg-file pathname))

(define-bitmap-file-reader :pbm (pathname)
  (opticl-read-pbm-file pathname))

(define-bitmap-file-reader :pgm (pathname)
  (opticl-read-pgm-file pathname))

(define-bitmap-file-reader :png (pathname)
  (opticl-read-png-file pathname))

(define-bitmap-file-reader :pnm (pathname)
  (opticl-read-pnm-file pathname))

(define-bitmap-file-reader :ppm (pathname)
  (opticl-read-ppm-file pathname))

(define-bitmap-file-reader :tiff (pathname)
  (opticl-read-tiff-file pathname))
