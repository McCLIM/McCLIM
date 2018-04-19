(in-package :mcclim-image)

;;; Bitmap images
;;;
;;; Based on CLIM 2.2, with an extension permitting the definition of
;;; new image formats by the user.

(defvar *bitmap-file-readers* (make-hash-table :test 'equalp)
  "A hash table mapping keyword symbols naming bitmap image
formats to a function that can read an image of that format. The
functions will be called with one argument, the pathname of the
file to be read. The functions should return two values as per
`read-bitmap-file'.")

(defmacro define-bitmap-file-reader (bitmap-format (&rest args) &body body)
  "Define a method for reading bitmap images of format
BITMAP-FORMAT that will be used by `read-bitmap-file' and
MAKE-PATTERN-FROM-BITMAP-FILE. BODY should return two values as
per `read-bitmap-file'."
  `(setf (gethash ,bitmap-format *bitmap-file-readers*)
         #'(lambda (,@args)
             ,@body)))

(defun bitmap-format-supported-p (format)
  "Return true if FORMAT is supported by `read-bitmap-file'."
  (not (null (gethash format *bitmap-file-readers*))))

(defun read-bitmap-file (pathname &key (format :bitmap) (port (find-port)))
  "Read a bitmap file named by `pathname'. `Port' specifies the
port that the bitmap is to be used on. `Format' is a keyword
symbol naming any defined bitmap file format defined by
`clim-extensions:define-bitmap-file-reader'. Two values are
returned: a two-dimensional array of pixel values and an array of
either colors or color names. If the second value is non-NIL, the
pixel values are assumed to be indexes into this
array. Otherwise, the pixel values are taken to be RGB values
encoded in 32 bit unsigned integers, with the three most
significant octets being the values R, G and B, in order."
  (declare (ignore port)) ; XXX?
  (funcall (or (gethash format *bitmap-file-readers*)
               #'opticl-read-bitmap-file)
           pathname))

(defun make-pattern-from-bitmap-file (pathname &key designs
                                      (format :bitmap) (port (find-port)))
  "Read a bitmap file named by `pathname'. `Port' specifies the
port that the bitmap is to be used on. `Format' is a keyword
symbol naming any defined bitmap file format defined by
`clim-extensions:define-bitmap-file-reader'. Two values are
returned: a two-dimensional array of pixel values and an array of
either colors or color names. If the second value is non-NIL, the
pixel values are assumed to be indexes into this
array. Otherwise, the pixel values are taken to be RGB values
encoded in 32 bit unsigned integers, with the three most
significant octets being the values R, G and B, in order."
  (multiple-value-bind (res read-designs)
      (read-bitmap-file pathname :format format :port port)
    (if read-designs
        (make-pattern res (or designs read-designs))
        (make-instance 'rgb-pattern :image (make-instance 'rgb-image
                                            :width (array-dimension res 1)
                                            :height (array-dimension res 0)
                                            :data res)))))

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
