;;;
;;; Copyright (c) 2016 Daniel Kochmański
;;;

(in-package :clim-internals)

(defmacro define-opticl-reader (name opticl-reader)
  `(defun ,name (image-pathname)
     (let* ((img (handler-case
                     (,opticl-reader image-pathname)
                   (error ()
                     (error 'unsupported-bitmap-format)))))
       (convert-opticl-img img))))

(define-opticl-reader opticl-read-bitmap-file opticl:read-image-file)
(define-opticl-reader opticl-read-gif-file opticl:read-gif-file)
(define-opticl-reader opticl-read-jpg-file opticl:read-jpeg-file)
(define-opticl-reader opticl-read-pbm-file opticl:read-pbm-file)
(define-opticl-reader opticl-read-pgm-file opticl:read-pgm-file)
(define-opticl-reader opticl-read-png-file opticl:read-png-file)
(define-opticl-reader opticl-read-pnm-file opticl:read-pnm-file)
(define-opticl-reader opticl-read-ppm-file opticl:read-ppm-file)
(define-opticl-reader opticl-read-tiff-file opticl:read-tiff-file)

(setf (gethash :fallback climi::*bitmap-file-readers*)
      #'opticl-read-bitmap-file)

(defun convert-opticl-img (img)
  "Converts opticl image format to RGBA array."
  (let* ((height (array-dimension img 0))
         (width (array-dimension img 1))
         (array (make-array (list height width)
                            :element-type '(unsigned-byte 32))))
    (etypecase img
      (opticl:gray-image
       (opticl:do-pixels (y x) img
         (let ((v (aref img y x)))
           (setf (aref array y x)
                 (dpb v (byte 8 24)
                      (dpb v (byte 8 16)
                           (dpb v (byte 8 8)
                                #xff)))))))

      (opticl:gray-alpha-image
       (opticl:do-pixels (y x) img
         (let ((v (aref img y x 0)))
           (setf (aref array y x)
                 (dpb v (byte 8 24)
                      (dpb v (byte 8 16)
                           (dpb v (byte 8 8)
                                (aref img y x 1))))))))

      (opticl:rgb-image
       (opticl:do-pixels (y x) img
         (setf (aref array y x)
               (dpb (aref img y x 0) (byte 8 24)
                    (dpb (aref img y x 1) (byte 8 16)
                         (dpb (aref img y x 2) (byte 8 8)
                              #xff))))))

      (opticl:rgba-image
       (opticl:do-pixels (y x) img
         (setf (aref array y x)
               (dpb (aref img y x 0) (byte 8 24)
                    (dpb (aref img y x 1) (byte 8 16)
                         (dpb (aref img y x 2) (byte 8 8)
                              (aref img y x 3))))))))
    array))
