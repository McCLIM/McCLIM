;;;
;;; Copyright (c) 2016 Daniel Kochmański
;;;

(in-package :clim-internals)

(define-condition unsupported-bitmap-format (simple-error) ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Unsupported bitmap format")))
  (:documentation "This condition is signaled when trying to read a
  bitmap file whose format is not supported." ))

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

(defun convert-opticl-img (img)
  (let* ((height (array-dimension img 0))
         (width (array-dimension img 1))
         (array (make-array (list height width)
                            :element-type '(unsigned-byte 32))))
    (etypecase img
      (opticl:gray-image
       (opticl:do-pixels (y x) img
         (let ((v (aref img y x)))
           (setf (aref array y x)
                 (dpb v (byte 8 0)
                      (dpb v (byte 8 8)
                           (dpb v (byte 8 16)
                                (dpb (- 255 0) (byte 8 24) 0))))))))

      ;; FIXME! Do the right thing with the alpha channel!
      (opticl:gray-alpha-image
       (opticl:do-pixels (y x) img
         (let ((v (aref img y x 0)))
           (setf (aref array y x)
                 (dpb v (byte 8 0)
                      (dpb v (byte 8 8)
                           (dpb v (byte 8 16)
                                (dpb (- 255 0) (byte 8 24) 0))))))))

      (opticl:rgb-image
       (opticl:do-pixels (y x) img
         (setf (aref array y x)
               (dpb (aref img y x 0) (byte 8 0)
                    (dpb (aref img y x 1) (byte 8 8)
                         (dpb (aref img y x 2) (byte 8 16)
                              (dpb (- 255 0) (byte 8 24) 0)))))))

      ;; FIXME! Do the right thing with the alpha channel!
      (opticl:rgba-image
       (opticl:do-pixels (y x) img
         (setf (aref array y x)
               (dpb (aref img y x 0) (byte 8 0)
                    (dpb (aref img y x 1) (byte 8 8)
                         (dpb (aref img y x 2) (byte 8 16)
                              (dpb (- 255 0) (byte 8 24) 0))))))))
    array))
