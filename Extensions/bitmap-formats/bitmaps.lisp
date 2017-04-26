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
      (let (red green blue)
        (ecase (array-rank img)
          (3
           (setq red (aref img y x 0))
           (setq green (aref img y x 1))
           (setq blue (aref img y x 2)))
          (2
           (let ((v (aref img y x)))
             (setq red v)
             (setq green v)
             (setq blue v))))
        (setf (aref array y x)
              (dpb red (byte 8 0)
                   (dpb green (byte 8 8)
                        (dpb blue (byte 8 16)
                             (dpb (- 255 0) (byte 8 24) 0)))))))
    array))
