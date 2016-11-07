(in-package :mcclim-render)

;;;
;;; Converting string into paths
;;;

(defun loader-font-scale (size loader)
  (float (/ size (zpb-ttf:units/em loader))))

(defun string-primitive-paths (x y string loader size)
  (let ((scale (loader-font-scale size loader)))
    (paths-ttf:paths-from-string loader string :scale-x scale :scale-y (- scale)
				 :offset (paths:make-point x y))))

(defun string-primitive-box (x y string loader size)
  (let* ((scale (loader-font-scale size loader))
	 (box (zpb-ttf:string-bounding-box string loader)))
    (map 'vector #'(lambda (x) (* x scale)) box)))
