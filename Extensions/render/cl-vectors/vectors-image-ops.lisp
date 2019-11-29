(in-package :mcclim-render-internals)

;;;
;;; aa render functions
;;;

(defun aa-render-draw-fn (image clip-region design)
  (let ((pixels (climi::pattern-array image)))
    (lambda (x y alpha)
      (declare (type fixnum x y alpha))
      (setf alpha (min (abs alpha) 255))
      (unless (or (zerop alpha)
                  (and clip-region
                       (not (region-contains-position-p clip-region x y))))
        (let* ((value (climi::%rgba-value (clime:design-ink design x y)))
               (a.fg (ldb (byte 8 0) value)))
          (if (> (octet-mult a.fg alpha) 250)
              (setf (aref pixels y x) value)
              (let-rgba ((r.fg g.fg b.fg a.fg) value)
                (let-rgba ((r.bg g.bg b.bg a.bg) (aref pixels y x))
                  (setf (aref pixels y x)
                        (multiple-value-call #'%vals->rgba
                          (octet-rgba-blend-function r.fg g.fg b.fg (octet-mult a.fg alpha)
                                                     r.bg g.bg b.bg a.bg)))))))))))

(defun aa-render-xor-draw-fn (image clip-region design)
  (let ((pixels (climi::pattern-array image)))
    (lambda (x y alpha)
      (declare (type fixnum x y alpha))
      (setf alpha (min (abs alpha) 255))
      (unless (or (zerop alpha)
                  (and clip-region
                       (not (region-contains-position-p clip-region x y))))
        (multiple-value-bind (r.fg g.fg b.fg a.fg)
            (%rgba->vals (let* ((ink (clime:design-ink design x y))
                                (d1 (clime:design-ink (slot-value ink 'climi::design1) x y))
                                (d2 (clime:design-ink (slot-value ink 'climi::design2) x y)))
                           (logior (logxor (climi::%rgba-value d1)
                                           (climi::%rgba-value d2))
                                   #xff)))
          (let-rgba ((r.bg g.bg b.bg a.bg) (aref pixels y x))
            (setf (aref pixels y x)
                  (octet-blend-function*
                   (color-octet-xor r.fg r.bg)
                   (color-octet-xor g.fg g.bg)
                   (color-octet-xor b.fg b.bg)
                   (%octet-mult a.fg alpha)
                   r.bg g.bg b.bg a.bg))))))))

(defun aa-render-alpha-draw-fn (image clip-region)
  (let ((pixels (climi::pattern-array image)))
    (lambda (x y alpha)
      (declare (type fixnum x y alpha))
      (setf alpha (min (abs alpha) 255))
      (unless (or (zerop alpha)
                (and clip-region
                     (not (region-contains-position-p clip-region x y))))
        (setf (aref pixels y x) alpha)))))
