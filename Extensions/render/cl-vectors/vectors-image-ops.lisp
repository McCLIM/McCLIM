(cl:in-package #:mcclim-render-internals)

(defun aa-render-draw-fn (image clip-region design)
  (let ((pixels (climi::pattern-array image)))
    (declare (type argb-pixel-array pixels))
    (if (static-ink-p design)
        (let ((value (climi::%rgba-value (clime:design-ink design 0 0))))
          (let-rgba ((r.fg g.fg b.fg a.fg) value)
            (lambda (x y alpha)
              (declare (type fixnum x y)
                       (type fixnum #+bad-range (integer -512 512) #+why-not octet alpha)
                       (optimize speed))
              (setf alpha (min (abs alpha) 255)) ; TODO why is this not [0,255]?
              (unless (or (zerop alpha)
                          (and clip-region
                               (not (region-contains-position-p clip-region x y))))
                (let ((alpha* (octet-mult a.fg alpha)))
                  (setf (aref pixels y x)
                        (if (> alpha* 250) ; TODO why not 255?
                            value
                            (let-rgba ((r.bg g.bg b.bg a.bg) (aref pixels y x))
                              (octet-blend-function* r.fg g.fg b.fg alpha*
                                                     r.bg g.bg b.bg a.bg)))))))))
        (lambda (x y alpha)
          (declare (type fixnum x y)
                   (type (integer -512 512) #+why-not octet alpha)
                   (optimize speed)
                   )
          (setf alpha (min (abs alpha) 255)) ; TODO why is this not [0,255]?
          (unless (or (zerop alpha)
                      (and clip-region
                           (not (region-contains-position-p clip-region x y))))
            (let* ((value (climi::%rgba-value (clime:design-ink design x y)))
                   (a.fg (ldb (byte 8 0) value))
                   (alpha* (octet-mult a.fg alpha)))
              (setf (aref pixels y x)
                    (if (> alpha* 250)  ; TODO why not 255?
                        value
                        (let-rgba ((r.fg g.fg b.fg) value)
                          (let-rgba ((r.bg g.bg b.bg a.bg) (aref pixels y x))
                            (octet-blend-function* r.fg g.fg b.fg alpha*
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
                   (octet-mult a.fg alpha)
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
