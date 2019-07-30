(cl:in-package #:mcclim-render-internals)

;;; Image

(defun draw-image* (medium image x y
                    &rest args
                    &key clipping-region transformation)
  (declare (ignorable clipping-region transformation args))
  (climi::with-medium-options (medium args)
    (draw-pattern* medium image x y)))

(clim-internals::def-graphic-op draw-image* (image x y))

;;; Image operations

(defun make-image (width height)
  "Create an empty transparent image of size WIDTH x HEIGHT."
  ;; XXX: something in text rendering depends image being transparent by
  ;; default. This should be fixed.
  (make-instance 'clime:image-pattern :array (make-argb-pixel-array width height)))

;;; Unsafe versions of COPY-IMAGE. Caller must ensure that all arguments are
;;; valid and arrays are of proper type.
(macrolet
    ((define-copy-image (name backwardp)
       `(progn
          (declaim (inline ,name))
          (defun ,name (src-array dst-array x1s y1s x1d y1d x2 y2)
            (declare (type image-index x1s y1s x1d y1d x2 y2)
                     (type argb-pixel-array src-array dst-array)
                     (optimize (speed 3) (safety 0)))
            (do-regions ((src-j dest-j y1s y1d y2)
                         (src-i dest-i x1s x1d x2) ,@(when backwardp
                                                       `(:backward t)))
              (setf (aref dst-array dest-j dest-i)
                    (aref src-array src-j src-i)))))))
  (define-copy-image %copy-image nil)
  (define-copy-image %copy-image* t))

;;; XXX: We should unify it with COPY-AREA and MEDIUM-COPY-AREA. That means that
;;; raster images should be mediums on their own rights (aren't they?).
(defun copy-image (src-image sx sy width height dst-image dx dy
                   &aux
                   (sx (round sx))
                   (sy (round sy))
                   (dx (round dx))
                   (dy (round dy))
                   (width (round width))
                   (height (round height))
                   (src-array (climi::pattern-array src-image))
                   (dst-array (climi::pattern-array dst-image)))
  "Copy SRC-IMAGE to DST-IMAGE region-wise. Both may be the same image."
  (unless (%check-coords src-array dst-array sx sy dx dy width height)
    (return-from copy-image nil))
  (let ((max-x (+ dx width -1))
        (max-y (+ dy height -1)))
    (declare (fixnum max-x max-y))
    (cond ((not (eq src-array dst-array))
           #1=(%copy-image src-array dst-array sx sy dx dy max-x max-y))
          ((> sy dy) #1#)
          ((< sy dy) #2=(%copy-image* src-array dst-array sx sy dx dy max-x max-y))
          ((> sx dx) #1#)
          ((< sx dx) #2#)
          (t nil)))
  (make-rectangle* (1- dx) (1- dy) (+ dx width) (+ dy height)))

(macrolet
    ((define-blend-image (name backwardp)
       `(progn
          (declaim (inline ,name))
          (defun ,name (src-array dst-array x1s y1s x1d y1d x2 y2)
            (declare (type image-index x1s y1s x1d y1d x2 y2)
                     (type argb-pixel-array src-array dst-array)
                     (optimize (speed 3) (safety 0)))
            (do-regions ((src-j dest-j y1s y1d y2)
                         (src-i dest-i x1s x1d x2) ,@(when backwardp
                                                       `(:backward t)))
              (let-rgba ((r.fg g.fg b.fg a.fg) (aref src-array src-j src-i))
                (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array dest-j dest-i))
                  (setf (aref dst-array dest-j dest-i)
                        (octet-blend-function* r.fg g.fg b.fg a.fg
                                               r.bg g.bg b.bg a.bg)))))))))
  (define-blend-image %blend-image nil)
  (define-blend-image %blend-image* t))

(defun blend-image (src-image sx sy width height dst-image dx dy
                    &aux
                    (sx (round sx))
                    (sy (round sy))
                    (dx (round dx))
                    (dy (round dy))
                    (width (round width))
                    (height (round height))
                    (src-array (climi::pattern-array src-image))
                    (dst-array (climi::pattern-array dst-image)))
  "Blend SRC-IMAGE into DST-IMAGE region-wise. Both may be the same image."
  (unless (%check-coords src-array dst-array sx sy dx dy width height)
    (return-from blend-image nil))
  (let ((max-x (+ dx width -1))
        (max-y (+ dy height -1)))
    (cond ((eq src-array dst-array)
           #1=(%blend-image src-array dst-array sx sy dx dy max-x max-y))
          ((> sy dy) #1#)
          ((< sy dy) #2=(%blend-image* src-array dst-array sx sy dx dy max-x max-y))
          ((> sx dx) #1#)
          ((< sx dx) #2#)
          (t nil)))
  (make-rectangle* (1- dx) (1- dy) (+ dx width) (+ dy height)))

(defun clone-image (image)
  (let ((src-array (climi::pattern-array image)))
    (declare (type argb-pixel-array src-array))
    (make-instance 'climi::%rgba-pattern :array (alexandria:copy-array src-array))))

(defun fill-image (image design &key (x 0) (y 0)
                                     (width (pattern-width image))
                                     (height (pattern-height image))
                                     stencil (stencil-dx 0) (stencil-dy 0)
                                     clip-region)
  "Blends DESIGN onto IMAGE with STENCIL and a CLIP-REGION."
  (declare (optimize (speed 3))
           (type fixnum x y width height stencil-dx stencil-dy))
  ;; Disregard CLIP-REGION if x,y,width,height is entirely contained.
  (when (and clip-region
             (region-contains-region-p
              clip-region (make-rectangle* x y (+ x width) (+ y height)))) ; TODO we create almost this rectangle as our return value
    (setf clip-region nil))
  (let ((dst-array (climi::pattern-array image))
        (stencil-array (and stencil (climi::pattern-array stencil)))
        (x2 (+ x width -1))
        (y2 (+ y height -1))
        old-alpha old-ink
        alpha ink mode
        source-rgba source-r source-g source-b source-a

        old-mode)
    (declare (type (simple-array (unsigned-byte 32) 2) dst-array))
    (flet ((update-alpha (i j)
             (locally (declare (type (simple-array (unsigned-byte 8) 2) stencil-array))
               (let ((stencil-x (+ stencil-dx i))
                     (stencil-y (+ stencil-dy j)))
                 (setf alpha
                       (cond ((not (array-in-bounds-p stencil-array stencil-y stencil-x))
                              nil)
                             ((let ((value (aref stencil-array stencil-y stencil-x)))
                                (if (= #xff value)
                                    nil
                                    value))))))))
           (update-ink (i j)
             (setf ink (clime:design-ink design i j))
             (when (and (eq old-ink ink) (eql old-alpha alpha))
               (return-from update-ink))

             (cond ((typep ink 'standard-flipping-ink)
                    (let-rgba ((r.fg g.fg b.fg a.fg) (let ((d1 (slot-value ink 'climi::design1))
                                                           (d2 (slot-value ink 'climi::design2)))
                                                       (logior (logxor (climi::%rgba-value d1)
                                                                       (climi::%rgba-value d2))
                                                               #xff)))
                      (setf source-r r.fg
                            source-g g.fg
                            source-b b.fg)
                      (cond (alpha
                             (setf source-a (octet-mult a.fg alpha)
                                   mode     :flipping/blend))
                            (t
                             (setf source-a    #x00
                                   source-rgba (%vals->rgba source-r source-g source-b #xff)
                                   mode        :flipping)))))
                   ((not alpha)
                    (let ((ink-rgba (climi::%rgba-value ink)))
                      (if (= #xff (logand #xff ink-rgba))
                          (setf source-rgba ink-rgba
                                mode        :copy)
                          (let-rgba ((r g b a) ink-rgba)
                            (setf source-r r
                                  source-g g
                                  source-b b
                                  source-a a
                                  mode     :blend)))))
                   (t
                    (let-rgba ((r.fg g.fg b.fg a.fg) (climi::%rgba-value ink))
                      (setf source-r r.fg
                            source-g g.fg
                            source-b b.fg
                            source-a (octet-mult a.fg alpha)
                            mode     (case source-a
                                       (0
                                        nil)
                                       (255
                                        (setf source-rgba (climi::%rgba-value ink))
                                        :copy)
                                       (t
                                        :blend))))))))
      (do-regions ((src-j j y y y2)
                   (src-i i x x x2))
        (when (or (null clip-region)
                  (region-contains-position-p clip-region src-i src-j))
          (when stencil-array
            (update-alpha i j))
          (update-ink i j)

          #+debug (unless (and (eq mode old-mode) (eql old-alpha alpha))
                    (format t "~A -> ~A ~A -> ~A @ ~D,~D ~D,~D~%"
                            old-mode mode old-alpha alpha
                            i j width height)
                    (when (eq mode :blend)
                      (format t "  ~D ~D ~D ~D~%"
                              source-r source-g source-b source-a))
                    (setf old-mode mode))

          (setf old-alpha alpha
                old-ink   ink)

          (case mode
            ((nil))
            (:flipping
             (setf (aref dst-array j i) (logxor source-rgba
                                                (aref dst-array j i))))
            (:flipping/blend
             (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array j i))
               (setf (aref dst-array j i)
                     (octet-blend-function*
                      (color-octet-xor source-r r.bg)
                      (color-octet-xor source-g b.bg)
                      (color-octet-xor source-b g.bg)
                      source-a
                      r.bg g.bg b.bg a.bg))))
            (:copy
             (setf (aref dst-array j i) source-rgba))
            (:blend
             (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array j i))
               (setf (aref dst-array j i)
                     (octet-blend-function*
                      source-r source-g source-b source-a
                      r.bg     g.bg     b.bg     a.bg)))))))))
  ;; XXX These #'1- are fishy. We don't capture correct region (rounding
  ;; issue?). This problem is visible when scrolling.
  (make-rectangle* (1- x) (1- y) (+ x width) (+ y height)))
