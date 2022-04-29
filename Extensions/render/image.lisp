(in-package #:mcclim-render)

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
  ;; XXX something in text rendering depends image being transparent by
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
                     (optimize (speed 3)
                               #-ccl (safety 0)
                               #+ccl (safety 1)))
            (do-regions ((src-j dest-j y1s y1d y2)
                         (src-i dest-i x1s x1d x2)
                         ,@(when backwardp
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
    (return-from copy-image +nowhere+))
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
  (make-rectangle* dx dy (+ dx width) (+ dy height)))

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
  (make-rectangle* dx dy (+ dx width) (+ dy height)))

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
           (type image-index x y width height)
           (type image-index-displacement stencil-dx stencil-dy))
  ;; Try to do something smart about CLIP-REGION to avoid checking
  ;; containment for each pixel.
  (when clip-region
    (let ((region (make-rectangle* x y (+ x width) (+ y height))))
      (cond ;; Disregard CLIP-REGION if x,y,width,height is entirely contained.
        ((region-contains-region-p clip-region region)
         (setf clip-region nil))
        ((bounding-rectangle-p clip-region)
         (with-bounding-rectangle* (x1 y1 :width w :height h)
             (region-intersection clip-region region)
           (setf x (floor x1) y (floor y1)
                 width (ceiling w) height (ceiling h)
                 clip-region nil))))))
  (let* (;; Stencil
         (stencil-array (and stencil (pattern-array stencil)))
         (stencil-width-max (when stencil-array
                              (1- (array-dimension stencil-array 1))))
         (stencil-height-max (when stencil-array
                               (1- (array-dimension stencil-array 0))))
         ;; Destination
         (dst-array (pattern-array image))
         (x2 (+ x width -1))
         (y2 (+ y height -1))
         ;; Current mode and color
         (old-alpha 255) (alpha 255)
         old-ink ink
         mode
         source-rgba source-r source-g source-b source-a)
    (declare (type (or null stencil-array) stencil-array)
             (type argb-pixel-array dst-array)
             (type octet old-alpha alpha))
    (flet ((update-alpha (i j)
             (locally (declare (type stencil-array stencil-array)
                               (type image-dimension stencil-width-max stencil-height-max))
               (let ((stencil-x (+ stencil-dx i))
                     (stencil-y (+ stencil-dy j)))
                 (setf alpha (if (and (<= 0 stencil-y stencil-height-max)
                                      (<= 0 stencil-x stencil-width-max))
                                 (aref stencil-array stencil-y stencil-x)
                                 0)))))
           (update-ink (i j)
             (setf ink (climi::design-ink* design i j))
             (when (and (eq old-ink ink) (= old-alpha alpha))
               (return-from update-ink))
             (setf old-alpha alpha
                   old-ink ink)
             (cond ((zerop alpha)
                    (setf mode nil))
                   ((typep ink 'standard-flipping-ink)
                    (setf source-rgba (let ((d1 (slot-value ink 'climi::design1))
                                            (d2 (slot-value ink 'climi::design2)))
                                        (logand #x00ffffff
                                                (logxor (climi::%rgba-value d1)
                                                        (climi::%rgba-value d2)))))
                    (if (= alpha 255)
                        (setf mode :flipping)
                        (setf source-a alpha
                              mode :flipping/blend)))
                   ((= alpha 255)
                    (let ((ink-rgba (climi::%rgba-value ink)))
                      (if (= 255 (ldb (byte 8 24) ink-rgba))
                          (setf source-rgba ink-rgba
                                mode :copy)
                          (let-rgba ((r g b a) ink-rgba)
                            (setf source-r r
                                  source-g g
                                  source-b b
                                  source-a a
                                  mode :blend)))))
                   (t ; If we get here, ALPHA is [1, 254].
                    (locally (declare (type (integer 1 254) alpha))
                      (let-rgba ((r.fg g.fg b.fg a.fg) (climi::%rgba-value ink))
                        (setf source-r r.fg
                              source-g g.fg
                              source-b b.fg
                              source-a (octet-mult a.fg alpha)
                              ;; SOURCE-A is [0, 254], so never :COPY.
                              mode (if (zerop source-a)
                                       nil
                                       :blend))))))))
      (do-regions ((src-j j y y y2)
                   (src-i i x x x2))
        (when (or (null clip-region)
                  (region-contains-position-p clip-region src-i src-j))
          (when stencil-array
            (update-alpha i j))
          (update-ink i j)
          (case mode ; do nothing if MODE is NIL
            (:flipping
             (setf (aref dst-array j i) (logxor source-rgba
                                                (aref dst-array j i))))
            (:flipping/blend
             (let ((dest-rgba (aref dst-array j i)))
               (let-rgba ((r.bg g.bg b.bg a.bg) dest-rgba)
                 (let-rgba ((r g b) (logxor source-rgba dest-rgba))
                   (setf (aref dst-array j i)
                         (octet-blend-function* r    g    b    source-a
                                                r.bg g.bg b.bg a.bg))))))
            (:copy
             (setf (aref dst-array j i) source-rgba))
            (:blend
             (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array j i))
               (setf (aref dst-array j i)
                     (octet-blend-function* source-r source-g source-b source-a
                                            r.bg     g.bg     b.bg     a.bg)))))))))
  (make-rectangle* x y (+ x width) (+ y height)))
