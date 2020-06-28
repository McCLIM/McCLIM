(in-package :mcclim-render-internals)

;;;
;;; Image
;;;

(defun draw-image* (medium image x y
                    &rest args
                    &key clipping-region transformation)
  (declare (ignorable clipping-region transformation args))
  (climi::with-medium-options (medium args)
    (draw-pattern* medium image x y)))

(clim-internals::def-graphic-op draw-image* (image x y))

;;;
;;; Image operations
;;;

(defun make-image (width height)
  "Create an empty transparent image of size WIDTH x HEIGHT."
  ;; XXX: something in text rendering depends image being transparent by
  ;; default. This should be fixed.
  (make-instance 'clime:image-pattern :array (make-array (list height width)
                                                         :element-type '(unsigned-byte 32)
                                                         :initial-element #xFFFFFF00)))

;;; Unsafe versions of COPY-IMAGE. Caller must ensure that all arguments are
;;; valid and arrays are of proper type.
(declaim (inline %copy-image %copy-image*))

(defun %copy-image (src-array dst-array x1s y1s x1d y1d x2 y2)
  (declare (type fixnum x1s y1s x1d y1d x2 y2)
           (type (simple-array (unsigned-byte 32) 2) src-array dst-array)
           (optimize (speed 3) (safety 0)))
  (do-regions ((src-j dest-j y1s y1d y2)
               (src-i dest-i x1s x1d x2))
    (setf (aref dst-array dest-j dest-i)
          (aref src-array src-j src-i))))

(defun %copy-image* (src-array dst-array x1s y1s x1d y1d x2 y2)
  (declare (type fixnum x1s y1s x1d y1d x2 y2)
           (type (simple-array (unsigned-byte 32) 2) src-array dst-array)
           (optimize (speed 3) (safety 0)))
  (do-regions ((src-j dest-j y1s y1d y2)
               (src-i dest-i x1s x1d x2) :backward t)
    (setf (aref dst-array dest-j dest-i)
          (aref src-array src-j src-i))))

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
  "Copies SRC-IMAGE to DST-IMAGE region-wise. Both may be the same image."
  (unless (%check-coords src-array dst-array sx sy dx dy width height)
    (return-from copy-image nil))
  (let ((max-x (+ dx width -1))
        (max-y (+ dy height -1)))
    (declare (fixnum max-x max-y))
    (if (eq src-array dst-array)
        (cond ((> sy dy) #1=(%copy-image src-array dst-array sx sy dx dy max-x max-y))
              ((< sy dy) #2=(%copy-image* src-array dst-array sx sy dx dy max-x max-y))
              ((> sx dx) #1#)
              ((< sx dx) #2#)
              (T NIL))
        #1#))
  (make-rectangle* (1- dx) (1- dy) (+ dx width) (+ dy height)))

(declaim (inline %blend-image %blend-image*))

(defun %blend-image (src-array dst-array x1s y1s x1d y1d x2 y2)
  (declare (type fixnum x1s y1s x1d y1d x2 y2)
           (type (simple-array (unsigned-byte 32) 2) src-array dst-array)
           (optimize (speed 3) (safety 0)))
  (do-regions ((src-j dest-j y1s y1d y2)
               (src-i dest-i x1s x1d x2))
    (let-rgba ((r.fg g.fg b.fg a.fg) (aref src-array src-j src-i))
      (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array dest-j dest-i))
        (setf (aref dst-array dest-j dest-i)
              (octet-blend-function* r.fg g.fg b.fg a.fg
                                     r.bg g.bg b.bg a.bg))))))

(defun %blend-image* (src-array dst-array x1s y1s x1d y1d x2 y2)
  (declare (type fixnum x1s y1s x1d y1d x2 y2)
           (type (simple-array (unsigned-byte 32) 2) src-array dst-array)
           (optimize (speed 3) (safety 0)))
  (do-regions ((src-j dest-j y1s y1d y2)
               (src-i dest-i x1s x1d x2) :backward t)
    (let-rgba ((r.fg g.fg b.fg a.fg) (aref src-array src-j src-i))
      (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array dest-j dest-i))
        (setf (aref dst-array dest-j dest-i)
              (octet-blend-function* r.fg g.fg b.fg a.fg
                                     r.bg g.bg b.bg a.bg))))))

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
  "Copies SRC-IMAGE to DST-IMAGE region-wise. Both may be the same image."
  (unless (%check-coords src-array dst-array sx sy dx dy width height)
    (return-from blend-image nil))
  (let ((max-x (+ dx width -1))
        (max-y (+ dy height -1)))
    (if (eq src-array dst-array)
        (cond ((> sy dy) #1=(%blend-image src-array dst-array sx sy dx dy max-x max-y))
              ((< sy dy) #2=(%blend-image* src-array dst-array sx sy dx dy max-x max-y))
              ((> sx dx) #1#)
              ((< sx dx) #2#)
              (T NIL))
        #1#))
  (make-rectangle* (1- dx) (1- dy) (+ dx width) (+ dy height)))

(defun clone-image (image)
  (let ((src-array (climi::pattern-array image)))
    (declare (type (simple-array (unsigned-byte 32) 2) src-array))
    (make-instance 'climi::%rgba-pattern :array (alexandria:copy-array src-array))))

(defun fill-image (image design &key (x 0) (y 0)
                                  (width (pattern-width image))
                                  (height (pattern-height image))
                                  stencil (stencil-dx 0) (stencil-dy 0)
                                  clip-region
                   &aux
                     (dst-array (climi::pattern-array image))
                     (x2 (+ x width -1))
                     (y2 (+ y height -1)))
  "Blends DESIGN onto IMAGE with STENCIL and a CLIP-REGION."
  (let ((stencil-array (and stencil (climi::pattern-array stencil))))
    (do-regions ((src-j j y y y2)
                 (src-i i x x x2))
      (when (or (null clip-region)
                (region-contains-position-p clip-region src-i src-j))
        (let ((alpha (if stencil-array
                         (let ((stencil-x (+ stencil-dx i))
                               (stencil-y (+ stencil-dy j)))
                           (if
                            (array-in-bounds-p stencil-array stencil-y stencil-x)
                            (aref stencil-array stencil-y stencil-x)
                            #xff))
                         #xff))
              (ink (clime:design-ink design src-i src-j)))
          (if (typep ink 'standard-flipping-ink)
              (let-rgba ((r.fg g.fg b.fg a.fg) (let ((d1 (slot-value ink 'climi::design1))
                                                     (d2 (slot-value ink 'climi::design2)))
                                                 (logior (logxor (climi::%rgba-value d1)
                                                                 (climi::%rgba-value d2))
                                                         #xff)))
                (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array j i))
                  (setf (aref dst-array j i)
                        (octet-blend-function* (color-octet-xor r.fg r.bg)
                                               (color-octet-xor g.fg g.bg)
                                               (color-octet-xor b.fg b.bg)
                                               (octet-mult a.fg alpha)
                                               r.bg g.bg b.bg a.bg))))
              (let-rgba ((r.fg g.fg b.fg a.fg) (climi::%rgba-value ink))
                (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array j i))
                  (setf (aref dst-array j i)
                        (octet-blend-function* r.fg g.fg b.fg (octet-mult a.fg alpha)
                                               r.bg g.bg b.bg a.bg)))))))))
  ;; XXX These #'1- are fishy. We don't capture correct region (rounding
  ;; issue?). This problem is visible when scrolling.
  (make-rectangle* (1- x) (1- y) (+ x width) (+ y height)))
