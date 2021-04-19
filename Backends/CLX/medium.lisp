;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2001 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2000 Iban Hatchondo <hatchond@emi.u-bordeaux.fr>
;;;  (c) copyright 2000 Julien Boninfante <boninfan@emi.u-bordeaux.fr>
;;;  (c) copyright 2000,2014 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 1998-1999 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-clx)

(defconstant +x11-pixmap-dimension-limit+ 2048)

(defun X-pixel (port color)
  (alexandria:ensure-gethash
   color (slot-value port 'color-table)
   (multiple-value-bind (r g b) (color-rgb color)
     (xlib:alloc-color (xlib:screen-default-colormap
                        (clx-port-screen port))
                       (xlib:make-color :red r :green g :blue b)))))

;;; Needed changes:

;; The gc slot in clx-medium must be either thread local, or [preferred] we
;; should have a unified drawing options -> gcontext cache.
;; --GB

;;; CLX-MEDIUM class

(defclass clx-medium (multiline-text-medium-mixin
                      font-rendering-medium-mixin
                      basic-medium)
  ((gc :initform nil)
   (last-medium-device-region :initform nil
                              :accessor last-medium-device-region)
   ;; CLIPPING-REGION-TMP is reused to avoid consing in the most common case
   ;; when configuring the clipping region.
   (clipping-region-tmp :initform (vector 0 0 0 0))))

;; Variable is used to deallocate lingering resources after the operation.
(defvar ^cleanup)


;;; secondary methods for changing text styles and line styles

(defmethod (setf medium-text-style) :before (text-style (medium clx-medium))
  (with-slots (gc) medium
    (when gc
      (let ((old-text-style (medium-text-style medium)))
        (unless (eq text-style old-text-style)
          (let ((fn (text-style-mapping (port medium) (medium-text-style medium))))
            ;;  This hack is really ugly. There really should be a better way to
            ;;  handle this.
            (when (typep fn 'xlib:font)
              (setf (xlib:gcontext-font gc) fn))))))))

;;; Translate from CLIM styles to CLX styles.
(defun translate-cap-shape (clim-shape)
  (case clim-shape
    (:butt         :butt)
    (:square       :projecting)
    (:round        :round)
    (:no-end-point :not-last)
    (otherwise
     (prog1 :round
       (warn "Unknown cap style ~S, using :round." clim-shape)))))

(defun translate-join-shape (clim-shape)
  (case clim-shape
    (:miter :miter)
    (:bevel :bevel)
    (:round :round)
    (:none
     (prog1 :miter
       (warn "Unsupported join style :NONE, using :MITER.")))
    (otherwise
     (prog1 :miter
       (warn "Unknown join style ~s, using :MITER." clim-shape)))))

(defmethod line-style-effective-dashes (line-style (medium clx-medium))
  (when-let ((dashes (call-next-method)))
    ;; X limits individual dash lengths to the range [0,255].
    (flet ((clamp-to-255 (length)
             (min length 255)))
      (declare (dynamic-extent #'clamp-to-255))
      (if (realp dashes)
          (clamp-to-255 dashes)
          (map 'list #'clamp-to-255 dashes)))))

(defun update-dash-pattern (gc line-style medium)
  (if-let ((dash-pattern (line-style-effective-dashes line-style medium)))
    (setf (xlib:gcontext-line-style gc) :dash
          (xlib:gcontext-dashes gc) (if (atom dash-pattern)
                                        (round dash-pattern)
                                        (mapcar #'round dash-pattern)))
    (setf (xlib:gcontext-line-style gc) :solid)))

(defmethod (setf medium-line-style) :before (new-value (medium clx-medium))
  (when-let ((gc (slot-value medium 'gc)))
    (let* ((old-line-style (medium-line-style medium))
           (old-unit (line-style-unit old-line-style))
           (new-unit (line-style-unit new-value))
           (new-cap-shape (line-style-cap-shape new-value))
           (new-joint-shape (line-style-joint-shape new-value)))
      (unless (and (eq new-unit old-unit)
                   (eql (line-style-thickness new-value)
                        (line-style-thickness old-line-style)))
        (setf (xlib:gcontext-line-width gc)
              (round (line-style-effective-thickness new-value medium))))
      (unless (eq new-cap-shape (line-style-cap-shape old-line-style))
        (setf (xlib:gcontext-cap-style gc)
              (translate-cap-shape new-cap-shape)))
      (unless (eq new-joint-shape (line-style-joint-shape old-line-style))
        (setf (xlib:gcontext-join-style gc)
              (translate-join-shape new-joint-shape)))
      ;; we could do better here by comparing elements of the vector
      ;; -RS 2001-08-24
      (unless (and new-unit old-unit
                   (eq (line-style-dashes new-value)
                       (line-style-dashes old-line-style)))
        (update-dash-pattern gc new-value medium)))))

(defmethod (setf medium-transformation) :around (new-value (medium clx-medium))
  (let ((old-value (medium-transformation medium))
        (new-value (call-next-method)))
    (when-let ((gc (slot-value medium 'gc)))
      (unless (transformation-equal old-value new-value)
        (let ((line-style (medium-line-style medium)))
          (when (eq :coordinate (line-style-unit line-style))
            ;; The following code uses the medium transformation of MEDIUM and
            ;; must there be called after the CALL-NEXT-METHOD call.
            (setf (xlib:gcontext-line-width gc)
                  (round (line-style-effective-thickness line-style medium)))
            (update-dash-pattern gc line-style medium)))))
    new-value))

(defun %clip-region-pixmap (medium mask mask-gc clipping-region x1 y1 width height)
  (typecase clipping-region
    (climi::nowhere-region)             ; do nothing
    (clim:standard-rectangle
     (multiple-value-bind (x1 y1 width height)
         (region->clipping-values clipping-region)
       (xlib:draw-rectangle mask mask-gc x1 y1 width height t)))
    (clim:standard-polygon
     (let ((coord-seq (climi::expand-point-seq (polygon-points clipping-region))))
       (setq coord-seq (map 'vector #'round-coordinate coord-seq))
       (xlib:draw-lines mask mask-gc
                        (concatenate 'vector
                                     coord-seq
                                     (vector (elt coord-seq 0)
                                             (elt coord-seq 1)))
                        :fill-p t)))
    (clim:standard-ellipse
     (flet ((%draw-lines (scan-line)
              (map-over-region-set-regions
               (lambda (reg)
                 (when (linep reg)
                   (multiple-value-bind (lx1 ly1) (line-start-point* reg)
                     (multiple-value-bind (lx2 ly2) (line-end-point* reg)
                       (xlib:draw-line mask mask-gc
                                       (round-coordinate lx1)
                                       (round-coordinate ly1)
                                       (round-coordinate lx2)
                                       (round-coordinate ly2))))))
               scan-line)))
       (if (<= width height)
           (loop for x from x1 to (+ x1 width) do
                (%draw-lines (region-intersection
                              clipping-region
                              (make-line* x y1 x (+ y1 height)))))
           (loop for y from y1 to (+ y1 height) do
                (%draw-lines (region-intersection
                              clipping-region
                              (make-line* x1 y (+ x1 width) y)))))))
    (clim:standard-region-difference
     (let ((region-a (climi::standard-region-difference-a clipping-region))
           (region-b (climi::standard-region-difference-b clipping-region)))
       (multiple-value-bind (x1* y1* width* height*)
           (region->clipping-values region-a)
         (%clip-region-pixmap medium mask mask-gc region-a x1* y1* width* height*))
       (rotatef (xlib:gcontext-foreground mask-gc)
                (xlib:gcontext-background mask-gc))
       (multiple-value-bind (x1* y1* width* height*)
           (region->clipping-values region-b)
         (%clip-region-pixmap medium mask mask-gc region-b x1* y1* width* height*))
       (rotatef (xlib:gcontext-foreground mask-gc)
                (xlib:gcontext-background mask-gc))))
    (clim:standard-region-union
     (map-over-region-set-regions
      (lambda (region)
        (multiple-value-bind (x1* y1* width* height*)
            (region->clipping-values region)
          (%clip-region-pixmap medium mask mask-gc region x1* y1* width* height*)))
      clipping-region))
    (otherwise
     (warn "clx backend: set clipping region: unoptimized path for ~s." (type-of clipping-region))
     (loop for x from x1 to (+ x1 width) do
          (loop for y from y1 to (+ y1 height) do
               (when (region-contains-position-p clipping-region x y)
                 (xlib:draw-point mask mask-gc x y)))))))

(defun %set-gc-clipping-region (medium gc)
  (declare (type clx-medium medium))
  (let ((clipping-region (medium-device-region medium))
        (tmp (slot-value medium 'clipping-region-tmp)))
    (typecase clipping-region
      (climi::nowhere-region
       (setf (xlib:gcontext-clip-mask gc) #()))
      (climi::everywhere-region
       (setf (xlib:gcontext-clip-mask gc) :none))
      (clim:standard-rectangle
       (multiple-value-bind (x1 y1 width height)
           (region->clipping-values clipping-region)
         (setf (aref tmp 0) x1
               (aref tmp 1) y1
               (aref tmp 2) width
               (aref tmp 3) height
               (xlib:gcontext-clip-mask gc :yx-banded) tmp)))
      (climi::standard-rectangle-set
       (when-let ((rect-seq (clipping-region->rect-seq clipping-region)))
         ;; What McCLIM is generating is not :yx-banded in the same
         ;; sense as CLX requires it. Use :unsorted until we fix it.
         #+ (or) (setf (xlib:gcontext-clip-mask gc :yx-banded) rect-seq)
         #- (or) (setf (xlib:gcontext-clip-mask gc :unsorted) rect-seq)))
      (otherwise
       (multiple-value-bind (x1 y1 width height)
           (region->clipping-values clipping-region)
         (let* ((drawable (clx-drawable medium))
                (mask (xlib:create-pixmap :drawable drawable
                                          :depth 1
                                          :width (+ x1 width)
                                          :height (+ y1 height)))
                (mask-gc (xlib:create-gcontext :drawable mask :foreground 1)))
           (setf (xlib:gcontext-foreground mask-gc) 0)
           (xlib:draw-rectangle mask mask-gc 0 0 (+ x1 width) (+ y1 height) t)
           (setf (xlib:gcontext-foreground mask-gc) 1)
           (%clip-region-pixmap medium mask mask-gc clipping-region x1 y1 width height)
           (xlib:free-gcontext mask-gc)
           (push #'(lambda ()
                     (xlib:free-pixmap mask)
                     (setf (last-medium-device-region medium) nil))
                 ^cleanup)
           (setf (xlib:gcontext-clip-mask gc :yx-banded) mask)))))))


(defgeneric medium-gcontext (medium ink)
  (:documentation "MEDIUM-GCONTEXT is responsible for creating graphics context
for foreground drawing. It sets properties like a line-style, sets ink etc. Inks
which are not uniform should be delegated to DESIGN-GCONTEXT which is
responsible for setting graphical context mask."))

(defgeneric design-gcontext (medium ink)
  (:documentation "DESIGN-GCONTEXT is called from MEDIUM-GCONTEXT as means to
set up appropriate mask in order to draw with non-uniform ink. It may be a
pattern, rectangular tile etc. If someone plans to add new kinds of not uniform
inks this is the method to specialize. Note, that MEDIUM-GCONTEXT must be
specialized on class too. Keep in mind, that inks may be transformed (i.e
translated, so they begin at different position than [0,0])."))

(defmethod medium-gcontext :before ((medium clx-medium) ink)
  (declare (ignore ink))
  (let ((mirror (clx-drawable medium)))
    (with-slots (gc) medium
      (unless gc
        (setf gc (xlib:create-gcontext :drawable mirror)
              (xlib:gcontext-fill-style gc) :solid)))))

(defmethod medium-gcontext ((medium clx-medium) (ink color))
  (declare (optimize (debug 3)))
  (let* ((port (port medium)))
    (with-slots (gc last-medium-device-region) medium
      (setf (xlib:gcontext-function gc) boole-1)
      (setf (xlib:gcontext-foreground gc) (X-pixel port ink)
            (xlib:gcontext-background gc) (X-pixel port (medium-background medium)))
      (let ((fn (text-style-mapping port (medium-text-style medium))))
        (when (typep fn 'xlib:font)
          (setf (xlib:gcontext-font gc) fn)))
      (let ((device-region (medium-device-region medium)))
        (unless (eq last-medium-device-region device-region)
          (setf last-medium-device-region device-region)
          (%set-gc-clipping-region medium gc)))
      gc)))

(defmethod medium-gcontext ((medium clx-medium) (ink climi::uniform-compositum))
  (let ((opacity (climi::compositum-mask ink))
        (ink (climi::compositum-ink ink)))
    (if (< (opacity-value opacity) 0.5)
        (slot-value medium 'gc)
        (medium-gcontext medium ink))))

(defmethod medium-gcontext ((medium clx-medium) (ink climi::over-compositum))
  (medium-gcontext medium (climi::compositum-foreground ink)))

(defmethod medium-gcontext ((medium clx-medium) (ink climi::opacity))
  (if (< (opacity-value ink) 0.5)
      (slot-value medium 'gc)
      (medium-gcontext medium +background-ink+)))

(defmethod medium-gcontext ((medium clx-medium) (ink clime:indirect-ink))
  ;; If foreground/background doesn't resolve properly it is a bug in core
  ;; system. We could have masked it with the following code. --jd 2018-09-27
  #+ (or)
  (alexandria:switch (ink)
    (+foreground-ink+ (medium-gcontext medium (medium-foreground medium)))
    (+background-ink+ (medium-gcontext medium (medium-background medium)))
    (otherwise (medium-gcontext medium (clime:indirect-ink-ink ink))))
  (medium-gcontext medium (clime:indirect-ink-ink ink)))

(defmethod medium-gcontext ((medium clx-medium) (ink (eql +flipping-ink+)))
  (let* ((gc (medium-gcontext medium (medium-background medium)))
         (port (port medium))
         (flipper (logxor (X-pixel port (medium-foreground medium))
                          (X-pixel port (medium-background medium)))))
    ;; Now, (logxor flipper foreground) => background
    ;; (logxor flipper background) => foreground
    (setf (xlib:gcontext-function gc) boole-xor)
    (setf (xlib:gcontext-foreground gc) flipper)
    (setf (xlib:gcontext-background gc) flipper)
    gc))

;;; From Tagore Smith <tagore@tagoresmith.com>

(defmethod medium-gcontext ((medium clx-medium)
                            (ink climi::standard-flipping-ink))
  (let* ((gc (medium-gcontext medium (medium-background medium)))
         (port (port medium))
         (color1 (slot-value ink 'climi::design1))
         (color2 (slot-value ink 'climi::design2))
         (flipper (logxor (X-pixel port color1)
                          (X-pixel port color2))))
    (setf (xlib:gcontext-function gc) boole-xor)
    (setf (xlib:gcontext-foreground gc) flipper)
    (setf (xlib:gcontext-background gc) flipper)
    gc))

(defmethod medium-gcontext ((medium clx-medium) (ink clime:pattern))
  (multiple-value-bind (mx my)
      (transform-position (medium-native-transformation medium) 0 0)
    (let ((gc-x (round-coordinate mx))
          (gc-y (round-coordinate my))
          (gc (design-gcontext medium ink)))
      (incf (xlib:gcontext-ts-x gc) gc-x)
      (incf (xlib:gcontext-ts-y gc) gc-y)
      (incf (xlib:gcontext-clip-x gc) gc-x)
      (incf (xlib:gcontext-clip-y gc) gc-y)
      gc)))

(defmethod medium-gcontext ((medium clx-medium) (ink clime:transformed-design)
                            &aux (ink (clime:effective-transformed-design ink)))
  (with-bounding-rectangle* (x1 y1) ink
    (with-transformed-position ((medium-native-transformation medium) x1 y1)
      (let ((gc-x (round-coordinate x1))
            (gc-y (round-coordinate y1))
            (gc (design-gcontext medium ink)))
        (incf (xlib:gcontext-ts-x gc) gc-x)
        (incf (xlib:gcontext-ts-y gc) gc-y)
        (incf (xlib:gcontext-clip-x gc) gc-x)
        (incf (xlib:gcontext-clip-y gc) gc-y)
        gc))))



(defun put-image-recursively (pixmap pixmap-context pixmap-image width height x0 y0)
  (labels ((put-partial-image (width height x0 y0)
             (cond
               ((and (< width +x11-pixmap-dimension-limit+)
                     (< height +x11-pixmap-dimension-limit+))
                (xlib:put-image pixmap pixmap-context pixmap-image
                                :src-x x0 :src-y y0 :x x0 :y y0
                                :width width :height height))
               ((> width height)
                (put-partial-image (ceiling width 2) height x0 y0)
                (put-partial-image (floor width 2) height (+ x0 (ceiling width 2)) y0))
               (T
                (put-partial-image width (ceiling height 2) x0 y0)
                (put-partial-image width (floor height 2) x0 (+ y0 (ceiling height 2)))))))
    (put-partial-image width height x0 y0)))

(defun compute-rgb-mask (drawable image region)
  (let* ((width (pattern-width image))
         (height (pattern-height image))
         (idata (climi::pattern-array image))
         (mm (xlib:create-pixmap :drawable drawable
                                 :width width
                                 :height height
                                 :depth 1))
         (mm-gc (xlib:create-gcontext :drawable mm
                                      :foreground 1
                                      :background 0))
         (mdata (make-array (list height width) :element-type 'bit))
         (mm-image (xlib:create-image :width  width
                                      :height height
                                      :depth  1
                                      :data   mdata)))
    ;; this will be IMAGE-INDEX if we move that into the core
    (declare (type (integer 0 #.(ash 1 30)) width height)
             (type (simple-array (unsigned-byte 32) 2) idata))
    (loop for x of-type alexandria:array-index below width
          do (loop for y of-type alexandria:array-index below height
                   do (setf (aref mdata y x)
                            (if (and (>= (ldb (byte 8 24) (aref idata y x)) #x80)
                                     (region-contains-position-p region x y))
                                1 0))))
    (put-image-recursively mm mm-gc mm-image width height 0 0)
    (xlib:free-gcontext mm-gc)
    (push (lambda () (xlib:free-pixmap mm)) ^cleanup)
    mm))


;;; The purpose of this is to reduce local network traffic for the case of many
;;; calls to compute-rgb-image, for example when drawing a pattern.
;;; For more details, see also: https://github.com/sharplispers/clx/pull/146
(defun cached-drawable-depth (drawable)
  (or (getf (xlib:drawable-plist drawable) :clim-cache)
      (setf (getf (xlib:drawable-plist drawable) :clim-cache)
            (xlib:drawable-depth drawable))))

(defun compute-rgb-image (drawable image)
  (let* ((width (pattern-width image))
         (height (pattern-height image))
         (depth (cached-drawable-depth drawable))
         (idata (clime:pattern-array image))
         (pm (xlib:create-pixmap :drawable drawable
                                 :width width
                                 :height height
                                 :depth depth))
         (pm-gc (xlib:create-gcontext :drawable pm))
         (pm-image (xlib:create-image :width  width
                                      :height height
                                      :depth  depth
                                      :bits-per-pixel 32
                                      :data   idata)))
    (put-image-recursively pm pm-gc pm-image width height 0 0)
    (xlib:free-gcontext pm-gc)
    (push (lambda () (xlib:free-pixmap pm)) ^cleanup)
    pm))

(defmethod design-gcontext ((medium clx-medium) (ink clime:pattern)
                            &aux (ink* (climi::transformed-design-design
                                        (clime:effective-transformed-design ink))))
  (let* ((drawable (clx-drawable medium))
         (rgba-pattern (climi::%collapse-pattern ink))
         (pm (compute-rgb-image drawable rgba-pattern)))
    (let ((gc (xlib:create-gcontext :drawable drawable)))
      (setf (xlib:gcontext-fill-style gc) :tiled
            (xlib:gcontext-tile gc) pm
            (xlib:gcontext-clip-x gc) 0
            (xlib:gcontext-clip-y gc) 0
            (xlib:gcontext-ts-x gc) 0
            (xlib:gcontext-ts-y gc) 0)
      (if-let ((mask (and (not (typep ink* 'clime:rectangular-tile))
                          (let* ((tr (clime:transformed-design-transformation ink))
                                 (region (medium-device-region medium))
                                 (region (untransform-region tr region)))
                            (compute-rgb-mask drawable rgba-pattern region)))))
        (setf (xlib:gcontext-clip-mask gc) mask)
        (%set-gc-clipping-region medium gc))
      (push #'(lambda () (xlib:free-gcontext gc)) ^cleanup)
      gc)))

;;;;

(defun region->clipping-values (region)
  (with-bounding-rectangle* (min-x min-y max-x max-y) region
    ;; We don't use here round-coordinate because clipping rectangle
    ;; must cover the whole region. It is especially important when we
    ;; draw arcs (ellipses without filling) which are not drawn if any
    ;; part is outside the clipped area. -- jd 2019-06-17
    (let ((clip-x (floor min-x))
          (clip-y (floor min-y)))
      (values clip-x
              clip-y
              (- (ceiling max-x) clip-x)
              (- (ceiling max-y) clip-y)))))

;;; This seems to work, but find out why all of these +nowhere+s are
;;; coming from and kill them at the source...
#-(or)
(defun clipping-region->rect-seq (clipping-region)
  (typecase clipping-region
    (area (multiple-value-list (region->clipping-values clipping-region)))
    (t (loop
          for region in (nreverse (mapcan
                                   (lambda (v) (unless (eq v +nowhere+) (list v)))
                                   (region-set-regions clipping-region
                                                       :normalize :y-banding)))
          nconcing (multiple-value-list (region->clipping-values region))))))

(defmacro with-clx-graphics ((&optional (mirror 'mirror)
                                        (line-style 'line-style)
                                        (ink 'ink)
                                        (gcontext 'gc))
                                medium &body body)
  (let ((medium-var (gensym)))
    `(let* ((,medium-var ,medium)
            (,mirror (clx-drawable ,medium-var))
            (^cleanup nil))
       (when ,mirror
         (unwind-protect (let* ((,line-style (medium-line-style ,medium-var))
                                (,ink (medium-ink ,medium-var))
                                (,gcontext (medium-gcontext ,medium-var ,ink)))
                           (declare (ignorable ,line-style ,gcontext))
                           (unless (eql ,ink +transparent-ink+)
                             ,@body))
           (mapc #'funcall ^cleanup))))))



;;; Medium-specific Drawing Functions

(defmethod medium-draw-point* ((medium clx-medium) x y)
  (with-transformed-position ((medium-native-transformation medium) x y)
    (with-clx-graphics () medium
      (let ((diameter (line-style-effective-thickness line-style medium)))
        (if (< diameter 2)
            (let ((x (round-coordinate x))
                  (y (round-coordinate y)))
              (when (and (typep x '(signed-byte 16))
                         (typep y '(signed-byte 16)))
                (xlib:draw-point mirror gc x y)))
            (let* ((radius   (round diameter 2))
                   (diameter (round diameter))
                   (min-x    (round-coordinate (- x radius)))
                   (min-y    (round-coordinate (- y radius))))
              (when (and (typep min-x '(signed-byte 16))
                         (typep min-y '(signed-byte 16)))
                (xlib:draw-arc mirror gc min-x min-y
                               diameter diameter
                               0 (* 2 pi) t))))))))


(defmethod medium-draw-points* ((medium clx-medium) coord-seq)
  (with-transformed-positions
      ((medium-native-transformation medium) coord-seq)
    (with-clx-graphics () medium
      (let ((diameter (line-style-effective-thickness line-style medium)))
        (if (< diameter 2)
            (do-sequence ((x y) coord-seq)
              (let ((x (round-coordinate x))
                    (y (round-coordinate y)))
                (when (and (typep x '(signed-byte 16))
                           (typep y '(signed-byte 16)))
                  (xlib:draw-point mirror gc x y))))
            (let ((radius   (round diameter 2))
                  (diameter (round diameter)))
              (do-sequence ((x y) coord-seq)
                (let ((min-x (round-coordinate (- x radius)))
                      (min-y (round-coordinate (- y radius))))
                  (when (and (typep min-x '(signed-byte 16))
                             (typep min-y '(signed-byte 16)))
                    (xlib:draw-arc mirror gc min-x min-y
                                   diameter diameter
                                   0 (* 2 pi) t))))))))))

(defmethod medium-draw-line* ((medium clx-medium) x1 y1 x2 y2)
  (multiple-value-bind (x1 y1 x2 y2)
      (clipped-line (medium-native-transformation medium) x1 y1 x2 y2)
    (when x1
      (with-clx-graphics () medium
        (xlib:draw-line mirror gc x1 y1 x2 y2)))))

(defmethod medium-draw-polygon* ((medium clx-medium) coord-seq closed filled)
  (assert (evenp (length coord-seq)))
  (let ((tr (medium-native-transformation medium)))
    (multiple-value-bind (coords unionp)
        (clipped-poly tr coord-seq closed)
      (when coords
        (with-clx-graphics () medium
          (flet ((draw-it (coords)
                   (xlib:draw-lines mirror gc coords :fill-p filled)))
            (if unionp
                (mapcar #'draw-it coords)
                (draw-it coords))))))))

(defmethod medium-draw-rectangle* :around
    ((medium clx-medium) left top right bottom filled
     &aux (ink (medium-ink medium)))
  (declare (ignore left top right bottom filled))
  (if (clime:indirect-ink-p ink)
      (with-drawing-options (medium :ink (clime:indirect-ink-ink ink))
        (call-next-method))
      (call-next-method)))

(defmethod medium-draw-rectangle* ((medium clx-medium) left top right bottom filled)
  (let ((tr (medium-native-transformation medium)))
    (if (rectilinear-transformation-p tr)
        (multiple-value-bind (left top width height)
            (clipped-rect tr left top right bottom)
          (when (and width height)
            (with-clx-graphics () medium
              (xlib:draw-rectangle mirror gc left top width height filled))))
        (let* ((vector (vector left top right top right bottom left bottom
                               left top))
               (coords (clipped-poly tr vector nil)))
          (with-clx-graphics () medium
            (xlib:draw-lines mirror gc coords :fill-p filled))))))

(defmethod medium-draw-rectangles* ((medium clx-medium) position-seq filled)
  (let ((length (length position-seq)))
    (assert (zerop (mod length 4)))
    (let ((points (make-array length))
          (index  0))
      (do-sequence ((left top right bottom) position-seq)
        (multiple-value-bind (min-x min-y width height)
            (clipped-rect (medium-native-transformation medium)
                          left top right bottom)
          (setf (aref points (+ index 0)) min-x)
          (setf (aref points (+ index 1)) min-y)
          (setf (aref points (+ index 2)) width)
          (setf (aref points (+ index 3)) height))
        (incf index 4))
      (with-clx-graphics () medium
        (xlib:draw-rectangles mirror gc points filled)))))

(defun %draw-rotated-ellipse (medium center-x center-y
                              radius-1-dx radius-1-dy
                              radius-2-dx radius-2-dy
                              start-angle end-angle filled)
  (let ((ellipse (make-ellipse* center-x center-y
                                radius-1-dx radius-1-dy
                                radius-2-dx radius-2-dy
                                :start-angle start-angle :end-angle end-angle)))
    (with-clx-graphics () medium
      (multiple-value-bind (x1 y1 width height)
          (region->clipping-values (bounding-rectangle ellipse))
        (labels ((ellipse-border-p (ellipse x-orig y-orig)
                   (with-slots (climi::tr climi::start-angle climi::end-angle) ellipse
                     (multiple-value-bind (x y) (untransform-position climi::tr x-orig y-orig)
                       (and (<= (- 1.0 .05) (+ (* x x) (* y y)) (+ 1.0 .05))
                            (or (null climi::start-angle)
                                (climi::arc-contains-angle-p
                                 (climi::%ellipse-position->angle ellipse x-orig y-orig)
                                 climi::start-angle climi::end-angle))))))
                 (draw-point (x y)
                   (if (< (line-style-thickness line-style) 2)
                       (let ((x (round-coordinate x))
                             (y (round-coordinate y)))
                         (xlib:draw-point mirror gc x y))
                       (let* ((radius (/ (line-style-thickness line-style) 2))
                              (min-x (round-coordinate (- x radius)))
                              (min-y (round-coordinate (- y radius)))
                              (max-x (round-coordinate (+ x radius)))
                              (max-y (round-coordinate (+ y radius))))
                         (xlib:draw-arc mirror gc min-x min-y
                                        (- max-x min-x) (- max-y min-y)
                                        0 (* 2 pi) t))))
                 (maybe-draw-border-points (line)
                   (multiple-value-bind (lx1 ly1) (line-start-point* line)
                     (when (ellipse-border-p ellipse lx1 ly1) (draw-point lx1 ly1)))
                   (multiple-value-bind (lx2 ly2) (line-end-point* line)
                     (when (ellipse-border-p ellipse lx2 ly2) (draw-point lx2 ly2))))
                 (draw-line-1 (line)
                   (multiple-value-bind (lx1 ly1) (line-start-point* line)
                     (multiple-value-bind (lx2 ly2) (line-end-point* line)
                       (xlib:draw-line mirror gc
                                       (round-coordinate lx1)
                                       (round-coordinate ly1)
                                       (round-coordinate lx2)
                                       (round-coordinate ly2)))))
                 (draw-lines (scan-line)
                   ;; XXX: this linep masks a problem with region-intersection.
                   (when (linep scan-line)
                     (cond
                       ((region-equal scan-line +nowhere+))
                       (filled (map-over-region-set-regions #'draw-line-1 scan-line))
                       (t (map-over-region-set-regions #'maybe-draw-border-points scan-line))))))
          ;; O(n+m) because otherwise we may skip some points (better drawing quality)
          (progn                      ;if (<= width height)
            (loop for x from x1 to (+ x1 width) do
              (draw-lines (region-intersection
                           ellipse
                           (make-line* x y1 x (+ y1 height)))))
            (loop for y from y1 to (+ y1 height) do
              (draw-lines (region-intersection
                           ellipse
                           (make-line* x1 y (+ x1 width) y))))))))))

;;; Round the parameters of the ellipse so that it occupies the expected pixels
(defmethod medium-draw-ellipse* ((medium clx-medium) center-x center-y
                                 rdx1 rdy1 rdx2 rdy2
                                 start-angle end-angle filled)
  (let ((tr (medium-native-transformation medium)))
    (with-transformed-position (tr center-x center-y)
      (climi::with-transformed-distance (tr rdx2 rdy2)
        (climi::with-transformed-distance (tr rdx1 rdy1)
          (if (or (= rdx2 rdy1 0) (= rdx1 rdy2 0))
              (let* ((arc-angle (- end-angle start-angle))
                     (arc-angle (if (< arc-angle 0)
                                    (+ (* pi 2) arc-angle)
                                    arc-angle)))
                (with-clx-graphics () medium
                  (let* ((radius-dx (abs (+ rdx1 rdx2)))
                         (radius-dy (abs (+ rdy1 rdy2)))
                         (min-x (round-coordinate (- center-x radius-dx)))
                         (min-y (round-coordinate (- center-y radius-dy)))
                         (max-x (round-coordinate (+ center-x radius-dx)))
                         (max-y (round-coordinate (+ center-y radius-dy))))
                    (xlib:draw-arc mirror gc
                                   min-x min-y (- max-x min-x) (- max-y min-y)
                                   (mod start-angle (* 2 pi)) arc-angle
                                   filled))))
              ;; Implementation scans for vertial or horizontal lines to get
              ;; the intersection. That is O(n), which is much better than
              ;; naive O(n2). This implementation may be numerically unstable
              ;; due to rounding errors. Until we introduce better rendering
              ;; mechanism we'll do the best we could without interruptint the
              ;; user program.
              (%draw-rotated-ellipse medium center-x center-y
                                     rdx1 rdy1 rdx2 rdy2
                                     start-angle end-angle filled)))))))

(defmethod medium-draw-circle* ((medium clx-medium)
                                center-x center-y radius start-angle end-angle
                                filled)
  (with-transformed-position
      ((medium-native-transformation medium) center-x center-y)
    (let* ((arc-angle (- end-angle start-angle))
           (arc-angle (if (< arc-angle 0)
                          (+ (* pi 2) arc-angle)
                          arc-angle))
           (min-x (round-coordinate (- center-x radius)))
           (min-y (round-coordinate (- center-y radius)))
           (max-x (round-coordinate (+ center-x radius)))
           (max-y (round-coordinate (+ center-y radius))))
      (with-clx-graphics () medium
        (xlib:draw-arc mirror gc
                       min-x min-y
                       (- max-x min-x) (- min-y max-y)
                       start-angle arc-angle
                       filled)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for text styles

(eval-when (:compile-toplevel :execute)
  ;; ASCII / CHAR-CODE compatibility checking
  (unless (equal (mapcar #'char-code '(#\Backspace #\Tab #\Linefeed
                                       #\Page #\Return #\Rubout))
                 '(8 9 10 12 13 127))
    (error "~S not ASCII-compatible for semi-standard characters: ~
           implement a CLX translate function for this implementation."
           'code-char))
  (let ((standard-chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))
    (dotimes (i 95)
      (unless (eql (char standard-chars i) (code-char (+ i 32)))
        (error "~S not ASCII-compatible for standard character ~S: ~
                implement a CLX translate function for this implementation."
               'code-char (code-char (+ i 32)))))))

(defun translate (src src-start src-end afont dst dst-start)
  (declare (type sequence src)
           (type xlib:array-index src-start src-end dst-start)
           (type (or null xlib:font) afont)
           (type vector dst))
  ;; FIXME: what if AFONT is null?
  (let ((min-char-index (xlib:font-min-char afont))
        (max-char-index (xlib:font-max-char afont)))
    (if (stringp src)
        (do ((i src-start (xlib::index+ i 1))
             (j dst-start (xlib::index+ j 1))
             (char))
            ((xlib::index>= i src-end)
             i)
          (declare (type xlib:array-index i j))
          (setq char (char-code (char src i)))
          (if (or (< char min-char-index) (> char max-char-index))
              ;; Character is not representable in the font.
              (return i)
              (setf (aref dst j) char)))
        (do ((i src-start (xlib::index+ i 1))
             (j dst-start (xlib::index+ j 1))
             (elt))
            ((xlib::index>= i src-end)
             i)
          (declare (type xlib:array-index i j))
          (setq elt (elt src i))
          (when (characterp elt)
            (setq elt (char-code elt)))
          (if (or (not (integerp elt))
                  (< elt min-char-index)
                  (> elt max-char-index))
              ;; Thing is not representable in the font.
              (return i)
              (setf (aref dst j) elt))))))

(defmethod medium-draw-text* ((medium clx-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (let ((merged-transform (medium-device-transformation medium)))
    (with-clx-graphics () medium
      (when (characterp string)
        (setq string (make-string 1 :initial-element string)))
      (if (null end)
          (setq end (length string))
          (setq end (min end (length string))))
      (multiple-value-bind (text-width text-height x-cursor y-cursor baseline)
          (text-size medium string :start start :end end)
        (declare (ignore x-cursor y-cursor))
        (unless (and (eq align-x :left) (eq align-y :baseline))
          (setq x (- x (ecase align-x
                         (:left 0)
                         (:center (round text-width 2)) ; worst case
                         (:right text-width))))         ; worst case
          (setq y (ecase align-y
                    (:top (+ y baseline))                              ; OK
                    (:baseline y)                                      ; OK
                    (:center (+ y baseline (- (floor text-height 2)))) ; change
                    (:baseline*  y)                                    ; change
                    (:bottom (+ y baseline (- text-height)))))))       ; change
      (multiple-value-bind (x y)
          (transform-position merged-transform x y)
        (xlib:draw-glyphs mirror gc (truncate (+ x 0.5)) (truncate (+ y 0.5)) string
                          :start start :end end :translate #'translate :size 16)))))


;;; Other Medium-specific Output Functions

(defmethod medium-finish-output ((medium clx-medium))
  (xlib:display-finish-output (clx-port-display (port medium))))

(defmethod medium-force-output ((medium clx-medium))
  (xlib:display-force-output (clx-port-display (port medium))))

(defmethod medium-clear-area ((medium clx-medium) left top right bottom)
  (let ((tr (medium-native-transformation medium)))
    (with-transformed-position (tr left top)
      (with-transformed-position (tr right bottom)
        (let ((min-x (round-coordinate (min left right)))
              (min-y (round-coordinate (min top bottom)))
              (max-x (round-coordinate (max left right)))
              (max-y (round-coordinate (max top bottom))))
          (let ((^cleanup nil))
            (when-let* ((mirror (clx-drawable medium))
                        (gc (medium-gcontext medium (medium-background medium))))
              (unwind-protect
                   (xlib:draw-rectangle mirror gc
                                        (clamp min-x           #x-8000 #x7fff)
                                        (clamp min-y           #x-8000 #x7fff)
                                        (clamp (- max-x min-x) 0       #xffff)
                                        (clamp (- max-y min-y) 0       #xffff)
                                        t)
                (mapc #'funcall ^cleanup)))))))))

(defmethod medium-beep ((medium clx-medium))
  (xlib:bell (clx-port-display (port medium))))

;;;;

(defmethod medium-miter-limit ((medium clx-medium))
  #.(* pi (/ 11 180)))
