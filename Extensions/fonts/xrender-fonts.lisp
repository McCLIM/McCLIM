;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MCCLIM-TRUETYPE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Font matrics, caching, and XRender text support 
;;;   Created: 2003-05-25 16:32
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2003 by Gilbert Baumann
;;;  (c) copyright 2008 by Andy Hefner
;;;  (c) copyright 2016 by Daniel Kochmański
;;;
;;;    See toplevel file 'Copyright' for the copyright details.
;;;

(in-package :mcclim-truetype)

(declaim (optimize (speed 1) (safety 3) (debug 1) (space 0)))

;;;; Notes

;;; You might need to tweak mcclim-truetype::*families/faces* to point
;;; to where ever there are suitable TTF fonts on your system.

;;; FIXME: Not particularly thread safe.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass truetype-font-renderer (clim-clx::font-renderer)
  ())

(setq clim:*default-server-path* '(:clx :font-renderer mcclim-truetype:truetype-font-renderer))

(defun display-the-glyph-set (display)
  (let ((glyph-set (or (getf (xlib:display-plist display) 'the-glyph-set)
                       (setf (getf (xlib:display-plist display) 'the-glyph-set)
                             (xlib::render-create-glyph-set
                              (first (xlib::find-matching-picture-formats display
                                                                          :alpha 8 :red 0 :green 0 :blue 0)))))))
    glyph-set))

(defun display-free-the-glyph-set (display)
  (alexandria:when-let ((glyph-set (getf (xlib:display-plist display) 'the-glyph-set)))
    (xlib:render-free-glyph-set glyph-set)
    (remf (xlib:display-plist display) 'the-glyph-set)))

(defun display-free-glyph-ids (display)
  (getf (xlib:display-plist display) 'free-glyph-ids))

(defun (setf display-free-glyph-ids) (new-value display)
  (setf (getf (xlib:display-plist display) 'free-glyph-ids) new-value))

(defun display-free-glyph-id-counter (display)
  (getf (xlib:display-plist display) 'free-glyph-id-counter 0))

(defun (setf display-free-glyph-id-counter) (new-value display)
  (setf (getf (xlib:display-plist display) 'free-glyph-id-counter) new-value))

(defun display-draw-glyph-id (display)
  (or (pop (display-free-glyph-ids display))
      (incf (display-free-glyph-id-counter display))))


;;;;;;; mcclim interface
(defclass clx-truetype-font (cached-truetype-font)
  ((display           :initarg :display :reader clx-truetype-font-display)
   (%buffer%          :initform (make-array 1024
                                            :element-type '(unsigned-byte 32)
                                            :adjustable nil
                                            :fill-pointer nil)
                      :accessor clx-truetype-font-%buffer%
                      :type (simple-array (unsigned-byte 32)))))

(defun register-all-ttf-fonts (port &optional (dir *truetype-font-path*))
  (when *truetype-font-path*
    (dolist (path (directory (merge-pathnames "*.ttf" dir)))
      ;; make-truetype-font make fail if zpb can't load the particular
      ;; file - in that case it signals an error and no font is
      ;; created. In that case we just skip that file- hence IGNORE-ERRORS.
      (ignore-errors
        (map () #'(lambda (size)
                    (make-truetype-font port path size))
             '(8 10 12 14 18 24 48 72))))))

(defmethod clim-clx:port-find-all-font-families ((port clim-clx::clx-port) (font-renderer truetype-font-renderer)
                                                 &key invalidate-cache)
  (when (or (null (clim-clx::font-families port)) invalidate-cache)
    (setf (clim-clx::font-families port) (clim-clx::reload-font-table port)))
  (register-all-ttf-fonts port)
  (append (call-next-method)
          (clim-clx::font-families port)))

(let ((font-loader-cache (make-hash-table :test #'equal))
      (font-families     (make-hash-table :test #'equal))
      (font-faces        (make-hash-table :test #'equal))
      (font-cache        (make-hash-table :test #'equal))
      (text-style-cache  (make-hash-table :test #'equal)))
  (defun make-truetype-font (port filename size)
    (climi::with-lock-held (*zpb-font-lock*)
      (let* ((display (clim-clx::clx-port-display port))
             (loader (ensure-gethash filename font-loader-cache
                                     (zpb-ttf:open-font-loader filename)))
             (family-name (zpb-ttf:family-name loader))
             (family (ensure-gethash family-name font-families
                                     (make-instance 'truetype-font-family
                                                    :port port
                                                    :name (zpb-ttf:family-name loader))))
             (face-name (zpb-ttf:subfamily-name loader))
             (font-face (ensure-gethash
                         (list family-name face-name) font-faces
                         (make-instance 'truetype-face
                                        :family family
                                        :name (zpb-ttf:subfamily-name loader)
                                        :loader loader)))
	     (font (ensure-gethash
                    (list display loader size) font-cache
                    (make-instance 'clx-truetype-font
                                   :face font-face
                                   :display display
                                   :size size))))
        (pushnew family    (clim-clx::font-families port))
        (ensure-gethash
         (list port (make-text-style family-name face-name size))
         text-style-cache
         font))))

  (defun find-truetype-font (port text-style)
    (gethash (list port text-style) text-style-cache)))



(defmethod font-generate-glyph ((font clx-truetype-font) code &optional (tr +identity-transformation+))
  (let* ((display (clx-truetype-font-display font))
         (glyph-id (display-draw-glyph-id display))
         (character (code-char (ldb (byte 16 0) code)))
         (next-character (code-char (ldb (byte 16 16) code))))
    (multiple-value-bind (arr left top width height dx dy udx udy)
        (glyph-pixarray font character next-character tr)

      (with-slots (fixed-width) font
        (when (and (numberp fixed-width)
                   (/= fixed-width dx))
          (setf fixed-width t)
          (warn "Font ~A is fixed width, but the glyph width appears to vary.
Disabling fixed width optimization for this font. ~A vs ~A" font dx fixed-width)))

      (when (= (array-dimension arr 0) 0)
        (setf arr (make-array (list 1 1)
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)))
      (xlib::render-add-glyph (display-the-glyph-set display) glyph-id
                              :data arr
;;; We negate LEFT, because we want to start drawing array LEFT pixels after the
;;; pen (pixarray contains only a glyph without its left-side bearing). TOP is
;;; not negated because glyph coordinates are in the first quadrant (while
;;; array's are in fourth).
;;;
;;; I'm leaving this comment because it wasn't obvious to me why we negate LEFT
;;; and not TOP here. -- jd 2018-09-29
                              :x-origin (- left)
                              :y-origin top
                              :x-advance dx
                              :y-advance dy)
      (let ((right (+ left (array-dimension arr 1))))
        (glyph-info glyph-id width height left right top dx dy)))))

(defun font-generate-glyph* (font glyph-index transformation)
  (let* ((display (clx-truetype-font-display font))
         (glyph-id (display-draw-glyph-id display))
         (character (code-char (ldb (byte 16 0) glyph-index)))
         (next-character (code-char (ldb (byte 16 16) glyph-index))))
    (multiple-value-bind (arr left top width height dx dy udx udy)
        (glyph-pixarray font character next-character; transformation
                        (compose-transformations #1=(make-scaling-transformation 1.0 -1.0)
                                                 (compose-transformations transformation #1#)))
      (when (= (array-dimension arr 0) 0)
        (setf arr (make-array (list 1 1)
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)))
      (xlib::render-add-glyph (display-the-glyph-set display) glyph-id
                              :data arr
                              :x-origin (- left)
                              :y-origin top
                              :x-advance dx
                              :y-advance dy)
      ;; INV advance-width and advance-height are hacked here for transformed
      ;; glyph rendering. They are not transformed. See %RENDER-TRANSFORMED-GLYPHS.
      (let ((right (+ left (array-dimension arr 1))))
        (glyph-info glyph-id width height left right top udx udy)))))

(defmethod climb:font-text-extents ((font truetype-font) string
                                    &key (start 0) (end (length string)) direction)
  ;; -> (width ascent descent left right
  ;; font-ascent font-descent direction
  ;; first-not-done)  
  (declare (optimize (speed 3))
           (ignore direction)
           (fixnum start end))
  (let* ((last-char-code (char-code (char string (1- end))))
         (left-offset (climb:font-glyph-left font last-char-code))
         (width
          ;; We could work a little harder and eliminate generic arithmetic
          ;; here. It might shave a few percent off a draw-text benchmark.
          ;; Rather silly to obsess over the array access considering that.
           (macrolet ((compute ()
                        `(loop
                            with sum fixnum = (glyph-info-advance-width (font-glyph-info font last-char-code))
                            with char = (char string start)
                            for i from (1+ start) below end
                            as next-char = (char string i)
                            as next-char-code = (char-code next-char)
                            as code = (dpb next-char-code (byte
                                                           #.(ceiling (log char-code-limit 2))
                                                           #.(ceiling (log char-code-limit 2)))
                                           (char-code char))
                            ;; for i from start below end
                            ;; as code = (char-code (char string i))
                            do
                              (incf sum (glyph-info-advance-width (font-glyph-info font code)))
                              (setf char next-char)
                            finally (return sum))))
             (if (climb:font-fixed-width font)
                 (* (climb:font-fixed-width font) (- end start))
                 (typecase string
                   (simple-string
                    (locally (declare (type simple-string string))
                      (compute)))
                   (string
                    (locally (declare (type string string))
                      (compute)))
                   (t (compute))))))
         (ascent (climb:font-ascent font))
         (descent (climb:font-descent font))
         (height (+ ascent descent))
         (linegap (- (climb:font-leading font) height)))
    (values
       ;; bounding box: xmin ymin xmax ymax
       0 (- ascent) width descent
       ;; text properties: left top width height
       0 (- ascent) width height
       ;; line properties: ascent descent linegap
       ascent descent linegap
       ;; cursor motion: cursor-dx cursor-dy
       width 0)))

(defun drawable-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) 'picture)
      (setf (getf (xlib:drawable-plist drawable) 'picture)
            (xlib::render-create-picture drawable
                                         :format
                                         (xlib::find-window-picture-format
                                          (xlib:drawable-root drawable))))))

(defun gcontext-picture (drawable gcontext)
  (flet ((update-foreground (picture)
           ;; FIXME! This makes assumptions about pixel format, and breaks 
           ;; on e.g. 16 bpp displays.
           ;; It would be better to store xrender-friendly color values in
           ;; medium-gcontext, at the same time we set the gcontext 
           ;; foreground. That way we don't need to know the pixel format.
           (let ((fg (the xlib:card32 (xlib:gcontext-foreground gcontext))))
             (xlib::render-fill-rectangle picture
                                          :src                                          
                                          (list (ash (ldb (byte 8 16) fg) 8)
                                                (ash (ldb (byte 8 8) fg) 8)
                                                (ash (ldb (byte 8 0) fg) 8)
                                                #xFFFF)
                                          0 0 1 1))))
    (let* ((fg (xlib:gcontext-foreground gcontext))
           (picture-info
            (or (getf (xlib:gcontext-plist gcontext) 'picture)
                (setf (getf (xlib:gcontext-plist gcontext) 'picture)
                      (let* ((pixmap (xlib:create-pixmap 
                                      :drawable drawable
                                      :depth (xlib:drawable-depth drawable)
                                      :width 1 :height 1))
                             (picture (xlib::render-create-picture
                                       pixmap
                                       :format (xlib::find-window-picture-format
                                                (xlib:drawable-root drawable))
                                       :repeat :on)))
                        (update-foreground picture)
                        (list fg
                             picture
                             pixmap))))))
      (unless (eql fg (first picture-info))
        (update-foreground (second picture-info))
        (setf (first picture-info) fg))
      (cdr picture-info))))

;;; Restriction: no more than 65536 glyph pairs cached on a single display. I
;;; don't think that's unreasonable. Having keys as glyph pairs is essential for
;;; kerning where the same glyph may have different advance-width values for
;;; different next elements. (byte 16 0) is the character code and (byte 16 16)
;;; is the next character code. For standalone glyphs (byte 16 16) is zero.
(defun mcclim-font:draw-glyphs (medium mirror gc x y string
                                &key start end translate direction
                                  transformation transform-glyphs
                                &aux (font (clim-clx::text-style-to-X-font
                                            (port medium) (medium-text-style medium))))
  (declare (optimize (speed 3))
           (ignore translate direction)
           (type #-sbcl (integer 0 #.array-dimension-limit)
                 #+sbcl sb-int:index
                 start end)
           (type string string))

  (when (< (length (the (simple-array (unsigned-byte 32)) (clx-truetype-font-%buffer% font)))
           (- end start))
    (setf (clx-truetype-font-%buffer% font)
          (make-array (* 256 (ceiling (- end start) 256))
                      :element-type '(unsigned-byte 32)
                      :adjustable nil :fill-pointer nil)))

  (when (and transform-glyphs
             (not (clim:translation-transformation-p transformation)))
    (return-from mcclim-font:draw-glyphs
      (%render-transformed-glyphs font string x y transformation mirror gc)))

  (multiple-value-setq (x y) (clim:transform-position transformation x y))
  (let ((glyph-ids (clx-truetype-font-%buffer% font))
        (glyph-set (display-the-glyph-set (xlib:drawable-display mirror))))
    (loop
       with char = (char string start)
       with i* = 0
       for i from (1+ start) below end
       as next-char = (char string i)
       as next-char-code = (char-code next-char)
       as code = (dpb next-char-code (byte #.(ceiling (log char-code-limit 2))
                                           #.(ceiling (log char-code-limit 2)))
                      (char-code char))
       do
         (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) i*)
               (the (unsigned-byte 32) (font-glyph-id font code)))
         (setf char next-char)
         (incf i*)
       finally
         (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) i*)
               (the (unsigned-byte 32) (font-glyph-id font (char-code char)))))

    (destructuring-bind (source-picture source-pixmap) (gcontext-picture mirror gc)
      (declare (ignore source-pixmap))
      ;; Sync the picture-clip-mask with that of the gcontext.
      (unless  (eq (xlib::picture-clip-mask (drawable-picture mirror))
                   (xlib::gcontext-clip-mask gc))
        (setf (xlib::picture-clip-mask (drawable-picture mirror))
              (xlib::gcontext-clip-mask gc)))

      (xlib::render-composite-glyphs (drawable-picture mirror)
                                     glyph-set
                                     source-picture
                                     (truncate (+ x 0.5))
                                     (truncate (+ y 0.5))
                                     glyph-ids
                                     :end (- end start)))))

;;; Transforming glyphs is very inefficient because we don't cache them.
(defun %render-transformed-glyphs (font string x y tr mirror gc
                                   &aux (end (length string)))
  ;; Sync the picture-clip-mask with that of the gcontext.

  ;; XXX: TYPE-ERROR "The value 54530151 is not of type (INTEGER 0 0) when
  ;; binding #:G2" in XLIB:GCONTEXT-CLIP-MASK null clip branch form:
  ;;
  ;;     (DECODE-TYPE (OR (MEMBER :NONE) PIXMAP) CLIP-MASK)
  ;;
  ;; is signalled if we change window size frequently and refresh the output
  ;; recording. I'm not sure about source of this error – it may also be a case
  ;; with untransformed rendering. I was unable to figure out what causes this
  ;; error. That's why we wrap clip in IGNORE-ERRORS.
  (when-let ((clip (ignore-errors (xlib::gcontext-clip-mask gc))))
    (unless (eq (xlib::picture-clip-mask (drawable-picture mirror)) clip))
    (setf (xlib::picture-clip-mask (drawable-picture mirror)) clip))

  (loop
     with glyph-transformation = (multiple-value-bind (x0 y0)
                                     (transform-position tr 0 0)
                                   (compose-translation-with-transformation tr (- x0) (- y0)))
     ;; for rendering one glyph at a time
     with current-x = x
     with current-y = y
     with picture = (drawable-picture mirror)
     with source-picture = (car (gcontext-picture mirror gc))
     ;; ~
     with glyph-ids = (clx-truetype-font-%buffer% font)
     with glyph-set = (display-the-glyph-set (xlib:drawable-display mirror))
     with char = (char string 0)
     with i* = 0
     for i from 1 below end
     as next-char = (char string i)
     as next-char-code = (char-code next-char)
     as code = (dpb next-char-code (byte #.(ceiling (log char-code-limit 2))
                                         #.(ceiling (log char-code-limit 2)))
                    (char-code char))
     as glyph-info = (font-generate-glyph* font code glyph-transformation)
     do
       (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) i*)
             (the (unsigned-byte 32) (glyph-info-id glyph-info)))
     do ;; rendering one glyph at a time
       (multiple-value-bind (current-x current-y)
           (transform-position tr current-x current-y)
         (xlib:render-composite-glyphs picture glyph-set source-picture
                                       (truncate (+ current-x 0.5))
                                       (truncate (+ current-y 0.5))
                                       glyph-ids :start i* :end (1+ i*)))
     ;; INV advance values are untransformed - see FONT-GENERATE-GLYPH*.
       (incf current-x (glyph-info-advance-width glyph-info))
       (incf current-y (glyph-info-advance-height glyph-info))
     do
       (setf char next-char)
       (incf i*)
     finally
       (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) i*)
             (the (unsigned-byte 32) (glyph-info-id (font-generate-glyph* font
                                                                          (char-code char)
                                                                          glyph-transformation))))
     finally
     ;; rendering one glyph at a time (last glyph)
       (multiple-value-bind (current-x current-y)
           (transform-position tr current-x current-y)
         (xlib:render-composite-glyphs picture glyph-set source-picture
                                       (truncate (+ current-x 0.5))
                                       (truncate (+ current-y 0.5))
                                       glyph-ids :start i* :end (1+ i*)))
       (xlib:render-free-glyphs glyph-set (subseq glyph-ids 0 (1+ i*)))
       #+ (or) ;; rendering all glyphs at once
       (destructuring-bind (source-picture source-pixmap) (gcontext-picture mirror gc)
         (declare (ignore source-pixmap))
         ;; This solution is correct in principle, but advance-width and
         ;; advance-height are victims of rounding errors and they don't
         ;; hold the line for longer text in case of rotations and other
         ;; hairy transformations. That's why we take our time and
         ;; render one glyph at a time. -- jd 2018-10-04
         (multiple-value-bind (x y) (clim:transform-position tr x y)
           (xlib::render-composite-glyphs (drawable-picture mirror)
                                          glyph-set
                                          source-picture
                                          (truncate (+ x 0.5))
                                          (truncate (+ y 0.5))
                                          glyph-ids :start 0 :end end)))))

(defstruct truetype-device-font-name
  (font-file (error "missing argument"))
  (size      (error "missing argument")))

(defstruct fontconfig-font-name
  (string (error "missing argument"))
  (size   (error "missing argument"))
  (options nil)
  (device-name nil))

(defmethod clim-clx::lookup-text-style-to-X-font ((port clim-clx::clx-port)
                                           (font-renderer truetype-font-renderer)
                                           (text-style climi::device-font-text-style))
  (let ((font-name (climi::device-font-name text-style)))
    (when (stringp font-name)
      (setf (climi::device-font-name text-style)
            (make-fontconfig-font-name :string font-name
                                       :size (getf clim-clx::*clx-text-sizes* :normal))
            font-name (climi::device-font-name text-style)))
    (etypecase font-name
      (truetype-device-font-name
       (make-truetype-font port
                           (namestring (truetype-device-font-name-font-file font-name))
                           (truetype-device-font-name-size font-name)))
      (fontconfig-font-name
       (clim-clx::text-style-to-X-font
        port
        (or (fontconfig-font-name-device-name font-name)
            (setf (fontconfig-font-name-device-name font-name)
                  (make-device-font-text-style
                   port
                   (make-truetype-device-font-name 
                    :font-file (find-fontconfig-font
                                (format nil "~A-~A~{:~A~}"
                                        (namestring (fontconfig-font-name-string font-name))
                                        (fontconfig-font-name-size font-name)
                                        (fontconfig-font-name-options font-name)))
                    :size (fontconfig-font-name-size font-name))))))))))

(define-condition missing-font (simple-error)
  ((filename :reader missing-font-filename :initarg :filename)
   (text-style :reader missing-font-text-style :initarg :text-style))
  (:report (lambda (condition stream)
             (format stream  "Cannot access ~W (~a)
Your *truetype-font-path* is currently ~W
The following files should exist:~&~{  ~A~^~%~}"
                     (missing-font-filename condition)
                     (missing-font-text-style condition)
                     *truetype-font-path*
                     (mapcar #'cdr *families/faces*)))))

(defmethod clim-clx::lookup-text-style-to-X-font ((port clim-clx::clx-port)
                                                  (font-renderer truetype-font-renderer)
                                                  (text-style standard-text-style))
  (labels
      ((find-and-make-truetype-font (family face size)
         (let* ((font-path-maybe-relative
                 (cdr (assoc (list family face) *families/faces*
                             :test #'equal)))
                (font-path
                 (and font-path-maybe-relative
                      (case (car (pathname-directory
                                  font-path-maybe-relative))
                        (:absolute font-path-maybe-relative)
                        (otherwise (merge-pathnames
                                    font-path-maybe-relative
                                    (or *truetype-font-path* "")))))))
           (if (and font-path (probe-file font-path))
               (make-truetype-font port font-path size)
               ;; We could error here, but we want to fallback to
               ;; fonts provided by CLX server. Its better to have
               ;; ugly fonts than none at all.
               (or (call-next-method)
                   (error 'missing-font
                          :filename font-path
                          :text-style text-style)))))
       (find-font ()
         (multiple-value-bind (family face size)
             (clim:text-style-components text-style)

           (setf face   (or face :roman)
                 family (or family :fix)
                 size   (or size :normal)
                 size   (getf clim-clx::*clx-text-sizes* size size))

           (find-and-make-truetype-font family face size))))

    (or (text-style-mapping port text-style)
        (setf (climi::text-style-mapping port text-style)
              (or (find-truetype-font port text-style)
                  (invoke-with-truetype-path-restart #'find-font))))))
