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

;;; FIXME: I don't think draw-text* works for strings spanning
;;; multiple lines.  FIXME: Not particularly thread safe.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass truetype-font-renderer (clim-clx::font-renderer)
  ())

(setq clim:*default-server-path* '(:clx :font-renderer mcclim-truetype:truetype-font-renderer))

(let ((lookaside nil))
  (defun display-the-glyph-set (display)
    (if (eq (car lookaside) display)
        (cdr lookaside)
        (let ((glyph-set (or (getf (xlib:display-plist display) 'the-glyph-set)
                             (setf (getf (xlib:display-plist display) 'the-glyph-set)
                                   (xlib::render-create-glyph-set
                                    (first (xlib::find-matching-picture-formats display
                                            :alpha 8 :red 0 :green 0 :blue 0)))))))
          (setf lookaside (cons display glyph-set))
          glyph-set))))

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

(defstruct (glyph-info (:constructor glyph-info (id width height left right top)))
  id                                    ; FIXME: Types?
  width height
  left right top)


;;;;;;; mcclim interface
(defclass clx-truetype-font (truetype-font)
  ((display           :initarg :display :reader clx-truetype-font-display)
   (fixed-width       :initform nil)
   (glyph-id-cache    :initform (make-gcache))
   (glyph-width-cache :initform (make-gcache))
   (char->glyph-info  :initform (make-hash-table :size 256))))

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



(defun font-generate-glyph (font glyph-index)
  (let* ((display (clx-truetype-font-display font))
         (glyph-id (display-draw-glyph-id display)))
    (multiple-value-bind (arr left top dx dy) (glyph-pixarray font (code-char glyph-index))
      (with-slots (fixed-width) font
        (when (and (numberp fixed-width)
                   (/= fixed-width dx))
          (setf fixed-width t)
          (warn "Font ~A is fixed width, but the glyph width appears to vary.
 Disabling fixed width optimization for this font. ~A vs ~A" 
                font dx fixed-width))
        (when (and (numberp fixed-width)
                   (font-fixed-width-p font))
          (setf fixed-width dx)))

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
      (let ((right (+ left (array-dimension arr 1))))
        (glyph-info glyph-id dx dy left right top)))))

(defun font-glyph-info (font character)
  (with-slots (char->glyph-info) font
    (ensure-gethash character char->glyph-info
                    (font-generate-glyph font (char-code character)))))

(defun font-glyph-id (font character)
  (glyph-info-id (font-glyph-info font character)))

(defmethod clim-clx::font-ascent ((font truetype-font))
  (truetype-font-ascent font))

(defmethod clim-clx::font-descent ((font truetype-font))
  (truetype-font-descent font))

(defmethod clim-clx::font-glyph-width ((font truetype-font) char)
  (glyph-info-width (font-glyph-info font char)))

(defmethod clim-clx::font-glyph-left ((font truetype-font) char)
  (glyph-info-left (font-glyph-info font char)))

(defmethod clim-clx::font-glyph-right ((font truetype-font) char)
  (glyph-info-right (font-glyph-info font char)))

;;; Simple custom cache for glyph IDs and widths. Much faster than
;;; using the char->glyph-info hash table directly.

(defun make-gcache ()
  (let ((array (make-array 512 :adjustable nil :fill-pointer nil)))
    (loop for i from 0 below 256 do (setf (aref array i) (1+ i)))
    array))

(declaim (inline gcache-get))

(defun gcache-get (cache key-number)  
  (declare (optimize (speed 3))
           (type (simple-array t (512))))
  (let ((hash (logand (the fixnum key-number) #xFF)))   ; hello.
    (and (= key-number (the fixnum (svref cache hash)))
         (svref cache (+ 256 hash)))))

(defun gcache-set (cache key-number value)
  (let ((hash (logand key-number #xFF)))
    (setf (svref cache hash) key-number
          (svref cache (+ 256 hash)) value)))

(defmethod clim-clx::font-text-extents ((font truetype-font) string
                                        &key (start 0) (end (length string)) translate direction)
  ;; -> (width ascent descent left right
  ;; font-ascent font-descent direction
  ;; first-not-done)  
  (declare (optimize (speed 3))
           (ignore translate direction))

  (let ((width
         ;; We could work a little harder and eliminate generic arithmetic
         ;; here. It might shave a few percent off a draw-text benchmark.
         ;; Rather silly to obsess over the array access considering that.
         (macrolet ((compute ()
                      `(loop with width-cache = (slot-value font 'glyph-width-cache)
                          for i from start below end
                          as char = (aref string i)
                          as code = (char-code char)
                          sum (or (gcache-get width-cache code)
                                  (gcache-set width-cache code (clim-clx::font-glyph-width font char)))
                            #+NIL (clim-clx::font-glyph-width font char))))
           (if (numberp (slot-value font 'fixed-width))
               (* (slot-value font 'fixed-width) (- end start))
               (typecase string 
                 (simple-string 
                  (locally (declare (type simple-string string))
                    (compute)))
                 (string 
                  (locally (declare (type string string))
                    (compute)))
                 (t (compute)))))))
    (values
     width
     (clim-clx::font-ascent font)
     (clim-clx::font-descent font)
     (clim-clx::font-glyph-left font (char string start))
     (- width (- (clim-clx::font-glyph-width font (char string (1- end)))
                 (clim-clx::font-glyph-right font (char string (1- end)))))
     (clim-clx::font-ascent font)
     (clim-clx::font-descent font)
     0 end)))

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

;;; Arbitrary restriction: No more than 65536 glyphs cached on a
;;; single display. I don't think that's unreasonable. Extending 
;;; this from 16 to 32 bits is straightforward, at a slight loss
;;; in performance.

(let ((buffer (make-array 1024 :element-type '(unsigned-byte 16) ; TODO: thread safety
                               :adjustable nil :fill-pointer nil)))
  (defmethod clim-clx::font-draw-glyphs ((font truetype-font) mirror gc x y string
                                         &key start end translate size direction transformation)
    (declare (optimize (speed 3))
             (ignore size translate direction)
             (type #-sbcl (integer 0 #.array-dimension-limit)
                   #+sbcl sb-int:index
                   start end)
             (type string string))
    (multiple-value-bind (x y)
        (transform-position transformation x y)
      (when (< (length buffer) (- end start))
        (setf buffer (make-array (* 256 (ceiling (- end start) 256))
                                 :element-type '(unsigned-byte 16)
                                 :adjustable nil :fill-pointer nil)))
      (let ((display (xlib:drawable-display mirror)))
        (destructuring-bind (source-picture source-pixmap) (gcontext-picture mirror gc)
          (declare (ignore source-pixmap))
          (let* ((cache (slot-value font 'glyph-id-cache))
                 (glyph-ids buffer))

            (loop
              for i from start below end ; TODO: Read optimization notes. Fix. Repeat.
              for i* upfrom 0
              as char = (aref string i)
              as code = (char-code char)
              do (setf (aref buffer i*)
                       (the (unsigned-byte 16)
                            (or (gcache-get cache code)
                                (gcache-set cache code (font-glyph-id font char))))))

            ;; Debugging - show the text rectangle
                                        ;(setf (xlib:gcontext-foreground gc) #xFF0000)
                                        ;(xlib:draw-rectangle mirror gc x0 y0 (- x1 x0) (- y1 y0))

            ;; Sync the picture-clip-mask with that of the gcontext.
            (unless  (eq (xlib::picture-clip-mask (drawable-picture mirror))
                         (xlib::gcontext-clip-mask gc))
              (setf (xlib::picture-clip-mask (drawable-picture mirror))
                    (xlib::gcontext-clip-mask gc)))

            (xlib::render-composite-glyphs
             (drawable-picture mirror)
             (display-the-glyph-set display)
             source-picture
             (truncate (+ 0.5 x)) (truncate (+ y 0.5))
             glyph-ids
             :end (- end start))))))))

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

;;;;;;

(in-package :clim-clx)

(defmethod make-medium-gcontext* (medium foreground background line-style text-style (ink color) clipping-region)
  (let* ((drawable (sheet-mirror (medium-sheet medium)))
         (port (port medium)))
    (let ((gc (xlib:create-gcontext :drawable drawable)))
      (let ((fn (text-style-to-X-font port text-style)))
        (if (typep fn 'xlib:font)
            (setf (xlib:gcontext-font gc) fn)))
      (setf 
            (xlib:gcontext-foreground gc) (X-pixel port ink)
            )
      gc)))

;;; This fixes the worst offenders making the assumption that drawing
;;; would be idempotent.
;; moved on basic repaint protocol

#|
(defmethod clim:handle-repaint :around ((s clim:sheet-with-medium-mixin) r)
  (let ((m (clim:sheet-medium s))
        (r (clim:bounding-rectangle
            (clim:region-intersection r (clim:sheet-region s)))))
    (unless (eql r clim:+nowhere+)
      (clim:with-drawing-options (m :clipping-region r)
        ;; This causes applications which want to do a double-buffered repaint,
        ;; such as the logic cube, to flicker. On the other hand, it also
        ;; stops things such as the listener wholine from overexposing their
        ;; text. Who is responsible for clearing to the background color before
        ;; repainting?
        ;(clim:draw-design m r :ink clim:+background-ink+)
        (call-next-method s r)
        ;; FIXME: Shouldn't McCLIM always do this?
        (medium-force-output (sheet-medium s))))))
|#
