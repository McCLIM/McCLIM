(defpackage :clim-freetype
  (:use :cl)
  (:export #:freetype-font-renderer))

(in-package :clim-freetype)

(setq clim:*default-server-path* '(:clx :font-renderer clim-freetype:freetype-font-renderer))

(defparameter *freetype-font-scale* 26.6)

(defclass freetype-font-renderer (clim-clx::font-renderer)
  ())

(setf (get :clx-freetype :server-path-parser) 'clim-clx::parse-clx-server-path)
(setf (get :clx-freetype :port-type) 'clx-freetype-port)

(defclass freetype-font-family (clim-extensions:font-family)
  ((faces :initform (make-hash-table :test 'equal)
          :reader freetype-font-family/faces)))

(defun find-font-family (port name)
  (let ((family (find name (clim-clx::font-families port) :key #'clim-extensions:font-family-name :test #'equal)))
    (or family
        (let ((v (make-instance 'freetype-font-family :port port :name name)))
          (push v (clim-clx::font-families port))
          v))))

(defclass cached-picture ()
  ((glyphset :initform nil
             :accessor cached-picture/glyphset)))

(defclass freetype-font-face (clim-extensions:font-face)
  ((file :initarg :file
         :reader freetype-font-face/file)
   (face :initarg :face
         :initform nil
         :accessor freetype-font-face/face)))

(defun find-or-load-face (f)
  (check-type f freetype-font-face)
  (or (freetype-font-face/face f)
      (setf (freetype-font-face/face f)
            (freetype2:new-face (freetype-font-face/file f)))))

(defclass freetype-font ()
  ((face           :initarg :face
                   :reader freetype-font/face)
   (size           :initarg :size
                   :initform 10
                   :reader freetype-font/size)
   (lock           :initform (bordeaux-threads:make-recursive-lock)
                   :reader freetype-font/lock)
   (cached-glyphs  :initform (make-hash-table :test 'eql)
                   :reader freetype-font/cached-glyphs)
   (cached-picture :type cached-picture
                   :reader freetype-font/cached-picture)))

(defmethod initialize-instance :after ((obj freetype-font) &key)
  (let ((cached (make-instance 'cached-picture)))
    (setf (slot-value obj 'cached-picture) cached)
    (trivial-garbage:finalize obj (lambda ()
                                    (alexandria:when-let ((glyphset (cached-picture/glyphset cached)))
                                      (xlib:render-free-glyph-set glyphset))))))

(defmethod print-object ((obj freetype-font) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "FACE ~s SIZE ~s" (freetype-font/face obj) (freetype-font/size obj))))

(defmacro with-size-face ((sym face size) &body body)
  (alexandria:once-only (face size)
    `(progn
       (freetype2:set-char-size ,face (round (* ,size 64)) 0 72 72)
       (let ((,sym ,face))
         ,@body))))

(defmacro with-face-from-font ((sym font) &body body)
  (alexandria:once-only (font)
    `(bordeaux-threads:with-recursive-lock-held ((freetype-font/lock ,font))
       (with-size-face (,sym (find-or-load-face (freetype-font/face ,font)) (freetype-font/size ,font))
         ,@body))))

(defmethod clim-extensions:font-face-all-sizes ((face freetype-font-face))
  '(8 12 18 20 24 36))

(defmethod clim-extensions:font-face-text-style ((face freetype-font-face) &optional size)
  (clim:make-text-style (clim-extensions:font-face-family face)
                        (clim-extensions:font-face-name face)
                        size))

(defun find-rgba-format (drawable)
  (let* ((formats (xlib::render-query-picture-formats (xlib:drawable-display drawable)))
         (format (find-if (lambda (v)
                            (and (= (car (xlib:picture-format-red-byte v)) 8)
                                 (= (car (xlib:picture-format-green-byte v)) 8)
                                 (= (car (xlib:picture-format-blue-byte v)) 8)
                                 (= (car (xlib:picture-format-alpha-byte v)) 8)))
                          formats)))
    (unless format
      (error "Can't find 8-bit RGBA format"))
    format))

(defun bitmap->array (bitmap)
  (let* ((width (/ (freetype2-types:ft-bitmap-width bitmap) 3))
         (height (freetype2-types:ft-bitmap-rows bitmap)))
    (assert (typep width 'integer))

    (if (or (zerop width) (zerop height))
        ;; Zero-sized glyphs causes the renderer to hang
        (make-array '(1 1) :element-type '(unsigned-byte 32) :initial-element 0)
        ;; Format the glyph array in a way that xrender accepts
        (let ((array (make-array (list height width) :element-type '(unsigned-byte 32) :initial-element 0)))
          (loop
            with buffer = (freetype2-types:ft-bitmap-buffer bitmap)
            with pitch = (freetype2-types:ft-bitmap-pitch bitmap)
            for y from 0 below height
            for ptr = (cffi:inc-pointer buffer (* y pitch))
            do (loop
                 for x from 0 below width
                 for v = (logior (ash (cffi:mem-ref ptr :unsigned-char (* x 3)) 16)
                                 (ash (cffi:mem-ref ptr :unsigned-char (+ (* x 3) 1)) 8)
                                 (cffi:mem-ref ptr :unsigned-char (+ (* x 3) 2))
                                 #xff000000)
                 do (setf (aref array y x) v)))
          array))))

(defun render-char-to-glyphset (glyphset face code)
  (freetype2:load-char face code '(:force-autohint))
  (let* ((glyph (freetype2-types:ft-face-glyph face))
         (advance (freetype2-types:ft-glyphslot-advance glyph))
         (bitmap (freetype2-types:ft-glyphslot-bitmap (freetype2:render-glyph glyph :lcd))))
    (xlib:render-add-glyph glyphset code
                           :x-origin (- (freetype2-types:ft-glyphslot-bitmap-left glyph))
                           :y-origin (freetype2-types:ft-glyphslot-bitmap-top glyph)
                           :x-advance (/ (freetype2-types:ft-vector-x advance) 64)
                           :y-advance 0 ; (/ (freetype2-types:ft-vector-y advance) 64)
                           :data (bitmap->array bitmap))))

(defun find-or-create-cached-glyphset (drawable font)
  (let ((cached (freetype-font/cached-picture font)))
    (or (cached-picture/glyphset cached)
        (setf (cached-picture/glyphset cached)
              (let ((format (find-rgba-format drawable)))
                (xlib:render-create-glyph-set format))))))

(defun create-glyphset (drawable font char-codes)
  (let ((glyphset (find-or-create-cached-glyphset drawable font))
        (cached-glyphs (freetype-font/cached-glyphs font)))
    (with-face-from-font (face font)
      (loop
        for code across char-codes
        unless (gethash code cached-glyphs)
          do (progn
               (render-char-to-glyphset glyphset face code)
               (setf (gethash code cached-glyphs) t))))
    glyphset))

(defun create-dest-picture (drawable)
  (xlib:render-create-picture drawable
                              :format (xlib:find-window-picture-format (xlib:drawable-root drawable))
                              :poly-edge :smooth
                              :poly-mode :precise))

(defun create-pen (drawable gc)
  (let* ((pixmap (xlib:create-pixmap :drawable (xlib:drawable-root drawable) :width 1 :height 1 :depth 32))
         (picture (xlib:render-create-picture pixmap :format (find-rgba-format drawable) :repeat :on))
         (fg (xlib::gcontext-foreground gc))
         (colour (list (ash (ldb (byte 8 16) fg) 8)
                       (ash (ldb (byte 8 8) fg) 8)
                       (ash (ldb (byte 8 0) fg) 8)
                       #xFFFF)))
    (xlib:render-fill-rectangle picture :over colour 0 0 1 1)
    (xlib:free-pixmap pixmap)
    picture))

(defmethod clim-clx::font-draw-glyphs ((font freetype-font) mirror gc x y string &key start end translate size)
  (declare (ignore translate size))
  (let* ((char-codes (map 'vector #'char-code string))
         (glyphset (create-glyphset mirror font char-codes)))
    (let ((source (create-pen mirror gc))
          (dest (create-dest-picture mirror)))
      (with-face-from-font (face font)
        (let ((fixed-p (freetype2:fixed-face-p face))
              (vec (make-array 1 :element-type 'integer :initial-element 0)))
          (loop with length = (length string)
                with rx = 0.0
                and ry = 0.0
                for i from (or start 0) below (or end length)
                as c1 = (aref string i)
                as c2 = (if (< i (1- length))
                            (aref string (1+ i))
                            nil)
                as kern = (if (and (not fixed-p)
                                   c2)
                              (freetype2:get-kerning face c1 c2)
                              0.0)
                do (let ((code (char-code c1)))
                     (freetype2:load-char face code '(:force-autohint))
                     (let* ((glyph (freetype2-types:ft-face-glyph face))
                            (advance (freetype2-types:ft-glyphslot-advance glyph))
                            (advance-pixels (/ (freetype2-types:ft-vector-x advance) 64)))
                       (let ((x-pos (round (+ rx x)))
                             (y-pos (round (+ ry  y))))
                         (setf (aref vec 0) code)
                         (xlib:render-composite-glyphs dest glyphset source x-pos y-pos vec)
                         (incf rx (+ advance-pixels kern))))))))
      (xlib:render-free-picture source)
      (xlib:render-free-picture dest))))

(defmethod clim-clx::font-text-extents ((font freetype-font) string &key (start 0) (end (length string)) translate)
  (declare (ignore translate))
  ;; Values to return:
  ;;   width ascent descent left right font-ascent font-descent direction first-not-done
  (with-face-from-font (face font)
    (let* ((s (subseq string start end))
           (width (freetype2:string-pixel-width face s)))
      (values width
              (freetype2:face-ascender-pixels face)
              (freetype2:face-descender-pixels face)
              0
              width
              (freetype2:face-ascender-pixels face)
              (freetype2:face-descender-pixels face)
              0
              end))))

(defmethod clim-clx::font-ascent ((font freetype-font))
  (with-face-from-font (face font)
    (freetype2:face-ascender-pixels face)))

(defmethod clim-clx::font-descent ((font freetype-font))
  (with-face-from-font (face font)
    (freetype2:face-descender-pixels face)))

(defun find-best-match (family face)
  (let ((family-expr (cond
                       ((typep family 'freetype-font-family) (list (cons "family" (clim-extensions:font-family-name family))))
                       ((eq family :fix) (list (cons "spacing" 100)))
                       ((eq family :serif) (list (cons "family" "DejaVu Serif")))
                       ((eq family :sans-serif) (list (cons "family" "DejaVu Sans")))
                       ((stringp family) (list (cons "family" family)))
                       (t (list (cons "family" "DejaVu Sans")))))
        (face-expr (cond
                     ((typep face 'freetype-font-face) (list (cons "style" (clim-extensions:font-face-name face))))
                     ((eq face :roman) (list (cons "weight" 80)))
                     ((eq face :bold) (list (cons "weight" 200)))
                     ((eq face :italic) (list (cons "slant" 100)))
                     ((stringp face) (list (cons "style" face)))
                     (t (list (cons "weight" 80))))))
    (let ((result (fontconfig:match-font (append family-expr face-expr))))
      (list (cdr (assoc :family result))
            (cdr (assoc :style result))
            (cdr (assoc :file result))))))

(defun find-freetype-font (port text-style)
  (multiple-value-bind (family face size)
      (clim:text-style-components text-style)
    (destructuring-bind (found-family found-style found-file)
        (find-best-match family face)
      (let* ((family-obj (find-font-family port found-family))
             (face-obj (alexandria:ensure-gethash found-style (freetype-font-family/faces family-obj)
                                                  (make-instance 'freetype-font-face
                                                                 :family family-obj
                                                                 :name found-style
                                                                 :file found-file))))
        (make-instance 'freetype-font
                       :face face-obj
                       :size (etypecase size
                               (keyword (or (getf clim-clx::*clx-text-sizes* size) 12))
                               (number size)
                               (null 12)))))))

(defmethod clim-clx::lookup-text-style-to-x-font ((port clim-clx::clx-port)
                                                  (renderer freetype-font-renderer)
                                                  (text-style clim:standard-text-style))
  (let ((x (or (clim:text-style-mapping port text-style)
               (setf (climi::text-style-mapping port text-style)
                     (find-freetype-font port text-style)))))
    x))

(defmethod clim-clx::lookup-text-style-to-x-font ((port clim-clx::clx-port)
                                                  (renderer freetype-font-renderer)
                                                  (text-style climi::device-font-text-style))
  nil)

;;;
;;;  List fonts
;;;

(defmethod clim-clx:port-find-all-font-families ((port clim-clx::clx-port) (font-renderer freetype-font-renderer)
                                                 &key invalidate-cache)
  (let ((h (make-hash-table :test 'equal))
        (existing-families (make-hash-table :test 'equal)))
    (unless invalidate-cache
      (loop
        for fam in (clim-clx::font-families port)
        do (setf (gethash (clim-extensions:font-family-name fam) existing-families) t)))
    (loop
      for font in (fontconfig:font-list)
      for family = (cdr (assoc :family font))
      for style = (cdr (assoc :style font))
      for file = (cdr (assoc :file font))
      for m = (alexandria:ensure-gethash family h
                                         (make-hash-table :test 'equal))
      do (setf (gethash style m) file))
    (setf (clim-clx::font-families port)
          (loop
            for family being each hash-key using (hash-value style-hash) in h
            unless (gethash family existing-families)
              collect (let ((f (make-instance 'freetype-font-family :name family :port port)))
                        (loop
                          with font-family-styles = (freetype-font-family/faces f)
                          for style being each hash-key using (hash-value file) in style-hash
                          unless (gethash style font-family-styles)
                            do (setf (gethash style font-family-styles)
                                     (make-instance 'freetype-font-face :name style :family f :file file)))
                        f))))
  (clim-clx::font-families port))

(defmethod clim-extensions:font-family-all-faces ((family freetype-font-family))
  (loop
    for face being each hash-value in (freetype-font-family/faces family)
    collect face))

;;;
;;;  Character info
;;;

(defmethod clim-clx::font-glyph-width ((font freetype-font) char)
  (with-face-from-font (face font)
    (freetype2:load-char face char)
    (let* ((glyph (freetype2-types:ft-face-glyph face))
           (metrics (freetype2-types:ft-glyphslot-metrics glyph)))
      (/ (freetype2-types:ft-glyph-metrics-width metrics) *freetype-font-scale*))))

(defmethod clim-clx::font-glyph-left ((font freetype-font) char)
  (with-face-from-font (face font)
    (freetype2:load-char face char)
    (let* ((glyph (freetype2-types:ft-face-glyph face))
           (metrics (freetype2-types:ft-glyphslot-metrics glyph)))
      (/ (freetype2-types:ft-glyph-metrics-hori-bearing-x metrics) *freetype-font-scale*))))

(defmethod clim-clx::font-glyph-right ((font freetype-font) char)
  (with-face-from-font (face font)
    (freetype2:load-char face char)
    (let* ((glyph (freetype2-types:ft-face-glyph face))
           (metrics (freetype2-types:ft-glyphslot-metrics glyph)))
      (/ (- (freetype2-types:ft-glyph-metrics-width metrics)
            (freetype2-types:ft-glyph-metrics-hori-advance metrics))
         *freetype-font-scale*))))

;;;
;;;  Needed overrides
;;;

#+nil
(defmethod clim::make-medium-gcontext* (medium foreground background line-style text-style (ink color) clipping-region)
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
