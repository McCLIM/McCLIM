(in-package :mcclim-render-internals)

;;;
;;; Font utilities.
;;;

(defstruct (glyph-info (:constructor glyph-info (id width height left right top dx dy paths opacity-image)))
  id                                    ; FIXME: Types?
  width height
  left right top
  dx
  dy
  paths
  opacity-image)

(defclass render-truetype-font (truetype-font)
  ((fixed-width       :initform nil)
   (glyph-id-cache    :initform (make-gcache))
   (glyph-width-cache :initform (make-gcache))
   (char->glyph-info  :initform (make-hash-table :size 256))))

(defun font-generate-glyph (font glyph-index)
  (multiple-value-bind (paths left top width height dx dy) (glyph-paths font (code-char glyph-index))
    (let ((right (+ left width))
	  (opacity-image (font-generate-opacity-image paths width height left top)))
      (glyph-info 0 dx dy left right top dx dy paths opacity-image))))

(defun font-glyph-info (font character)
  (with-slots (char->glyph-info) font
    (ensure-gethash character char->glyph-info
                    (font-generate-glyph font (char-code character)))))

(defun font-glyph-id (font character)
  (glyph-info-id (font-glyph-info font character)))

(defun font-glyph-paths (font character)
  (glyph-info-paths (font-glyph-info font character)))

(defun font-glyph-opacity-image (font character)
  (glyph-info-opacity-image (font-glyph-info font character)))

(defmethod font-ascent ((font truetype-font))
  (truetype-font-ascent font))

(defmethod font-descent ((font truetype-font))
  (truetype-font-descent font))

(defmethod font-glyph-width ((font truetype-font) char)
  (glyph-info-width (font-glyph-info font char)))

(defmethod font-glyph-height ((font truetype-font) char)
  (glyph-info-height (font-glyph-info font char)))

(defmethod font-glyph-dx ((font truetype-font) char)
  (glyph-info-dx (font-glyph-info font char)))

(defmethod font-glyph-dy ((font truetype-font) char)
  (glyph-info-dy (font-glyph-info font char)))

(defmethod font-glyph-left ((font truetype-font) char)
  (glyph-info-left (font-glyph-info font char)))

(defmethod font-glyph-right ((font truetype-font) char)
  (glyph-info-right (font-glyph-info font char)))

(defmethod font-glyph-top ((font truetype-font) char)
  (glyph-info-top (font-glyph-info font char)))

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

;;;
;;; font to opacity mask image
;;;

(defun font-generate-opacity-image (paths width height dx dy)
  (let* ((image (make-image :gray
                            (1+ (* 1 width))
                            (1+ (* 1 height)) :opticl)))
    (if (= (* width height) 0)
	nil
        (aa-fill-alpha-paths image nil paths (aa:make-state)
                       (make-translation-transformation
                        (- dx) dy)
                       (make-rectangle* 0 0 (* 1 width) (* 1 height))))
    image))

;;;
;;; text geometry
;;;

(defgeneric font-text-extents (font string &key start end translate))

(defmethod font-text-extents ((font render-truetype-font) string
                                        &key (start 0) (end (length string)) translate)
  ;; -> (width ascent descent left right
  ;; font-ascent font-descent direction
  ;; first-not-done)
  (declare ;;(optimize (speed 3))
           (ignore translate))
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
                                  (gcache-set width-cache code (max (font-glyph-right font char)
                                                                    (font-glyph-width font char))))
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
     (font-ascent font)
     (font-descent font)
     (font-glyph-left font (char string start))
     (- width (- (font-glyph-width font (char string (1- end)))
		 (font-glyph-right font (char string (1- end)))))
     (font-ascent font)
     (font-descent font)
     0 end)))

(defmethod text-size ((medium render-medium-mixin) string
                      &key text-style (start 0) end)
  (declare (optimize (speed 3)))
  (when (characterp string)
    (setf string (make-string 1 :initial-element string)))
  (check-type string string)

  (unless end (setf end (length string)))
  (check-type start (integer 0 #.array-dimension-limit))
  (check-type end (integer 0 #.array-dimension-limit))

  (when (= start end)
    (return-from text-size (values 0 0 0 0 0)))

  (unless text-style
    (setf text-style (medium-text-style medium)))
  (let ((xfont (text-style-to-font (port medium) text-style)))
    (let ((position-newline
           (macrolet ((p (type)
                        `(locally (declare (type ,type string))
                           (position #\newline string :start start :end end))))
             (typecase string
               (simple-base-string (p simple-base-string))
               #+SBCL (sb-kernel::simple-character-string (p sb-kernel::simple-character-string))
               #+SBCL (sb-kernel::character-string (p sb-kernel::character-string))
               (simple-string (p simple-string))
               (string (p string))))))
      (cond ((not (null position-newline))
             (multiple-value-bind (width ascent descent left right
                                         font-ascent font-descent direction
                                         first-not-done)
                 (font-text-extents xfont string
                                    :start start :end position-newline
                                    )
               (declare (ignorable left right
                                   font-ascent font-descent
                                   direction first-not-done))
               (multiple-value-bind (w h x y baseline)
                   (text-size medium string :text-style text-style
                              :start (1+ position-newline) :end end)
                 (values (max w width) (+ ascent descent h)
                         x (+ ascent descent y) (+ ascent descent baseline)))))
            (t
             (multiple-value-bind (width ascent descent left right
                                         font-ascent font-descent direction
                                         first-not-done)
                 (font-text-extents xfont string
                                    :start start :end end
                                    )
               (declare (ignorable left right
                                   font-ascent font-descent
                                   direction first-not-done))
               (values width (+ ascent descent) width 0 ascent)) )))) )

(defmethod text-style-ascent (text-style (medium render-medium-mixin))
  (let ((xfont (text-style-to-font (port medium) text-style)))
    (font-ascent xfont)))

(defmethod text-style-descent (text-style (medium render-medium-mixin))
  (let ((xfont (text-style-to-font (port medium) text-style)))
    (font-descent xfont)))

(defmethod text-style-height (text-style (medium render-medium-mixin))
  (+ (text-style-ascent text-style medium)
     (text-style-descent text-style medium)))

(defmethod text-style-width (text-style (medium render-medium-mixin))
  (let ((xfont (text-style-to-font (port medium) text-style)))
    (font-glyph-width xfont #\m)))

(defmethod climi::text-bounding-rectangle*
    ((medium render-medium-mixin) string &key text-style (start 0) end)
  (when (characterp string)
    (setf string (make-string 1 :initial-element string)))
  (unless end (setf end (length string)))
  (unless text-style (setf text-style (medium-text-style medium)))
  (let ((xfont (text-style-to-font (port medium) text-style)))
    (cond ((= start end)
           (values 0 0 0 0))
          (t
           (let ((position-newline (position #\newline string :start start :end end)))
             (cond ((not (null position-newline))
                    (multiple-value-bind (width ascent descent left right
                                                font-ascent font-descent direction
                                                first-not-done)
                        (font-text-extents xfont string
                                           :start start :end position-newline
                                           )
                      (declare (ignorable width left right
                                          font-ascent font-descent
                                          direction first-not-done))
                      (multiple-value-bind (minx miny maxx maxy)
                          (climi::text-bounding-rectangle*
                           medium string :text-style text-style
                           :start (1+ position-newline) :end end)
			(declare (ignore miny))
                        (values (min minx left) (- ascent)
                                (max maxx right) (+ descent maxy)))))
                   (t
                    (multiple-value-bind (width ascent descent left right
                                                font-ascent font-descent direction
                                                first-not-done)
                        (font-text-extents
                         xfont string :start start :end end)
                      (declare (ignore width ascent descent)
			       (ignore direction first-not-done))
                      ;; FIXME: Potential style points:
                      ;; * (min 0 left), (max width right)
                      ;; * font-ascent / ascent
                      (values left (- font-ascent) right font-descent)))))))))
