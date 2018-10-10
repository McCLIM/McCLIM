(in-package :mcclim-render-internals)

;;;
;;; Font utilities.
;;;
(defstruct (render-glyph-info
             (:include glyph-info)
             (:constructor render-glyph-info (id width height left right top advance-width advance-height paths opacity-image)))
  paths
  opacity-image)

(defclass render-truetype-font (mcclim-truetype::cached-truetype-font)
  ())

(defmethod font-generate-glyph ((font render-truetype-font) code &optional tr)
  (declare (ignore tr))
  (multiple-value-bind (paths left top width height dx dy)
      (glyph-paths font (code-char code))
    (let ((right (+ left width))
          (opacity-image (font-generate-opacity-image paths width height left top)))
      (render-glyph-info 0 dx dy left right top dx dy paths opacity-image))))

(defun font-glyph-info (font character)
  (with-slots (mcclim-truetype::char->glyph-info) font
    (ensure-gethash character mcclim-truetype::char->glyph-info
                    (font-generate-glyph font (char-code character)))))

(defun font-glyph-paths (font character)
  (render-glyph-info-paths (font-glyph-info font character)))

(defun font-glyph-opacity-image (font character)
  (render-glyph-info-opacity-image (font-glyph-info font character)))

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
  (let* ((image (make-image (1+ (* 1 width))
                            (1+ (* 1 height)))))
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

(defmethod climb:font-text-extents ((font render-truetype-font) string
                                    &key (start 0) (end (length string)) direction)
  ;; -> (width ascent descent left right
  ;; font-ascent font-descent direction
  ;; first-not-done)
  (declare (ignore direction))
  (let ((width
         ;; We could work a little harder and eliminate generic arithmetic
         ;; here. It might shave a few percent off a draw-text benchmark.
         ;; Rather silly to obsess over the array access considering that.
         (macrolet ((compute ()
                      `(loop with width-cache = (slot-value font 'mcclim-truetype::glyph-width-cache)
                          for i from start below end
                          as char = (aref string i)
                          as code = (char-code char)
                          sum (or (gcache-get width-cache code)
                                  (gcache-set width-cache code (max (climb:font-glyph-right font code)
                                                                    (climb:font-glyph-width font code))))
                            #+NIL (climb:font-glyph-width font char))))
           (if (climb:font-fixed-width font)
               (* (climb:font-fixed-width font) (- end start))
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
     (climb:font-ascent font)
     (climb:font-descent font)
     (climb:font-glyph-left font (char-code (char string start)))
     (- width (- (climb:font-glyph-width font (char-code (char string (1- end))))
                 (climb:font-glyph-right font (char-code (char string (1- end))))))
     (climb:font-ascent font)
     (climb:font-descent font)
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
                 (climb:font-text-extents xfont string
                                          :start start :end position-newline)
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
                 (climb:font-text-extents xfont string :start start :end end)
               (declare (ignorable left right
                                   font-ascent font-descent
                                   direction first-not-done))
               (values width (+ ascent descent) width 0 ascent)) )))) )

(defmethod text-style-ascent (text-style (medium render-medium-mixin))
  (let ((xfont (text-style-to-font (port medium) text-style)))
    (climb:font-ascent xfont)))

(defmethod text-style-descent (text-style (medium render-medium-mixin))
  (let ((xfont (text-style-to-font (port medium) text-style)))
    (climb:font-descent xfont)))

(defmethod text-style-height (text-style (medium render-medium-mixin))
  (+ (climb:text-style-ascent text-style medium)
     (climb:text-style-descent text-style medium)))

(defmethod text-style-width (text-style (medium render-medium-mixin))
  (let ((xfont (text-style-to-font (port medium) text-style)))
    (climb:font-character-width xfont #\m)))

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
                        (climb:font-text-extents xfont string :start start :end position-newline)
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
                        (climb:font-text-extents xfont string :start start :end end)
                      (declare (ignore width ascent descent)
                               (ignore direction first-not-done))
                      ;; FIXME: Potential style points:
                      ;; * (min 0 left), (max width right)
                      ;; * font-ascent / ascent
                      (values left (- font-ascent) right font-descent)))))))))
