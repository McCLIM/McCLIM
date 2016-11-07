(in-package :mcclim-render)

;;;
;;; Font utilities. 
;;;

(defstruct (glyph-info (:constructor glyph-info (id width height left right top)))
  id                                    ; FIXME: Types?
  width height
  left right top)

(defun font-generate-glyph (font glyph-index)
  (multiple-value-bind (arr left top dx dy) (glyph-pixarray font (code-char glyph-index))
    (let ((right (+ left (array-dimension arr 1))))
      (glyph-info 0 dx dy left right top))))

    
(defun font-glyph-info (font character)
  (font-generate-glyph font (char-code character)))

(defun font-glyph-id (font character)
  (glyph-info-id (font-glyph-info font character)))

(defmethod font-ascent ((font truetype-font))
  (truetype-font-ascent font))

(defmethod font-descent ((font truetype-font))
  (truetype-font-descent font))

(defmethod font-glyph-width ((font truetype-font) char)
  (glyph-info-width (font-glyph-info font char)))

(defmethod font-glyph-left ((font truetype-font) char)
  (glyph-info-left (font-glyph-info font char)))

(defmethod font-glyph-right ((font truetype-font) char)
  (glyph-info-right (font-glyph-info font char)))

;;; Simple custom cache for glyph IDs and widths. Much faster than
;;; using the char->glyph-info hash table directly.


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

(let ((font-loader-cache (make-hash-table :test #'equal))
      (font-families     (make-hash-table :test #'equal))
      (font-faces        (make-hash-table :test #'equal))
      (font-cache        (make-hash-table :test #'equal))
      (text-style-cache  (make-hash-table :test #'eql)))
  (defun make-truetype-font (port filename size)
    (climi::with-lock-held (*zpb-font-lock*)
      (let* ((loader (ensure-gethash filename font-loader-cache
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
                    (list loader size) font-cache
                    (make-instance 'truetype-font
				   :family nil
				   :name nil
				   :loader nil
                                   :face font-face
                                   :size 10))));;size))))
        ;;(pushnew family (font-families port))
        (ensure-gethash
         (make-text-style family-name face-name size) text-style-cache
         font))))

  (defun find-truetype-font (text-style)
    (gethash text-style text-style-cache)))


(defgeneric font-text-extents (font string &key start end translate))

(defmethod font-text-extents ((font truetype-font) string
                                        &key (start 0) (end (length string)) translate)
  ;; -> (width ascent descent left right
  ;; font-ascent font-descent direction
  ;; first-not-done)  
  (declare (optimize (speed 3))
           (ignore translate))
  (let ((width
         ;; We could work a little harder and eliminate generic arithmetic
         ;; here. It might shave a few percent off a draw-text benchmark.
         ;; Rather silly to obsess over the array access considering that.
         (macrolet ((compute ()
                      `(loop 
                          for i from start below end
                          as char = (aref string i)
                          as code = (char-code char)
                          sum (font-glyph-width font char))))
	   (typecase string 
	     (simple-string 
	      (locally (declare (type simple-string string))
		(compute)))
	     (string 
	      (locally (declare (type string string))
		(compute)))
	     (t (compute))))))
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
	       ;;(format t ">> ~A ~A~% " string width)
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
    (font-glyph-width xfont #\M)))

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


