;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MCCLIM-FREETYPE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Experimental FreeType support
;;;   Created: 2003-05-25 16:32
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2003 by Gilbert Baumann
;;;  (c) copyright 2008 by Andy Hefner

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :MCCLIM-FREETYPE)

(declaim (optimize (speed 1) (safety 3) (debug 1) (space 0)))

;;;; Notes

;; You might need to tweak mcclim-freetype::*families/faces* to point
;; to where ever there are suitable TTF fonts on your system.

(defclass vague-font ()
  ((lib :initarg :lib)
   (filename :initarg :filename)))

;;; I can't say I understand this vague vs. concrete font distinction,
;;; but I'll leave it around. -Hefner

(defparameter *vague-font-hash* (make-hash-table :test #'equal))

(defun make-vague-font (filename)
  (let ((val (gethash filename *vague-font-hash*)))
    (or val
        (setf (gethash filename *vague-font-hash*)
              (make-instance 'vague-font
                             :lib (let ((libf (make-alien freetype:library)))
                                    (declare (type (alien (* freetype:library)) libf))
                                    (freetype:init-free-type libf)
                                    (deref libf))
                             :filename filename)))))

(defparameter *dpi* 72)

(defparameter *concrete-font-hash* (make-hash-table :test #'equal))

;;; One "concrete font" is shared for a given face, regardless of text size,
;;; presumably to conserve resources. Therefore, we must configure it for
;;; the correct text size with set-concrete-font-size before using it.

(defun make-concrete-font (vague-font size &key (dpi *dpi*))
  (with-slots (lib filename) vague-font
    (let* ((key (cons lib filename))
           (val (gethash key *concrete-font-hash*)))
      (unless val
        (let ((facef (make-alien freetype:face)))
          (declare (type (alien (* freetype:face)) facef))
          (if (zerop (freetype:new-face lib filename 0 facef))
              (setf val (setf (gethash key *concrete-font-hash*)
                              (deref facef)))
              (error "Freetype error in make-concrete-font"))))
      val)))

(defun set-concrete-font-size (face size dpi)
  (declare (type (alien freetype:face) face))
  (freetype:set-char-size face 0 (round (* size 64)) (round dpi) (round dpi))
  face)

(defun glyph-pixarray (face char)
  (declare (optimize (speed 3) (debug 1))
           (inline freetype:load-glyph freetype:render-glyph)
           (type (alien freetype:face) face))
  (freetype:load-glyph face (freetype:get-char-index face (char-code char)) 0)
  (freetype:render-glyph (slot face 'freetype:glyph) 0)
  (symbol-macrolet
      ((glyph (slot face 'freetype:glyph))
       (bm (slot glyph 'freetype:bitmap)))
    (let* ((width  (slot bm 'freetype:width))
           (pitch  (slot bm 'freetype:pitch))
           (height (slot bm 'freetype:rows))
           (buffer (slot bm 'freetype:buffer))
           (res    (make-array (list height width) :element-type '(unsigned-byte 8))))
      (declare (type (simple-array (unsigned-byte 8) (* *)) res))
      (let ((m (* width height)))
        (locally
            (declare (optimize (speed 3) (safety 0)))
          (loop for y*width of-type fixnum below m by width 
                for y*pitch of-type fixnum from 0 by pitch do
                (loop for x of-type fixnum below width do
                      (setf (row-major-aref res (+ x y*width))
                            (deref buffer (+ x y*pitch)))))))
      (values
       res
       (slot glyph 'freetype:bitmap-left)
       (slot glyph 'freetype:bitmap-top)
       (/ (slot (slot glyph 'freetype:advance) 'freetype:x) 64)
       (/ (slot (slot glyph 'freetype:advance) 'freetype:y) 64)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defparameter *font-hash*
  (make-hash-table :test #'equalp))

(defstruct (glyph-info (:constructor glyph-info (id width height left right top)))
  id                                    ; FIXME: Types?
  width height
  left right top)

(defun font-generate-glyph (font glyph-index)
  (let* ((display (freetype-face-display font))
         (glyph-id (display-draw-glyph-id display))
         (face (freetype-face-concrete-font font)))
    (set-concrete-font-size face (freetype-face-matrix font) *dpi*)
    (multiple-value-bind (arr left top dx dy) (glyph-pixarray face (code-char glyph-index))
      (with-slots (fixed-width) font
        (when (and (numberp fixed-width)
                   (/= fixed-width dx))
          (setf fixed-width t)
          (warn "Font ~A is fixed width, but the glyph width appears to vary.
 Disabling fixed width optimization for this font. ~A vs ~A" 
                 font dx fixed-width))
        (unless (or fixed-width
                    (zerop (logand (slot face 'freetype:face-flags)
                                   4)))    ; FT_FACE_FLAG_FIXED_WIDTH
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

;;;;;;; mcclim interface
(defclass freetype-face ()
  ((display :initarg :display :reader freetype-face-display)
   (font   :initarg :font :reader freetype-face-name)
   (matrix :initarg :matrix :reader freetype-face-matrix)
   (ascent :initarg :ascent :reader freetype-face-ascent)
   (descent :initarg :descent :reader freetype-face-descent)
   (concrete-font :initarg :concrete-font :reader freetype-face-concrete-font)
   (fixed-width :initform nil)
   (glyph-id-cache :initform (make-gcache))
   (glyph-width-cache :initform (make-gcache))
   (char->glyph-info :initform (make-hash-table :size 256))))

(defmethod print-object ((object freetype-face) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-slots (font matrix ascent descent) object
      (format stream "~A size=~A ~A/~A" font matrix ascent descent))))

(defun font-glyph-info (font character)
  (with-slots (char->glyph-info) font
    (or (gethash character char->glyph-info)
        (setf (gethash character char->glyph-info) 
              (font-generate-glyph font (char-code character))))))

(defun font-glyph-id (font character)
  (glyph-info-id (font-glyph-info font character)))

(defmethod clim-clx::font-ascent ((font freetype-face))
  (freetype-face-ascent font))

(defmethod clim-clx::font-descent ((font freetype-face))
  (freetype-face-descent font))

(defmethod clim-clx::font-glyph-width ((font freetype-face) char)
  (glyph-info-width (font-glyph-info font char)))

(defmethod clim-clx::font-glyph-left ((font freetype-face) char)
  (glyph-info-left (font-glyph-info font char)))

(defmethod clim-clx::font-glyph-right ((font freetype-face) char)
  (glyph-info-right (font-glyph-info font char)))


(defun make-gcache () 
  (let ((array (make-array 512 :adjustable nil :fill-pointer nil)))
    (loop for i from 0 below 256 do (setf (aref array i) (1+ i)))
    array))

(declaim (inline gcache-get))

(defun gcache-get (cache key-number)  
  (declare (optimize (speed 3))
           (type (simple-array t (512))))
  (let ((hash (logand (the fixnum key-number) #xFF))) ; best hash function ever.
    (and (= key-number (the fixnum (svref cache hash))) ; I <3 fixnums
         (svref cache (+ 256 hash)))))

(defun gcache-set (cache key-number value)
  (let ((hash (logand key-number #xFF)))
    (setf (svref cache hash) key-number
          (svref cache (+ 256 hash)) value)))

;;; this is a hacky copy of XLIB:TEXT-EXTENTS
(defmethod clim-clx::font-text-extents ((font freetype-face) string
                                        &key (start 0) (end (length string)) translate)
  ;; -> (width ascent descent left right
  ;; font-ascent font-descent direction
  ;; first-not-done)  
  (declare (optimize (speed 3)))
  translate                             ; ???
  (let ((width
         ;; We could work a little harder and maybe get the generic arithmetic
         ;; out of here, but I doubt it would shave more than a few percent
         ;; off a draw-text benchmark.
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
  (or (getf (xlib:gcontext-plist gcontext) 'picture)
      (setf (getf (xlib:gcontext-plist gcontext) 'picture)
            (let ((pixmap (xlib:create-pixmap :drawable drawable
                           :depth (xlib:drawable-depth drawable)
                           :width 1 :height 1)))
              (list
               (xlib::render-create-picture
                pixmap
                :format (xlib::find-window-picture-format (xlib:drawable-root drawable))
                :repeat :on)
               pixmap)))))

(let ((buffer (make-array 1024 :element-type '(unsigned-byte 32) ; TODO: thread safety
                               :adjustable nil :fill-pointer nil)))
  (defmethod clim-clx::font-draw-glyphs ((font freetype-face) mirror gc x y string &key start end translate)
    (declare (optimize (speed 3)))
    (when (< (length buffer) (- end start))
      (setf buffer (make-array (* 256 (ceiling (- end start) 256))
                               :element-type '(unsigned-byte 32)
                               :adjustable nil :fill-pointer nil)))
    (let ((display (xlib:drawable-display mirror)))
      (destructuring-bind (source-picture source-pixmap) (gcontext-picture mirror gc)
        (let* ((fg (xlib:gcontext-foreground gc))
               (cache (slot-value font 'glyph-id-cache))
               (glyph-ids buffer))
          (loop
             for i from start below end ; TODO: Read optimization notes. Fix. Repeat.
             for i* upfrom 0
             as char = (aref string i)
             as code = (char-code char)
             do (setf (aref buffer i*)
                      (or (gcache-get cache code)
                          (gcache-set cache code (font-glyph-id font char)))))

          (xlib::render-fill-rectangle source-picture
                                       :src
                                       (list (ash (ldb (byte 8 16) fg) 8)
                                             (ash (ldb (byte 8 8) fg) 8)
                                             (ash (ldb (byte 8 0) fg) 8)
                                             #xFFFF)
                                       0 0 1 1)
          (setf (xlib::picture-clip-mask (drawable-picture mirror))
                (xlib::gcontext-clip-mask gc))
          (xlib::render-composite-glyphs
           (drawable-picture mirror)
           (display-the-glyph-set display)
           source-picture
           x y       
           glyph-ids
           :end (- end start)))))))

(let ((cache (make-hash-table :test #'equal)))
  (defun make-free-type-face (display font size)
    (or (gethash (list display font size) cache)
        (setf (gethash (list display font size) cache)
              (let* ((f.font (or (gethash font *font-hash*)
                                 (setf (gethash font *font-hash*)
                                       (make-vague-font font))))
                     (f (make-concrete-font f.font size)))
                (declare (type (alien freetype:face) f))
                (set-concrete-font-size f size *dpi*)
                (make-instance 'freetype-face
                               :display display
                               :font font
                               :matrix size
                               :concrete-font f
                               :ascent  (/ (slot (slot (slot f 'freetype:size_s) 'freetype:metrics) 'freetype:ascender) 64)
                               :descent (/ (slot (slot (slot f 'freetype:size_s) 'freetype:metrics) 'freetype:descender) -64)))))))

(defparameter *sizes*
  '(:normal 12
    :small 10
    :very-small 8
    :tiny 8
    :large 14
    :very-large 18
    :huge 24))

(defparameter *vera-families/faces*
  '(((:fix :roman) . "VeraMono.ttf")
    ((:fix :italic) . "VeraMoIt.ttf")
    ((:fix (:bold :italic)) . "VeraMoBI.ttf")
    ((:fix (:italic :bold)) . "VeraMoBI.ttf")
    ((:fix :bold) . "VeraMoBd.ttf")
    ((:serif :roman) . "VeraSe.ttf")
    ((:serif :italic) . "VeraSe.ttf")
    ((:serif (:bold :italic)) . "VeraSeBd.ttf")
    ((:serif (:italic :bold)) . "VeraSeBd.ttf")
    ((:serif :bold) . "VeraSeBd.ttf")
    ((:sans-serif :roman) . "Vera.ttf")
    ((:sans-serif :italic) . "VeraIt.ttf")
    ((:sans-serif (:bold :italic)) . "VeraBI.ttf")
    ((:sans-serif (:italic :bold)) . "VeraBI.ttf")
    ((:sans-serif :bold) . "VeraBd.ttf")))

;;; Here are alternate mappings for the DejaVu family of fonts, which
;;; are a derivative of Vera with improved unicode coverage.
(defparameter *dejavu-families/faces* 
  '(((:FIX :ROMAN) . "DejaVuSansMono.ttf") 
    ((:FIX :ITALIC) . "DejaVuSansMono-Oblique.ttf")
    ((:FIX (:BOLD :ITALIC)) . "DejaVuSansMono-BoldOblique.ttf")
    ((:FIX (:ITALIC :BOLD)) . "DejaVuSansMono-BoldOblique.ttf") 
    ((:FIX :BOLD) . "DejaVuSansMono-Bold.ttf")
    ((:SERIF :ROMAN) . "DejaVuSerif.ttf") 
    ((:SERIF :ITALIC) . "DejaVuSerif-Oblique.ttf")
    ((:SERIF (:BOLD :ITALIC)) . "DejaVuSerif-BoldOblique.ttf")
    ((:SERIF (:ITALIC :BOLD)) . "DejaVuSerif-BoldOblique.ttf") 
    ((:SERIF :BOLD) . "DejaVuSerif-Bold.ttf")
    ((:SANS-SERIF :ROMAN) . "DejaVuSans.ttf") 
    ((:SANS-SERIF :ITALIC) . "DejaVuSans-Oblique.ttf")
    ((:SANS-SERIF (:BOLD :ITALIC)) . "DejaVuSans-BoldOblique.ttf")
    ((:SANS-SERIF (:ITALIC :BOLD)) . "DejaVuSans-BoldOblique.ttf")
    ((:SANS-SERIF :BOLD) . "DejaVuSans-Bold.ttf")))

(defvar *families/faces* *vera-families/faces*)

(defparameter *freetype-font-path* #p"/usr/share/fonts/truetype/ttf-dejavu/")

(fmakunbound 'clim-clx::text-style-to-x-font)

(defstruct freetype-device-font-name
  (font-file (error "missing argument"))
  (size (error "missing argument")))

(defmethod clim-clx::text-style-to-X-font :around 
    ((port clim-clx::clx-port) (text-style climi::device-font-text-style))
  (let ((display (slot-value port 'clim-clx::display))
        (font-name (climi::device-font-name text-style)))
    (make-free-type-face display
                         (freetype-device-font-name-font-file font-name)
                         (freetype-device-font-name-size font-name))))

(defmethod text-style-mapping :around
    ((port clim-clx::clx-port) (text-style climi::device-font-text-style)
     &optional character-set)
  (values (gethash text-style (clim-clx::port-text-style-mappings port))))

(defmethod (setf text-style-mapping) :around
    (value 
     (port clim-clx::clx-port) 
     (text-style climi::device-font-text-style)
     &optional character-set)
  (setf (gethash text-style (clim-clx::port-text-style-mappings port)) value))

(defparameter *free-type-face-hash* (make-hash-table :test #'equal))

(define-condition missing-font (simple-error)
  ((filename :reader missing-font-filename :initarg :filename))
  (:report (lambda (condition stream)
             (format stream  "Cannot access ~W~%Your *freetype-font-path* is currently ~W~%The following files should exist:~&~{  ~A~^~%~}"
                     (missing-font-filename condition)
                     *freetype-font-path*
                     (mapcar #'cdr *families/faces*)))))

(defun invoke-with-freetype-path-restart (continuation)
  (restart-case (funcall continuation)
    (change-font-path (new-path)
      :report (lambda (stream) (format stream "Retry with alternate freetype font path"))
      :interactive (lambda ()
                     (format t "Enter new value: ")
                     (list (read-line)))
      (setf *freetype-font-path* new-path)
      (invoke-with-freetype-path-restart continuation))))

(let (lookaside)
  (defmethod clim-clx::text-style-to-X-font :around ((port clim-clx::clx-port) (text-style standard-text-style))
    (flet ((f ()
             (multiple-value-bind (family face size) 
                 (clim:text-style-components text-style)
            
               (let ((display (clim-clx::clx-port-display port)))
                 (setf face (or face :roman))
                 (setf family (or family :fix))
                 (setf size (or size :normal))
                 (cond (size
                        (setf size (getf *sizes* size size))
                        (let ((val (gethash (list display family face size) *free-type-face-hash*)))
                          (if val val
                              (setf (gethash (list display family face size) *free-type-face-hash*)
                                    (let* ((font-path-relative (cdr (assoc (list family face) *families/faces*
                                                                           :test #'equal)))
                                           (font-path (namestring (merge-pathnames font-path-relative *freetype-font-path*))))
                                      (unless (and font-path (probe-file font-path))
                                        (error 'missing-font :filename font-path))
                                      #+NIL
                                      (if (and font-path (probe-file font-path))
                                          (make-free-type-face display font-path size)
                                          (call-next-method))
                                      (make-free-type-face display font-path size))))))
                       (t
                        (call-next-method)))))))
      (cdr (if (eq (car lookaside) text-style)
               lookaside
               (setf lookaside (cons text-style (invoke-with-freetype-path-restart #'f))))))))

(defmethod clim-clx::text-style-to-X-font ((port clim-clx::clx-port) text-style)
  (error "You lost: ~S." text-style))

;;;;;;

(in-package :clim-clx)

(defmethod text-style-ascent (text-style (medium clx-medium))
  (let ((font (text-style-to-X-font (port medium) text-style)))
    (clim-clx::font-ascent font)))

(defmethod text-style-descent (text-style (medium clx-medium))
  (let ((font (text-style-to-X-font (port medium) text-style)))
    (clim-clx::font-descent font)))

(defmethod text-style-height (text-style (medium clx-medium))
  (let ((font (text-style-to-X-font (port medium) text-style)))
    (+ (clim-clx::font-ascent font) (clim-clx::font-descent font))))

(defmethod text-style-character-width (text-style (medium clx-medium) char)
  (clim-clx::font-glyph-width (text-style-to-X-font (port medium) text-style) char))

(defmethod text-style-width (text-style (medium clx-medium))
  (text-style-character-width text-style medium #\m))

(defmethod text-size ((medium clx-medium) string &key text-style (start 0) end)
  (when (characterp string)
    (setf string (make-string 1 :initial-element string)))
  (unless end (setf end (length string)))
  (unless text-style (setf text-style (medium-text-style medium)))
  (let ((xfont (text-style-to-X-font (port medium) text-style)))
    (cond ((= start end)
           (values 0 0 0 0 0))
          (t
           (let ((position-newline (position #\newline string :start start)))
             (cond ((not (null position-newline))
                    (multiple-value-bind (width ascent descent left right
                                                font-ascent font-descent direction
                                                first-not-done)
                        (font-text-extents xfont string
                                           :start start :end position-newline
                                           :translate #'translate)
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
                                   :translate #'translate)
                      (declare (ignorable left right
                                          font-ascent font-descent
                                          direction first-not-done))
                      (values width (+ ascent descent) width 0 ascent)) )))))) )

(defmethod climi::text-bounding-rectangle*
    ((medium clx-medium) string &key text-style (start 0) end)
  (when (characterp string)
    (setf string (make-string 1 :initial-element string)))
  (unless end (setf end (length string)))
  (unless text-style (setf text-style (medium-text-style medium)))
  (let ((xfont (text-style-to-X-font (port medium) text-style)))
    (cond ((= start end)
           (values 0 0 0 0))
          (t
           (let ((position-newline (position #\newline string :start start)))
             (cond ((not (null position-newline))
                    (multiple-value-bind (width ascent descent left right
                                                font-ascent font-descent direction
                                                first-not-done)
                        (font-text-extents xfont string
                                           :start start :end position-newline
                                           :translate #'translate)
                      (declare (ignorable left right
                                          font-ascent font-descent
                                          direction first-not-done))
                      (multiple-value-bind (minx miny maxx maxy)
                          (climi::text-bounding-rectangle*
                           medium string :text-style text-style
                           :start (1+ position-newline) :end end)
                        (values (min minx left) (- ascent)
                                (max maxx right) (+ descent maxy)))))
                   (t
                    (multiple-value-bind (width ascent descent left right
                                                font-ascent font-descent direction
                                                first-not-done)
                        (font-text-extents xfont string
                                   :start start :end end
                                   :translate #'translate)
                      (declare (ignore width direction first-not-done))
                      ;; FIXME: Potential style points:
                      ;; * (min 0 left), (max width right)
                      ;; * font-ascent / ascent
                      (values left (- font-ascent) right font-descent)))))))))


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

(defmethod medium-draw-text* ((medium clx-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (with-transformed-position ((sheet-native-transformation (medium-sheet medium))
                              x y)
    (with-clx-graphics (medium)
      (when (characterp string)
        (setq string (make-string 1 :initial-element string)))
      (when (null end) (setq end (length string)))
      (multiple-value-bind (text-width text-height x-cursor y-cursor baseline) 
          (text-size medium string :start start :end end)
        (declare (ignore x-cursor y-cursor))
        (unless (and (eq align-x :left) (eq align-y :baseline))	    
          (setq x (- x (ecase align-x
                         (:left 0)
                         (:center (round text-width 2))
                         (:right text-width))))
          (setq y (ecase align-y
                    (:top (+ y baseline))
                    (:center (+ y baseline (- (floor text-height 2))))
                    (:baseline y)
                    (:bottom (+ y baseline (- text-height)))))))
      (let ((x (round-coordinate x))
            (y (round-coordinate y)))
        (when (and (<= #x-8000 x #x7FFF)
                   (<= #x-8000 y #x7FFF))
          (multiple-value-bind (halt width)
              (font-draw-glyphs
               (text-style-to-X-font (port medium) (medium-text-style medium))
               mirror gc x y string
                                :start start :end end
                                :translate #'translate)))))))


(defmethod (setf medium-text-style) :before (text-style (medium clx-medium))
  (with-slots (gc) medium
    (when gc
      (let ((old-text-style (medium-text-style medium)))
	(unless (eq text-style old-text-style)
          (let ((fn (text-style-to-X-font (port medium) (medium-text-style medium))))
            (when (typep fn 'xlib:font)
              (setf (xlib:gcontext-font gc)
                    fn))))))))

;;;
;;; This fixes the worst offenders making the assumption that drawing
;;; would be idempotent.
;;;

(defmethod clim:handle-repaint :around ((s clim:sheet-with-medium-mixin) r)
  (let ((m (clim:sheet-medium s))
        (r (clim:bounding-rectangle
            (clim:region-intersection r (clim:sheet-region s)))))
    (unless (eql r clim:+nowhere+)
      (clim:with-drawing-options (m :clipping-region r)
        (clim:draw-design m r :ink clim:+background-ink+)
        (call-next-method s r)))))
