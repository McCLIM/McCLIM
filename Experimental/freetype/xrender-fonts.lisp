;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MCCLIM-TRUETYPE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Font matrics, caching, and XRender text support 
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

(in-package :mcclim-truetype)

(declaim (optimize (speed 1) (safety 3) (debug 1) (space 0)))

(defparameter *dpi* 72)

;;;; Notes

;; You might need to tweak mcclim-truetype::*families/faces* to point
;; to where ever there are suitable TTF fonts on your system.

;; FIXME: I don't think draw-text* works for strings spanning multiple lines.
;; FIXME: Not particularly thread safe.

;; Some day, it might become useful to decouple the font representation
;; from the xrender details. 

(defclass vague-font ()
  ((lib :initarg :lib)
   (filename :initarg :filename)))

(defparameter *vague-font-hash* (make-hash-table :test #'equal))

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

(defstruct (glyph-info (:constructor glyph-info (id width height left right top)))
  id                                    ; FIXME: Types?
  width height
  left right top)

;;;;;;; mcclim interface
(defclass truetype-face ()
  ((display       :initarg :display       :reader truetype-face-display)
   (filename      :initarg :filename      :reader truetype-face-filename)
   (size          :initarg :size          :reader truetype-face-size)
   (ascent        :initarg :ascent        :reader truetype-face-ascent)
   (descent       :initarg :descent       :reader truetype-face-descent)
   (fixed-width       :initform nil)
   (glyph-id-cache    :initform (make-gcache))
   (glyph-width-cache :initform (make-gcache))
   (char->glyph-info  :initform (make-hash-table :size 256))))

(defun font-generate-glyph (font glyph-index)
  (let* ((display (truetype-face-display font))
         (glyph-id (display-draw-glyph-id display)))
    (multiple-value-bind (arr left top dx dy) (glyph-pixarray font (code-char glyph-index))
      (with-slots (fixed-width) font
        (when (and (numberp fixed-width)
                   (/= fixed-width dx))
          (setf fixed-width t)
          (#-hef warn #+hef cerror #+hef "Ignore it." "Font ~A is fixed width, but the glyph width appears to vary.
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

(defmethod print-object ((object truetype-face) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-slots (filename size ascent descent) object
      (format stream "~A size=~A ~A/~A" filename size ascent descent))))

(defun font-glyph-info (font character)
  (with-slots (char->glyph-info) font
    (or (gethash character char->glyph-info)
        (setf (gethash character char->glyph-info) 
              (font-generate-glyph font (char-code character))))))

(defun font-glyph-id (font character)
  (glyph-info-id (font-glyph-info font character)))

(defmethod clim-clx::font-ascent ((font truetype-face))
  (truetype-face-ascent font))

(defmethod clim-clx::font-descent ((font truetype-face))
  (truetype-face-descent font))

(defmethod clim-clx::font-glyph-width ((font truetype-face) char)
  (glyph-info-width (font-glyph-info font char)))

(defmethod clim-clx::font-glyph-left ((font truetype-face) char)
  (glyph-info-left (font-glyph-info font char)))

(defmethod clim-clx::font-glyph-right ((font truetype-face) char)
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

(defmethod clim-clx::font-text-extents ((font truetype-face) string
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
  (defun clim-clx::font-draw-glyphs (font #|(font truetype-face)|# mirror gc x y string
                                     #|x0 y0 x1 y1|# &key start end translate)
    (declare (optimize (speed 3))
             (type #-sbcl (integer 0 #.array-dimension-limit)
                   #+sbcl sb-int:index
                   start end)
             (type string string))
    (when (< (length buffer) (- end start))
      (setf buffer (make-array (* 256 (ceiling (- end start) 256))
                               :element-type '(unsigned-byte 16)
                               :adjustable nil :fill-pointer nil)))
    (let ((display (xlib:drawable-display mirror)))
      (destructuring-bind (source-picture source-pixmap) (gcontext-picture mirror gc)
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
           x y
           glyph-ids
           :end (- end start)))))))



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

(defparameter *truetype-font-path* #p"/usr/share/fonts/truetype/ttf-dejavu/")

(fmakunbound 'clim-clx::text-style-to-x-font)

(defstruct truetype-device-font-name
  (font-file (error "missing argument"))
  (size      (error "missing argument")))

(defstruct fontconfig-font-name
  (string (error "missing argument"))
  (size   (error "missing argument"))
  (options nil)
  (device-name nil))

(defmethod clim-clx::text-style-to-X-font :around
    ((port clim-clx::clx-port) (text-style climi::device-font-text-style))
  (let ((display (slot-value port 'clim-clx::display))
        (font-name (climi::device-font-name text-style)))
    (typecase font-name
      (truetype-device-font-name
       (make-truetype-face display
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

(defparameter *display-face-hash* (make-hash-table :test #'equal))

(define-condition missing-font (simple-error)
  ((filename :reader missing-font-filename :initarg :filename))
  (:report (lambda (condition stream)
             (format stream  "Cannot access ~W~%Your *truetype-font-path* is currently ~W~%The following files should exist:~&~{  ~A~^~%~}"
                     (missing-font-filename condition)
                     *truetype-font-path*
                     (mapcar #'cdr *families/faces*)))))

(defun invoke-with-truetype-path-restart (continuation)
  (restart-case (funcall continuation)
    (change-font-path (new-path)
      :report (lambda (stream) (format stream "Retry with alternate truetype font path"))
      :interactive (lambda ()
                     (format t "Enter new value: ")
                     (list (read-line)))
      (setf *truetype-font-path* new-path)
      (invoke-with-truetype-path-restart continuation))))

(let (lookaside)
  (defmethod clim-clx::text-style-to-X-font :around ((port clim-clx::clx-port) (text-style standard-text-style))
    (flet ((f ()
             (multiple-value-bind (family face size) 
                 (clim:text-style-components text-style)
            
               (let ((display (clim-clx::clx-port-display port)))
                 (setf face (or face :roman))
                 (setf family (or family :fix))
                 (setf size (or size :normal))
                 (when (eq family :fixed) (setf family :fix))                   
                 (cond (size
                        (setf size (getf *sizes* size size))
                        (let ((val (gethash (list display family face size) *display-face-hash*)))
                          (if val val
                              (setf (gethash (list display family face size) *display-face-hash*)
                                    (let* ((font-path-relative (cdr (assoc (list family face) *families/faces*
                                                                           :test #'equal)))
                                           (font-path (namestring (merge-pathnames font-path-relative 
                                                                                   *truetype-font-path*))))
                                      (unless (and font-path (probe-file font-path))
                                        (error 'missing-font :filename font-path))                                      
                                      (make-truetype-face display font-path size))))))
                       (t (call-next-method)))))))
      (cdr (if (eq (car lookaside) text-style)
               lookaside
               (setf lookaside (cons text-style (invoke-with-truetype-path-restart #'f))))))))

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
  (declare (optimize (speed 3)))
  (when (characterp string)
    (setf string (make-string 1 :initial-element string)))
  (check-type string string)
  (unless end (setf end (length string)))
  (check-type start
              #-sbcl (integer 0 #.array-dimension-limit)
              #+sbcl sb-int:index)
  (check-type end
              #-sbcl (integer 0 #.array-dimension-limit)
              #+sbcl sb-int:index)
  (unless text-style (setf text-style (medium-text-style medium)))
  (let ((xfont (text-style-to-X-font (port medium) text-style)))
    (cond ((= start end)
           (values 0 0 0 0 0))
          (t
           (let ((position-newline 
                  (macrolet ((p (type)
                               `(locally 
                                 (declare (type ,type string))
                                 (position #\newline string :start start))))
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
                    (:bottom (+ y baseline (- text-height))))))

        (let ((x (round-coordinate x))
              (y (round-coordinate y)))
          (when (and (<= #x-8000 x #x7FFF)
                     (<= #x-8000 y #x7FFF))
            (font-draw-glyphs
             (text-style-to-X-font (port medium) (medium-text-style medium))
             mirror gc x y string
             #| x (- y baseline) (+ x text-width) (+ y (- text-height baseline )) |#
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
        ;; This causes applications which want to do a double-buffered repaint,
        ;; such as the logic cube, to flicker. On the other hand, it also
        ;; stops things such as the listener wholine from overexposing their
        ;; text. Who is responsible for clearing to the background color before
        ;; repainting?
        ;(clim:draw-design m r :ink clim:+background-ink+)
        (call-next-method s r)
        ;; FIXME: Shouldn't McCLIM always do this?
        (medium-force-output (sheet-medium s))))))

