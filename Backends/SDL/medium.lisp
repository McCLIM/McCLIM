;;; -*- Mode: Lisp; Package: CLIM-SDL -*-

;;; (c) 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

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

(in-package :clim-sdl)

(defclass sdl-medium (basic-medium)
  ((buffering-output-p :accessor medium-buffering-output-p)
   (renderer      :initform nil
                  :accessor sdl-medium/renderer)
   (texture       :initform nil
                  :accessor sdl-medium/texture)
   (surface       :initform nil
                  :accessor sdl-medium/surface)
   (cairo-context :initform nil
                  :accessor sdl-medium/cairo-context)))

(defmacro with-medium-mirror ((mirror-sym medium) &body body)
  (alexandria:once-only (medium)
    (alexandria:with-gensyms (mirror-copy-sym)
      `(let ((,mirror-copy-sym (climi::port-lookup-mirror (port ,medium) (medium-sheet ,medium))))
         (when ,mirror-copy-sym
           (let ((,mirror-sym ,mirror-copy-sym))
             ,@body))))))

#+nil
(defun find-renderer (medium)
  (if (sdl-medium/renderer medium)
      (values (sdl-medium/renderer medium)
              (sdl-medium/texture medium)
              (sdl-medium/surface medium)
              (sdl-medium/cairo-context medium))
      ;; ELSE: Need to create a new renderer
      (let* ((renderer (sdl2:create-renderer (sheet-mirror (medium-sheet medium)) nil '(:accelerated)))
             (texture (sdl2:create-texture renderer :argb8888 :streaming 400 400)))
        (multiple-value-bind (pixels pitch)
            (sdl2:lock-texture texture)
          (let* ((surface (cairo:create-image-surface-for-data pixels :argb32
                                                               (sdl2:texture-width texture)
                                                               (sdl2:texture-height texture)
                                                               pitch))
                 (context (cairo:create-context surface)))
            (setf (sdl-medium/renderer medium) renderer)
            (setf (sdl-medium/texture medium) texture)
            (setf (sdl-medium/surface medium) surface)
            (setf (sdl-medium/cairo-context medium) context)
            (values renderer texture surface context))))))

(defun %make-renderer (medium)
  (if (sdl-medium/renderer medium)
      (values (sdl-medium/renderer medium) (sdl-medium/texture medium))
      ;; ELSE: We need to create the renderer and texture
      (progn
        (log:info "Creating renderer for mediu: ~s" medium)
        (let* ((renderer (sdl2:create-renderer (sheet-mirror (medium-sheet medium)) nil '(:accelerated)))
               (texture (sdl2:create-texture renderer :argb8888 :streaming 400 400)))
          (setf (sdl-medium/renderer medium) renderer)
          (setf (sdl-medium/texture medium) texture)
          (values renderer texture)))))

(defun find-context (medium)
  (or (sdl-medium/cairo-context medium)
      (multiple-value-bind (renderer texture)
          (%make-renderer medium)
        (declare (ignore renderer))
        (multiple-value-bind (pixels pitch)
            (sdl2:lock-texture texture)
          (let* ((surface (cairo:create-image-surface-for-data pixels :argb32
                                                               (sdl2:texture-width texture)
                                                               (sdl2:texture-height texture)
                                                               pitch))
                 (context (cairo:create-context surface)))
            (setf (sdl-medium/surface medium) surface)
            (setf (sdl-medium/cairo-context medium) context)
            context)))))

#+nil
(defun ensure-ubyte (n)
  (truncate (max (min n 255) 0)))

#+nil
(defun update-renderer-attrs (medium renderer)
  (multiple-value-bind (red green blue alpha)
      (clime::color-rgba (medium-ink medium))
    (sdl2:set-render-draw-color renderer
                                (ensure-ubyte (* red 255))
                                (ensure-ubyte (* green 255))
                                (ensure-ubyte (* blue 255))
                                (ensure-ubyte (* alpha 255)))))

#+nil
(defmacro with-medium-renderer ((medium &key mirror renderer) &body body)
  (alexandria:once-only (medium)
    (alexandria:with-gensyms (mirror-copy renderer-copy sheet ink)
      `(let* ((,sheet (medium-sheet ,medium))
              (,mirror-copy (climi::port-lookup-mirror (port ,medium) ,sheet)))
         (when ,mirror-copy
           (let ((,renderer-copy (find-renderer (sheet-mirrored-ancestor ,sheet))))
             (let ((,ink (medium-ink medium)))
               (unless (eq ,ink +transparent-ink+)
                 (update-renderer-attrs medium ,renderer-copy)
                 (let (,@(if mirror
                             `((,mirror ,mirror-copy))
                             nil)
                       ,@(if renderer
                             `((,renderer ,renderer-copy))))
                   ,@body)))))))))

(defmethod (setf medium-text-style) :before (text-style (medium sdl-medium))
  (declare (ignore text-style))
  nil)

(defmethod (setf medium-line-style) :before (line-style (medium sdl-medium))
  (declare (ignore line-style))
  (log:info "not implemented")
  nil)

(defmethod (setf medium-clipping-region) :after (region (medium sdl-medium))
  (declare (ignore region))
  (log:info "not implemented")
  nil)

(defmethod medium-copy-area ((from-drawable sdl-medium)
			     from-x from-y width height
                             (to-drawable sdl-medium)
			     to-x to-y)
  (declare (ignore from-x from-y width height to-x to-y))
  (log:info "not implemented")
  nil)

#+nil ; FIXME: PIXMAP class
(progn
  (defmethod medium-copy-area ((from-drawable sdl-medium)
			       from-x from-y width height
			       (to-drawable pixmap)
			       to-x to-y)
    (declare (ignore from-x from-y width height to-x to-y))
    nil)

  (defmethod medium-copy-area ((from-drawable pixmap)
			       from-x from-y width height
			       (to-drawable sdl-medium)
			       to-x to-y)
    (declare (ignore from-x from-y width height to-x to-y))
    nil)

  (defmethod medium-copy-area ((from-drawable pixmap)
			       from-x from-y width height
			       (to-drawable pixmap)
			       to-x to-y)
    (declare (ignore from-x from-y width height to-x to-y))
    nil))

(defmethod medium-draw-point* ((medium sdl-medium) x y)
  (declare (ignore x y))
  (log:info "not implemented")
  nil)

(defmethod medium-draw-points* ((medium sdl-medium) coord-seq)
  (declare (ignore coord-seq))
  (log:info "not implemented")
  nil)

(defun update-attrs (medium)
  (multiple-value-bind (red green blue)
      (clime::color-rgba (medium-ink medium))
    (cairo:set-source-rgb red green blue)
    (cairo:set-line-width (line-style-thickness (medium-line-style medium)))))

(defmacro with-cairo-context ((medium) &body body)
  (alexandria:once-only (medium)
    (alexandria:with-gensyms (context)
      `(sdl2:in-main-thread ()
         (let ((,context (find-context ,medium)))
           (cairo:with-context (,context)
             (update-attrs ,medium)
             (progn ,@body)))))))

(defmethod medium-draw-line* ((medium sdl-medium) x1 y1 x2 y2)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (climi::with-transformed-position (tr x1 y1)
      (climi::with-transformed-position (tr x2 y2)
        (let ((x1 (round-coordinate x1))
              (y1 (round-coordinate y1))
              (x2 (round-coordinate x2))
              (y2 (round-coordinate y2)))
          (sdl2:in-main-thread ()
            (let ((context (find-context medium)))
              (cairo:with-context (context)
                (update-attrs medium)
                (cairo:move-to x1 y1)
                (cairo:line-to x2 y2)
                (cairo:stroke)))))))))

;; FIXME: Invert the transformation and apply it here, as the :around
;; methods on transform-coordinates-mixin will cause it to be applied
;; twice, and we need to undo one of those. The
;; transform-coordinates-mixin stuff needs to be eliminated.
(defmethod medium-draw-lines* ((medium sdl-medium) coord-seq)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (declare (ignore tr))
    (log:info "not implemented")
    nil))

(defmethod medium-draw-polygon* ((medium sdl-medium) coord-seq closed filled)
  (declare (ignore coord-seq closed filled))
  nil)

(defmethod medium-draw-rectangle* ((medium sdl-medium) left top right bottom filled)
  (log:info "Draw rect: medium=~s (~s,~s)-(~s,~s) filled=~s" medium left top right bottom filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (climi::with-transformed-position (tr left top)
      (climi::with-transformed-position (tr right bottom)
        (when (< right left) (rotatef left right))
        (when (< bottom top) (rotatef top bottom))
        (let ((left   (round-coordinate left))
              (top    (round-coordinate top))
              (right  (round-coordinate right))
              (bottom (round-coordinate bottom)))
          (with-cairo-context (medium)
            (when filled
              (cairo:fill-path))
            (cairo:rectangle left top (- right left) (- bottom top)))
          #+nil
          (with-medium-renderer (medium :renderer renderer)
            (let ((rect (sdl2:make-rect left top (- right left) (- bottom top))))
              (if filled
                  (sdl2:render-fill-rect renderer rect)
                  (sdl2:render-draw-rect renderer rect)))))))))

(defmethod medium-draw-rectangles* ((medium sdl-medium) position-seq filled)
  (declare (ignore position-seq filled))
  (log:info "not implemented")
  nil)

(defmethod medium-draw-ellipse* ((medium sdl-medium) center-x center-y
				 radius-1-dx radius-1-dy
				 radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (declare (ignore center-x center-y
		   radius-1-dx radius-1-dy
		   radius-2-dx radius-2-dy
		   start-angle end-angle filled))
  nil)

(defmethod medium-draw-circle* ((medium sdl-medium)
				center-x center-y radius start-angle end-angle
				filled)
  (declare (ignore center-x center-y radius
		   start-angle end-angle filled))
  nil)

(defmethod text-style-ascent (text-style (medium sdl-medium))
  (declare (ignore text-style))
  1)

(defmethod text-style-descent (text-style (medium sdl-medium))
  (declare (ignore text-style))
  1)

(defmethod text-style-height (text-style (medium sdl-medium))
  (+ (text-style-ascent text-style medium)
     (text-style-descent text-style medium)))

(defmethod text-style-character-width (text-style (medium sdl-medium) char)
  (declare (ignore text-style char))
  1)

(defmethod text-style-width (text-style (medium sdl-medium))
  (text-style-character-width text-style medium #\m))

(defmethod text-size ((medium sdl-medium) string &key text-style (start 0) end)
  (setf string (etypecase string
		 (character (string string))
		 (string string)))
  (let ((width 0)
	(height (text-style-height text-style medium))
	(x (- (or end (length string)) start))
	(y 0)
	(baseline (text-style-ascent text-style medium)))
    (do ((pos (position #\Newline string :start start :end end)
	      (position #\Newline string :start (1+ pos) :end end)))
	((null pos) (values width height x y baseline))
      (let ((start start)
	    (end pos))
	(setf x (- end start))
	(setf y (+ y (text-style-height text-style medium)))
	(setf width (max width x))
	(setf height (+ height (text-style-height text-style medium)))
	(setf baseline (+ baseline (text-style-height text-style medium)))))))

(defmethod climb:text-bounding-rectangle* ((medium sdl-medium) string
                                           &key text-style (start 0) end align-x align-y direction)
  (declare (ignore align-x align-y direction)) ; implement me!
  (multiple-value-bind (width height x y baseline)
      (text-size medium string :text-style text-style :start start :end end)
    (declare (ignore baseline))
    (values x y (+ x width) (+ y height))))

(defmethod medium-draw-text* ((medium sdl-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore string x y start end align-x align-y toward-x toward-y transform-glyphs))
  nil)

(defmethod medium-buffering-output-p ((medium sdl-medium))
  t)

(defmethod (setf medium-buffering-output-p) (buffer-p (medium sdl-medium))
  buffer-p)

(defmethod medium-finish-output ((medium sdl-medium))
  (let* ((sheet (medium-sheet medium))
         (mirror (climi::port-lookup-mirror (port medium) sheet)))
    (log:info "Finishing output: ~s" medium)
    (when mirror
      (let ((renderer (sdl-medium/renderer medium)))
        (when renderer
          (sdl2:in-main-thread ()
            (let ((texture (sdl-medium/texture medium)))
              (sdl2:unlock-texture texture)
              (sdl2:render-copy renderer texture)
              (sdl2:render-present renderer)
              (setf (sdl-medium/surface medium) nil)
              (setf (sdl-medium/cairo-context medium) nil))))))))

(defmethod medium-force-output ((medium sdl-medium))
  (log:info "Forcing output: ~s" medium))

(defmethod medium-clear-area ((medium sdl-medium) left top right bottom)
  (declare (ignore left top right bottom))
  nil
  #+nil
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr left top)
      (with-transformed-position (tr right bottom)
        (let ((min-x (round-coordinate (min left right)))
              (min-y (round-coordinate (min top bottom)))
              (max-x (round-coordinate (max left right)))
              (max-y (round-coordinate (max top bottom))))
          
          (xlib:draw-rectangle (port-lookup-mirror (port medium)
                                                   (medium-sheet medium))
                               (medium-gcontext medium (medium-background medium))
                               (clamp min-x           #x-8000 #x7fff)
                               (clamp min-y           #x-8000 #x7fff)
                               (clamp (- max-x min-x) 0       #xffff)
                               (clamp (- max-y min-y) 0       #xffff)
                               t))))))

(defmethod medium-beep ((medium sdl-medium))
  nil)

(defmethod medium-miter-limit ((medium sdl-medium))
  0)

