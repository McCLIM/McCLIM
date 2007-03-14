;;; -*- Mode: Lisp; Package: CLIM-GRAPHIC-FORMS -*-

;;; (c) 2006 Jack D. Unrue (jdunrue (at) gmail (dot) com)
;;; based on the null backend by:
;;;  (c) 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

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

(in-package :clim-graphic-forms)

(defclass graphic-forms-medium (basic-medium)
  ((font
    :accessor font-of
    :initform nil)
   (image
    :accessor image-of
    :initform nil)
   (port
    :accessor port-of
    :initarg :port
    :initform nil)))

(defvar *medium-origin*     (gfs:make-point))
(defvar *mediums-to-render* nil)

(defun add-medium-to-render (medium)
  (pushnew medium *mediums-to-render* :test #'eql))

(defun remove-medium-to-render (medium)
  (setf *mediums-to-render* (remove medium *mediums-to-render*)))

(defun render-medium (medium)
  (let ((mirror (climi::port-lookup-mirror (port-of medium) (medium-sheet medium))))
    (gfw:with-graphics-context (gc mirror)
      (gfg:draw-image gc (image-of medium) *medium-origin*))))

(defun render-pending-mediums ()
  (loop for medium in *mediums-to-render*
        do (render-medium medium))
  (setf *mediums-to-render* nil))

(defun resize-medium-buffer (medium size)
  (let ((old-image (image-of medium)))
    (when old-image
      (if (not (gfs:disposed-p old-image))
        (let ((old-size (gfg:size old-image)))
          (unless (gfs:equal-size-p size old-size)
            (gfs:dispose old-image)
            (setf old-image nil)))
        (setf old-image nil)))
    (unless old-image
      (setf (image-of medium) (make-instance 'gfg:image :size size)))))

(defun destroy-medium (medium)
  (remove-medium-to-render medium)
  (let ((image (image-of medium)))
    (if (and image (not (gfs:disposed-p image)))
      (gfs:dispose image)))
  (let ((font (font-of medium)))
    (if (and font (not (gfs:disposed-p font)))
      (gfs:dispose font))
    (setf (font-of medium) nil)))

(defun normalize-text-data (text)
  (etypecase text
    (string    text)
    (character (string text))
    (symbol    (symbol-name text))))

(defun sync-text-style (medium text-style)
  (multiple-value-bind (family face size)
      (text-style-components (merge-text-styles text-style *default-text-style*))
    #+nil (gfs::debug-format "family: ~a  face: ~a  size: ~a~%" family face size)
    ;;
    ;; FIXME: what to do about font data char sets?
    ;;
    ;; FIXME: externalize these specific choices so that applications can
    ;; have better control over them
    ;;
    (gfw:with-graphics-context (gc (climi::port-lookup-mirror (port-of medium) (medium-sheet medium)))
      (let ((old-data (if (font-of medium) (gfg:data-object (font-of medium) gc)))
            (face-name (case family
                         ((:fix :fixed) "Lucida Console")
                         (:serif        "Times New Roman")
                         (:sansserif    "Arial")))
            (pnt-size (case size
                        (:tiny       6)
                        (:very-small 8)
                        (:small      10)
                        (:normal     12)
                        (:large      14)
                        (:very-large 16)
                        (:huge       18)
                        (otherwise   10)))
            (style nil))
        (pushnew (case face
                   ((:bold :bold-italic :bold-oblique :italic-bold :oblique-bold)
                     :bold)
                   (otherwise
                     :normal))
                 style)
        (pushnew (case face
                   ((:bold-italic :italic :italic-bold)
                     :italic)
                   (otherwise
                     :normal))
                 style)
        (pushnew (case family
                   ((:fix :fixed) :fixed)
                   (otherwise     :normal))
                 style)
        (when (or (null old-data)
                  (not (eql pnt-size (gfg:font-data-point-size old-data)))
                  (string-not-equal face-name (gfg:font-data-face-name old-data))
                  (/= (length style)
                      (length (intersection style (gfg:font-data-style old-data)))))
          (when old-data
            (gfs:dispose (font-of medium))
            (setf (font-of medium) nil))
          (let ((new-data (gfg:make-font-data :face-name face-name
                                              :point-size pnt-size
                                              :style style)))
            #+nil (gfs::debug-format "new font data: ~a~%" new-data)
            (setf (font-of medium) (make-instance 'gfg:font :gc gc :data new-data))))))))

(defmethod (setf medium-text-style) :before (text-style (medium graphic-forms-medium))
  (sync-text-style medium
                   (merge-text-styles (medium-text-style medium)
                                      (medium-default-text-style medium))))

(defmethod (setf medium-line-style) :before (line-style (medium graphic-forms-medium))
  ())

(defmethod medium-copy-area ((from-drawable graphic-forms-medium)
			     from-x from-y width height
                             (to-drawable graphic-forms-medium)
			     to-x to-y)
  nil)

#+nil ; FIXME: PIXMAP class
(progn
  (defmethod medium-copy-area ((from-drawable graphic-forms-medium)
			       from-x from-y width height
			       (to-drawable pixmap)
			       to-x to-y)
    nil)
  (defmethod medium-copy-area ((from-drawable pixmap)
			       from-x from-y width height
			       (to-drawable graphic-forms-medium)
			       to-x to-y)
    ())
  (defmethod medium-copy-area ((from-drawable pixmap)
			       from-x from-y width height
			       (to-drawable pixmap)
			       to-x to-y)
    ()))

(defmethod medium-draw-point* ((medium graphic-forms-medium) x y)
  ())

(defmethod medium-draw-points* ((medium graphic-forms-medium) coord-seq)
  ())

(defmethod medium-draw-line* ((medium graphic-forms-medium) x1 y1 x2 y2)
  ())

;; FIXME: Invert the transformation and apply it here, as the :around
;; methods on transform-coordinates-mixin will cause it to be applied
;; twice, and we need to undo one of those. The
;; transform-coordinates-mixin stuff needs to be eliminated.
(defmethod medium-draw-lines* ((medium graphic-forms-medium) coord-seq)
  (let ((tr (invert-transformation (medium-transformation medium))))
    (declare (ignore tr))
    nil))

(defmethod medium-draw-polygon* ((medium graphic-forms-medium) coord-seq closed filled)
  #+nil (gfs::debug-format "draw-polygon ~a ~a ~a~%" coord-seq closed filled)
  (when (image-of medium)
    (gfw:with-graphics-context (gc (image-of medium))
      (setf (gfg:background-color gc) gfg:*color-white*
            (gfg:foreground-color gc) gfg:*color-black*)
      (let ((points-list (coordinates->points coord-seq)))
        (if filled
          (gfg:draw-filled-polygon gc points-list)
          (gfg:draw-polygon gc points-list))))
    (add-medium-to-render medium)))

(defmethod medium-draw-rectangle* ((medium graphic-forms-medium) left top right bottom filled)
  #+nil (gfs::debug-format "draw-rectangle ~a ~a ~a ~a ~a~%" left top right bottom filled)
  (when (image-of medium)
    (gfw:with-graphics-context (gc (image-of medium))
      (setf (gfg:background-color gc) gfg:*color-white*
            (gfg:foreground-color gc) gfg:*color-black*)
      (let ((rect (coordinates->rectangle left top right bottom)))
        (if filled
          (gfg:draw-filled-rectangle gc rect)
          (gfg:draw-rectangle gc rect))))
    (add-medium-to-render medium)))

(defmethod medium-draw-rectangles* ((medium graphic-forms-medium) position-seq filled)
  ())

(defmethod medium-draw-ellipse* ((medium graphic-forms-medium) center-x center-y
				 radius-1-dx radius-1-dy
				 radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  ())

(defmethod medium-draw-circle* ((medium graphic-forms-medium)
				center-x center-y radius start-angle end-angle
				filled)
  ())

(defmethod text-style-ascent (text-style (medium graphic-forms-medium))
  (let ((font (font-of medium)))
    (if font
      (gfw:with-graphics-context (gc (image-of medium))
        (gfg:ascent (gfg:metrics gc font)))
      1)))

(defmethod text-style-descent (text-style (medium graphic-forms-medium))
  (let ((font (font-of medium)))
    (if font
      (gfw:with-graphics-context (gc (image-of medium))
        (gfg:descent (gfg:metrics gc font)))
      1)))

(defmethod text-style-height (text-style (medium graphic-forms-medium))
  (let ((font (font-of medium)))
    (if font
      (gfw:with-graphics-context (gc (image-of medium))
        (gfg:height (gfg:metrics gc font)))
      1)))

(defmethod text-style-character-width (text-style (medium graphic-forms-medium) char)
  (let ((font (font-of medium))
        (width 1)
        (text (normalize-text-data char)))
    (if font
      (gfw:with-graphics-context (gc (image-of medium))
        (setf (gfg:font gc) font)
        (setf width (gfs:size-width (gfg:text-extent gc text)))))
    width))

(defmethod text-style-width (text-style (medium graphic-forms-medium))
  (let ((font (font-of medium)))
    (if font
      (gfw:with-graphics-context (gc (image-of medium))
        (gfg:average-char-width (gfg:metrics gc font)))
      1)))

(defmethod text-size ((medium graphic-forms-medium) string &key text-style (start 0) end)
  (setf string (normalize-text-data string))
#|
  (setf text-style (merge-text-styles (or text-style (make-text-style nil nil nil))
                                      (medium-default-text-style medium)))
|#
  ;; FIXME: handle embedded newlines
  ;;
  (let ((font (font-of medium)))
    (if font
      (gfw:with-graphics-context (gc (image-of medium))
        (let ((metrics (gfg:metrics gc font))
              (width (gfs:size-width (gfg:text-extent gc (subseq string
                                                                 start
                                                                 (or end (length string)))))))
          (values width
                  (gfg:height metrics)
                  width
                  (gfg:height metrics)
                  (gfg:ascent metrics))))
      (values 1 1 1 1 1))))

(defmethod climi::text-bounding-rectangle*
    ((medium graphic-forms-medium) string &key text-style (start 0) end)
  (text-size medium string :text-style text-style :start start :end end))

(defmethod medium-draw-text* ((medium graphic-forms-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  #+nil (gfs::debug-format "medium-draw-text: ~d, ~d  ~s~%" x y string)
  (when (image-of medium)
    (setf string (normalize-text-data string))
    (gfw:with-graphics-context (gc (image-of medium))
      (let ((font (font-of medium)))
        (if font
          (setf (gfg:font gc) font))
        (gfg:draw-text gc
                       (subseq string start (or end (length string)))
                       (gfs:make-point :x x :y y))))
    (add-medium-to-render medium)))

(defmethod medium-buffering-output-p ((medium graphic-forms-medium))
  t)

(defmethod (setf medium-buffering-output-p) (buffer-p (medium graphic-forms-medium))
  buffer-p)

(defmethod medium-draw-glyph ((medium graphic-forms-medium) element x y
			      align-x align-y toward-x toward-y
			      transform-glyphs)
  ())

(defmethod medium-finish-output ((medium graphic-forms-medium))
  (render-medium medium))

(defmethod medium-force-output ((medium graphic-forms-medium))
  (render-medium medium))

(defmethod medium-clear-area ((medium graphic-forms-medium) left top right bottom)
  (when (image-of medium)
    (let ((rect (coordinates->rectangle left top right bottom)))
      (gfw:with-graphics-context (gc (image-of medium))
        (setf (gfg:background-color gc) gfg:*color-white*
              (gfg:foreground-color gc) gfg:*color-white*)
        (gfg:draw-filled-rectangle gc rect)))
    (add-medium-to-render medium)))

(defmethod medium-beep ((medium graphic-forms-medium))
  ())

(defmethod invoke-with-special-choices (continuation (medium graphic-forms-medium))
  (let ((sheet (medium-sheet medium)))
    (funcall continuation (sheet-medium sheet))))

(defmethod medium-miter-limit ((medium graphic-forms-medium))
  0)
