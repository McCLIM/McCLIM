;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)
;;;           Gilbert Baumann (unk6@rz.uni-karlsruhe.de)


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

;;; TODO:
;;;
;;; - clipping
;;; - - more regions to draw
;;; - (?) blending
;;; - check MEDIUM-DRAW-TEXT*
;;; - POSTSCRIPT-ACTUALIZE-GRAPHICS-STATE: fix CLIPPING-REGION reusing logic
;;; - MEDIUM-DRAW-... should not duplicate code from POSTSCRIPT-ADD-PATH
;;; - structure this file
;;; - implement functions dealing with text and move them into another file

;;; Also missing IMO:
;;;
;;; - device fonts are missing
;;;
;;;--GB

(in-package :CLIM-POSTSCRIPT)

(defvar *transformation* nil
  "Native transformation")

;;; Postscript output utilities
(defmacro with-graphics-state ((medium) &body body)
  `(invoke-with-graphics-state ,medium
    (lambda () ,@body)))

(defun postscript-save-graphics-state (medium)
  (push (copy-alist (postscript-medium-graphics-state medium))
        (slot-value medium 'graphics-state-stack))
  (format (postscript-medium-file-stream medium) "gsave~%"))

(defun postscript-restore-graphics-state (medium)
  (pop (slot-value medium 'graphics-state-stack))
  (format (postscript-medium-file-stream medium) "grestore~%"))

(defun invoke-with-graphics-state (medium continuation)
  (postscript-save-graphics-state medium)
  (funcall continuation)
  (postscript-restore-graphics-state medium))

(defun format-postscript-number (number)
  (if (not (integerp number))
      (coerce number 'single-float)
      number))

(defun format-postscript-angle (angle)
  (format-postscript-number (* angle (/ 180 pi))))

(defun write-coordinates (stream x y)
  (with-transformed-position (*transformation* x y)
    (format stream "~A ~A "
            (format-postscript-number x)
            (format-postscript-number y))))

(defun write-transformation* (stream mxx mxy myx myy tx ty)
  (format stream "[~{~A~^ ~}]"
          (loop for c in (list mxx mxy myx myy tx ty)
             collect (format-postscript-number c))))

(defun moveto (stream x y)
  (write-coordinates stream x y)
  (format stream "moveto~%"))

(defun lineto (stream x y)
  (write-coordinates stream x y)
  (format stream "lineto~%"))

(defun postscript-set-transformation (stream transformation)
  (multiple-value-bind (mxx mxy myx myy tx ty) (get-transformation transformation)
    (write-transformation* stream mxx mxy myx myy tx ty)))


;;; Postscript path functions

(defgeneric postscript-add-path (stream region)
  (:documentation
   "Adds REGION (if it is a path) or its boundary (if it is an area)
   to the current path of STREAM."))

(defmethod postscript-add-path (stream (region (eql +nowhere+)))
  (declare (ignore stream)))

(defmethod postscript-add-path (stream (region standard-region-union))
  (map-over-region-set-regions (lambda (region)
                                 (postscript-add-path stream region))
                               region))

(defmethod postscript-add-path (stream (region standard-region-intersection))
  (format stream "gsave~%")
  #+nil (format stream "initclip~%")
  (loop for subregion in (region-set-regions region)
        do (format stream "newpath~%")
        (postscript-add-path stream subregion)
        (format stream "clip~%"))
  (format stream "clippath false upath~%")
  (format stream "grestore~%")
  (format stream "uappend~%"))

;;; Primitive paths
(defmethod postscript-add-path (stream (polygon polygon))
  (let ((points (polygon-points polygon)))
    (moveto stream (point-x (first points)) (point-y (first points)))
    (loop for point in (rest points)
          do (lineto stream (point-x point) (point-y point)))
    (format stream "closepath~%")))

(defmethod postscript-add-path (stream (ellipse ellipse))
  (let ((ellipse (transform-region *transformation* ellipse)))
    (multiple-value-bind (ndx1 ndy1 ndx2 ndy2) (ellipse-normal-radii* ellipse)
      (let* ((center (ellipse-center-point ellipse))
             (cx (point-x center))
             (cy (point-y center))
             (tr (make-transformation ndx1 ndy1 ndx2 ndy2 cx cy))
             (circle (untransform-region tr ellipse))
             (start-angle (ellipse-start-angle circle))
             (end-angle (ellipse-end-angle circle)))
        (format stream "matrix currentmatrix~%")
        (write-transformation* stream ndx1 ndy1 ndx2 ndy2 cx cy)
        (format stream "concat~%")
        (format stream "0 0 1 ~A ~A arc~%"
                (format-postscript-angle start-angle)
                (format-postscript-angle (if (< end-angle start-angle)
                                             (+ end-angle (* 2 pi))
                                             end-angle)))
        ;; (when filled ; uncomment when this code will be shared
        ;;              ; with ...-ELLIPTICAL-ARC
        (format stream "0 0 lineto~%")
        ;;)
        (format stream "closepath~%")
        (format stream "setmatrix~%")))))


;;; Graphics state

(defgeneric postscript-set-graphics-state (stream medium kind))

(defvar *postscript-graphics-states*
  '((:line-style . medium-line-style)
    (:color . medium-ink)
    (:clipping-region . medium-clipping-region)
    (:text-style . medium-text-style)))

(defun postscript-current-state (medium kind)
  (funcall (cdr (assoc kind *postscript-graphics-states*))
           medium))

(defmacro postscript-saved-state (medium kind)
  `(getf (postscript-medium-graphics-state ,medium)
       ,kind))

(defun postscript-actualize-graphics-state (stream medium &rest kinds)
  "Sets graphics parameters named in STATES."
  (loop for kind in (cons :clipping-region kinds)
        ;; every drawing function depends on clipping region
        ;;
        ;; KLUDGE: clipping-region MUST be actualized first due to its
        ;; dirty dealing with graphics state. -- APD, 2002-02-11
        unless (eql (postscript-current-state medium kind)
                    (postscript-saved-state medium kind))
        do (postscript-set-graphics-state stream medium kind)
           (setf (postscript-saved-state medium kind)
                 (postscript-current-state medium kind))))

;;; Line style
(defconstant +postscript-line-joints+ '(:miter 0
                                        :round 1
                                        :bevel 2
                                        :none 0))

(defconstant +postscript-line-caps+ '(:butt 0
                                      :round 1
                                      :square 2 ; extended butt caps
                                      :no-end-point 0))

(defconstant +postscript-default-line-dashes+ '(30 30))

(defconstant +normal-line-width+ (/ 2.0 3.0))

(defun medium-line-scale (medium)
  (let* ((line-style (medium-line-style medium))
         (unit (line-style-unit line-style)))
    (ecase unit
      (:normal +normal-line-width+)
      (:point 1)
      (:coordinate (error ":COORDINATE line unit not implemented.")))))

(defun medium-line-thickness (medium)
  (* (medium-line-scale medium)
     (line-style-thickness (medium-line-style medium))))

(defmethod postscript-set-graphics-state (stream medium
                                          (kind (eql :line-style)))
  (let* ((line-style (medium-line-style medium))
         (scale (medium-line-scale medium)))
    (format stream "~D setlinewidth ~A setlinejoin ~A setlinecap~%"
            (format-postscript-number
             (* scale (line-style-thickness line-style)))
            (getf +postscript-line-joints+
                  (line-style-joint-shape line-style))
            (getf +postscript-line-caps+
                  (line-style-cap-shape line-style)))
    (let ((dashes (line-style-dashes line-style)))
      (format stream "[")
      (mapc (lambda (l) (format stream "~D " (format-postscript-number
                                              (* scale l))))
            (if (eq dashes 't)
                +postscript-default-line-dashes+
                dashes))
      (format stream "] 0 setdash~%"))))

;;; Color
(defmethod medium-color-rgb (medium (ink (eql +foreground-ink+)))
  (medium-color-rgb medium (medium-foreground medium)))

(defmethod medium-color-rgb (medium (ink (eql +background-ink+)))
  (medium-color-rgb medium (medium-background medium)))

(defmethod medium-color-rgb (medium (ink color))
  (declare (ignore medium))
  (color-rgb ink))

(defmethod postscript-set-graphics-state (stream medium (kind (eql :color)))
  (multiple-value-bind (r g b)
      (medium-color-rgb medium (medium-ink medium))
    (format stream "~,3F ~,3F ~,3F setrgbcolor~%" r g b)))

;;; Clipping region
(defgeneric postscript-set-clipping-region (stream region))

(defmethod postscript-set-clipping-region (stream region)
  (format stream "newpath~%")
  (postscript-add-path stream region)
  (format stream "clip~%"))

(defmethod postscript-set-clipping-region (stream (region (eql +everywhere+)))
  (declare (ignore stream)))

(defmethod postscript-set-clipping-region (stream (region (eql +nowhere+)))
  (format stream "newpath 0 0 moveto closepath clip~%"))

(defmethod postscript-set-graphics-state (stream medium
                                          (kind (eql :clipping-region)))
  ;; FIXME: There is no way to enlarge clipping path. Current code
  ;; does only one level of saving graphics state, so we can restore
  ;; and save again GS to obtain an initial CP. It is ugly, but I see
  ;; no other way now. -- APD, 2002-02-11
  (postscript-restore-graphics-state medium)
  (postscript-save-graphics-state medium)
  (postscript-set-clipping-region stream
                                  (medium-clipping-region medium)))


;;; Medium drawing functions

;;; FIXME: the following methods should share code with POSTSCRIPT-ADD-PATH

(defmethod medium-draw-point* ((medium postscript-medium) x y)
  (let ((stream (postscript-medium-file-stream medium))
        (*transformation* (sheet-native-transformation (medium-sheet medium)))
        (radius (format-postscript-number (/ (medium-line-thickness medium) 2))))
    (postscript-actualize-graphics-state stream medium :color)
    (with-graphics-state (medium) ; FIXME: this is because of setlinewidth below
      (format stream "newpath~%")
      (write-coordinates stream x y)
      (format stream "~A 0 360 arc~%" radius)
      (format stream "0 setlinewidth~%")
      (format stream "fill~%"))))

(defmethod medium-draw-points* ((medium postscript-medium) coord-seq)
  (let ((stream (postscript-medium-file-stream medium))
        (*transformation* (sheet-native-transformation (medium-sheet medium)))
        (radius (format-postscript-angle (/ (medium-line-thickness medium) 2))))
    (postscript-actualize-graphics-state stream medium :color)
    (with-graphics-state (medium) ; FIXME: this is because of setlinewidth below
      (format stream "0 setlinewidth~%")
      (map-repeated-sequence 'nil 2
                             (lambda (x y)
                               (format stream "newpath~%")
                               (write-coordinates stream x y)
                               (format stream "~A 0 360 arc~%" radius)
                               (format stream "fill~%"))
                             coord-seq))))

(defmethod medium-draw-line* ((medium postscript-medium) x1 y1 x2 y2)
  (let ((stream (postscript-medium-file-stream medium))
        (*transformation* (sheet-native-transformation (medium-sheet medium))))
    (postscript-actualize-graphics-state stream medium :line-style :color)
    (format stream "newpath~%")
    (moveto stream x1 y1)
    (lineto stream x2 y2)
    (format stream "stroke~%")))

(defmethod medium-draw-lines* ((medium postscript-medium) coord-seq)
  (let ((stream (postscript-medium-file-stream medium))
        (*transformation* (sheet-native-transformation (medium-sheet medium))))
    (postscript-actualize-graphics-state stream medium :line-style :color)
    (format stream "newpath~%")
    (map-repeated-sequence 'nil 4
                           (lambda (x1 y1 x2 y2)
                             (moveto stream x1 y1)
                             (lineto stream x2 y2))
                           coord-seq)
    (format stream "stroke~%")))

(defmethod medium-draw-polygon*
    ((medium postscript-medium) coord-seq closed filled)
  (assert (evenp (length coord-seq)))
  (let ((stream (postscript-medium-file-stream medium))
        (*transformation* (sheet-native-transformation (medium-sheet medium))))
    (postscript-actualize-graphics-state stream medium :line-style :color)
    (format stream "newpath~%")
    (let ((command "moveto"))
      (map-repeated-sequence 'nil 2
                             (lambda (x y)
                               (write-coordinates stream x y)
                               (format stream "~A~%"
                                       command)
                               (setq command "lineto"))
                             coord-seq))
    (when closed
      (format stream "closepath~%"))
    (format stream (if filled
                       "fill~%"
                       "stroke~%"))))

(defmethod medium-draw-rectangle*
    ((medium postscript-medium) x1 y1 x2 y2 filled)
  (let ((stream (postscript-medium-file-stream medium))
        (*transformation* (sheet-native-transformation (medium-sheet medium))))
    (postscript-actualize-graphics-state stream medium :line-style :color)
    (format stream "newpath~%")
    (moveto stream x1 y1)
    (lineto stream x2 y1)
    (lineto stream x2 y2)
    (lineto stream x1 y2)
    (format stream "closepath~%")
    (format stream (if filled
                       "fill~%"
                       "stroke~%"))))

(defmethod medium-draw-rectangles*
    ((medium postscript-medium) position-seq filled)
  (assert (evenp (length position-seq)))
  (let ((stream (postscript-medium-file-stream medium))
        (*transformation* (sheet-native-transformation (medium-sheet medium))))
    (postscript-actualize-graphics-state stream medium :line-style :color)
    (format stream "newpath~%")
    (map-repeated-sequence 'nil 4
        (lambda (x1 y1 x2 y2)
          (moveto stream x1 y1)
          (lineto stream x2 y1)
          (lineto stream x2 y2)
          (lineto stream x1 y2)
          (format stream "closepath~%"))
         position-seq)
    (format stream (if filled
                       "fill~%"
                       "stroke~%"))))

(defmethod medium-draw-ellipse* ((medium postscript-medium) center-x center-y
				 radius1-dx radius1-dy radius2-dx radius2-dy
				 start-angle end-angle filled)
  (let* ((stream (postscript-medium-file-stream medium))
         (*transformation* (sheet-native-transformation (medium-sheet medium)))
         (ellipse (transform-region
                   *transformation*
                   (make-ellipse* center-x center-y
                                  radius1-dx radius1-dy radius2-dx radius2-dy
                                  :start-angle start-angle
                                  :end-angle end-angle))))
    (multiple-value-bind (ndx1 ndy1 ndx2 ndy2) (ellipse-normal-radii* ellipse)
      (let* ((center (ellipse-center-point ellipse))
             (cx (point-x center))
             (cy (point-y center))
             (tr (make-transformation ndx1 ndx2 ndy1 ndy2 cx cy))
             (circle (untransform-region tr ellipse))
             (start-angle (ellipse-start-angle circle))
             (end-angle (ellipse-end-angle circle)))
        (postscript-actualize-graphics-state stream medium :line-style :color)
        (format stream "matrix currentmatrix~%")
        (write-transformation* stream ndx1 ndy1 ndx2 ndy2 cx cy)
        (format stream "concat~%")
        (format stream "newpath~%")
        (format stream "0 0 1 ~A ~A arc~%"
                (format-postscript-angle start-angle)
                (format-postscript-angle (if (< end-angle start-angle)
                                             (+ end-angle (* 2 pi))
                                             end-angle)))
        (when filled
          (format stream "0 0 lineto~%"))
        (format stream "setmatrix~%")
        (format stream (if filled
                           "fill~%"
                           "stroke~%"))))))

(defconstant +postscript-fonts+
  '(:fix (:roman "Courier"
          :bold "Courier-Bold"
          :italic "Courier-Oblique"
          :bold-italic "Courier-BoldOblique"
          :italic-bold "Courier-BoldOblique")
    :serif (:roman "Times-Roman"
            :bold "Times-Bold"
            :italic "Times-Italic"
            :bold-italic "Times-BoldItalic"
            :italic-bold "Times-BoldItalic")
    :sans-serif (:roman "Helvetica"
                 :bold "Helvetica-Bold"
                 :italic "Helvetica-Oblique"
                 :bold-italic "Helvetica-BoldOblique"
                 :italic-bold "Helvetica-BoldOblique")))

(defconstant +postscript-font-sizes+
  '(:normal 14
    :tiny 8
    :very-small 10
    :small 12
    :large 18
    :very-large 20
    :huge 24))

(defun text-style->postscript-font (text-style)
  (multiple-value-bind (family face size) (text-style-components text-style)
    (let* ((family-fonts (or (getf +postscript-fonts+ family)
                             (getf +postscript-fonts+ :fix)))
           (font-name (or (getf family-fonts face)
                          (getf family-fonts :roman)))
           (size-number (if (numberp size)
                            (round size)
                            (or (getf +postscript-font-sizes+ size)
                                (getf +postscript-font-sizes+ :normal)))))
      (values font-name size-number))))

(defun medium-font (medium)
  (text-style->postscript-font (medium-text-style medium)))

(defmethod postscript-set-graphics-state (stream medium
                                          (kind (eql :text-style)))
  (multiple-value-bind (font size)
      (medium-font medium)
    (pushnew font (slot-value medium 'document-fonts) :test #'string=)
    (format stream "/~A findfont ~D scalefont setfont~%" font size)))

(defun postscript-escape-char (char)
  (case char
    (#\Linefeed "\\n")
    (#\Return "\\r")
    (#\Tab "\\t")
    (#\Backspace "\\b")
    (#\Page "\\f")
    (#\\ "\\\\")
    (#\( "\\(")
    (#\) "\\)")
    (t (if (standard-char-p char)
           (string char)
           (format nil "\\~3,'0O" (char-code char))))))

(defun postscript-escape-string (string)
  (apply #'concatenate 'string
         (map 'list #'postscript-escape-char string)))

(defmethod medium-draw-text* ((medium postscript-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (setq string (if (characterp string)
                   (make-string 1 :initial-element string)
                   (subseq string start end)))
  (let ((*transformation* (sheet-native-transformation (medium-sheet medium))))
    (with-slots (file-stream) medium
      (postscript-actualize-graphics-state file-stream medium :color :text-style)
      (with-graphics-state (medium)
        (when transform-glyphs
          ;;
          ;; Now the harder part is that we also want to transform the glyphs,
          ;; which is rather painless in Postscript. BUT: the x/y coordinates
          ;; we get are already transformed coordinates, so what I do is
          ;; untransform them again and simply tell the postscript interpreter
          ;; our transformation matrix. --GB
          ;;
          ;; It should be updated. -- APD, 2002-05-27
          (multiple-value-setq (x y)
            (untransform-position (medium-transformation medium) x y))
          (multiple-value-bind (mxx mxy myx myy tx ty)
              (get-transformation (medium-transformation medium))
            (format file-stream "initmatrix [~A ~A ~A ~A ~A ~A] concat~%"
                    (format-postscript-number mxx)
                    (format-postscript-number mxy)
                    (format-postscript-number myx)
                    (format-postscript-number myy)
                    (format-postscript-number tx)
                    (format-postscript-number ty))))
        (multiple-value-bind (total-width total-height
                              final-x final-y baseline)
            (multiple-value-call #'text-size-in-font
              (medium-font medium) string 0 (length string))
          (declare (ignore final-x final-y))
          ;; Only one line?
          (setq x (ecase align-x
                    (:left x)
                    (:center (- x (/ total-width 2)))
                    (:right (- x total-width))))
          (setq y (ecase align-y
                    (:baseline y)
                    (:top (+ y baseline))
                    (:center (- y (- (/ total-height 2)
                                     baseline)))
                    (:bottom (- y (- total-height baseline)))))
          (moveto file-stream x y))
        (format file-stream "(~A) show~%" (postscript-escape-string string))))))

;; The following four functions should be rewritten: AFM contains all
;; needed information
(defmethod text-style-ascent (text-style (medium postscript-medium))
  (multiple-value-bind (width height final-x final-y baseline)
      (text-size medium "I" :text-style text-style)
    (declare (ignore width height final-x final-y))
    baseline))

(defmethod text-style-descent (text-style (medium postscript-medium))
  (multiple-value-bind (width height final-x final-y baseline)
      (text-size medium "q" :text-style text-style)
    (declare (ignore width final-x final-y))
    (- height baseline)))

(defmethod text-style-height (text-style (medium postscript-medium))
  (multiple-value-bind (width height final-x final-y baseline)
      (text-size medium "Iq" :text-style text-style)
    (declare (ignore width final-x final-y baseline))
    height))

(defmethod text-style-width (text-style (medium postscript-medium))
  (multiple-value-bind (width height final-x final-y baseline)
      (text-size medium "M" :text-style text-style)
    (declare (ignore height final-x final-y baseline))
    width))

(defmethod text-size ((medium postscript-medium) string
                      &key text-style (start 0) end)
  (when (characterp string)
    (setq string (string string)))
  (multiple-value-bind (font size)
      (text-style->postscript-font
       (merge-text-styles text-style
                          (medium-text-style medium)))
    (text-size-in-font font size
                       string start (or end (length string)))))
