;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)

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
;;; - (?) WITH-OUTPUT-TO-POSTSCRIPT-STREAM should bind its first argument
;;;   to stream, not to medium.
;;;
;;; - POSTSCRIPT-ACTUALIZE-GRAPHICS-STATE: fix CLIPPING-REGION reusing logic
;;; - NEWPAGE should do smth with GS
;;; - MEDIUM-DRAW-... should not duplicate code from POSTSCRIPT-ADD-PATH
;;; - structure this file

;;; Also missing IMO:
;;;
;;; - WITH-OUTPUT-TO-POSTSCRIPT-STREAM should offer a :PAPER-SIZE option.
;;; - How can one ask for the dimensions of a postscript device (aka paper-size)?
;;; - NEW-PAGE should also offer to specify the page name.
;;; - device fonts are missing
;;; - font metrics are missing
;;;
;;;--GB

(in-package :CLIM-INTERNALS)

(defvar *default-postscript-title* "")

(defvar *default-postscript-for*
  #+unix (or (get-environment-variable "USER")
             "Unknown")
  #-unix "")

(defclass postscript-medium (basic-medium)
  ((file-stream :initarg :file-stream :reader postscript-medium-file-stream)
   (title :initarg :title)
   (for :initarg :for)
   (orientation :initarg :orientation)
   (current-page :initform 1)
   (document-fonts :initform '())
   (graphics-state-stack :initform '())))

(defmacro postscript-medium-graphics-state (medium)
  `(car (slot-value ,medium 'graphics-state-stack)))

(defmacro with-graphics-state ((medium) &body body)
  `(invoke-with-graphics-state ,medium
    (lambda () ,@body)))

(defun make-postscript-medium (file-stream device-type
                               multi-page scale-to-fit
                               orientation header-comments)
  (declare (ignore device-type multi-page scale-to-fit))
  (let ((title (or (getf header-comments :title)
                   *default-postscript-title*))
        (for (or (getf header-comments :for)
                 *default-postscript-for*)))
    (make-instance 'postscript-medium
                   :sheet (make-postscript-graft)
                   :file-stream file-stream
                   :title title :for for
                   :orientation orientation)))

(defmacro with-output-to-postscript-stream ((stream-var file-stream
                                             &rest options)
                                            &body body)
  (let ((cont (gensym)))
    `(labels ((,cont (,stream-var)
               ,@body))
      (declare (dynamic-extent #',cont))
      (invoke-with-output-to-postscript-stream #',cont ,file-stream ,@options))))

(defun invoke-with-output-to-postscript-stream (continuation
                                                file-stream &key device-type
                                                multi-page scale-to-fit
                                                (orientation :portrait)
                                                header-comments)
  (let ((medium (make-postscript-medium file-stream device-type
                                        multi-page scale-to-fit
                                        orientation header-comments)))
    (prog2
        (with-slots (file-stream title for orientation) medium
          (format file-stream "%!PS-Adobe-3.0~%")
          (format file-stream "%%Title: ~A~%" title)
          (format file-stream "%%For: ~A~%" for)
          (format file-stream "%%Orientation: ~A~%"
                  (ecase orientation
                    (:portrait "Portrait")
                    (:landscape "Landscape")))
          (format file-stream "%%Pages: (atend)~%")
          (format file-stream "%%DocumentNeededResources: (atend)~%")
          (format file-stream "%%EndComments~%~%")
          (format file-stream "%%Page: 1 1~%")
          (format file-stream "newpath~%"))
        (with-graphics-state (medium)
          ;; we need at least one level of saving -- APD, 2002-02-11
          (funcall continuation medium))
      (with-slots (file-stream current-page document-fonts) medium
        (format file-stream "showpage~%~%")
        (format file-stream "%%Trailer~%")
        (format file-stream "%%Pages: ~D~%" current-page)
        (format file-stream "%%DocumentNeededResources: ~{font ~A~%~^%%+ ~}~%" (reverse document-fonts))
        (format file-stream "%%EOF~%")
        (finish-output file-stream)))))

(defun new-page (stream)
  ;; FIXME: it is necessary to do smth with GS -- APD, 2002-02-11
  (postscript-restore-graphics-state stream)
  (with-slots (file-stream current-page) stream
    (format file-stream "showpage~%")
    (format file-stream "%%Page: ~D ~:*~D~%" (incf current-page)))
  (postscript-save-graphics-state stream))


;;; Postscript output utilities

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
    (format stream "~A ~A moveto~%"
            (format-postscript-number (point-x (first points)))
            (format-postscript-number (point-y (first points))))
    (loop for point in (rest points)
          do (format stream "~A ~A lineto~%"
                     (format-postscript-number (point-x point))
                     (format-postscript-number (point-y point))))
    (format stream "closepath~%")))

(defmethod postscript-add-path (stream (ellipse ellipse))
  (multiple-value-bind (center-x center-y)
      (ellipse-center-point* ellipse)
    (let ((start-angle (or (ellipse-start-angle ellipse) 0))
          (end-angle (or (ellipse-end-angle ellipse) (* 2 pi))))
      (multiple-value-bind (ndx1 ndy1 ndx2 ndy2) (ellipse-normal-radii* ellipse)
        (let* ((angle (atan* ndx1 ndy1))
               (s1 (sqrt (+ (* ndx1 ndx1) (* ndy1 ndy1))))
               (s2 (sqrt (+ (* ndx2 ndx2) (* ndy2 ndy2))))
               (tr (compose-transformation-with-scaling
                    (make-rotation-transformation angle)
                    s1 s2))
               (start-angle (untransform-angle tr start-angle))
               (end-angle (untransform-angle tr end-angle)))
          (format stream "matrix currentmatrix~%")
          (format stream "~A ~A translate~%"
                  (format-postscript-number center-x)
                  (format-postscript-number center-y))
          (format stream "~A rotate~%"
                  (format-postscript-angle angle))
          (format stream "~A ~A scale~%"
                  (format-postscript-number s1) (format-postscript-number s2))
          (format stream "0 0 1 ~A ~A arc~%"
                  (format-postscript-angle start-angle)
                  (format-postscript-angle end-angle))
          ;; (when filled ; uncomment when this code will be shared
          ;;              ; with ...-ELLIPTICAL-ARC
          (format stream "0 0 lineto~%")
          ;;)
          (format stream "closepath~%")
          (format stream "setmatrix~%"))))))


;;; Graphics state

(defgeneric postscript-set-graphics-state (stream medium kind))

(defvar *postscript-graphics-states*
  '((:line-style . medium-line-style)
    (:color . medium-ink)
    (:clipping-region . medium-clipping-region)))

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
        unless (eq (postscript-current-state medium kind)
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

(defconstant +postscript-default-line-dashes+ '(3 3))

(defmethod postscript-set-graphics-state (stream medium
                                          (kind (eql :line-style)))
  (let ((line-style (medium-line-style medium)))
    (format stream "~A setlinewidth ~A setlinejoin ~A setlinecap~%"
            (line-style-thickness line-style)
            (getf +postscript-line-joints+
                  (line-style-joint-shape line-style))
            (getf +postscript-line-caps+
                  (line-style-cap-shape line-style)))
    (let ((dashes (line-style-dashes line-style)))
      (when dashes
        (format stream "[~{~D~^ ~}] 0 setdash~%"
                (if (eq dashes t)
                    +postscript-default-line-dashes+
                    dashes))))))

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
  (let ((stream (postscript-medium-file-stream medium)))
    (postscript-actualize-graphics-state stream medium :color)
    (with-graphics-state (medium) ; FIXME: this is because of setlinewidth below
      (format stream "newpath~%")
      (format stream "~A ~A ~A 0 360 arc~%"
              (format-postscript-number x) (format-postscript-number y)
              (format-postscript-number
               (/ (line-style-thickness (medium-line-style medium)) 2)))
      (format stream "0 setlinewidth~%")
      (format stream "fill~%"))))

(defmethod medium-draw-points* ((medium postscript-medium) coord-seq)
  (let ((stream (postscript-medium-file-stream medium))
        (radius (/ (line-style-thickness (medium-line-style medium)) 2)))
    (postscript-actualize-graphics-state stream medium :color)
    (with-graphics-state (medium) ; FIXME: this is because of setlinewidth below
      (format stream "0 setlinewidth~%")
      (map-repeated-sequence 'nil 2
                             (lambda (x y)
                               (format stream "newpath~%")
                               (format stream "~A ~A ~A 0 360 arc~%"
                                       (format-postscript-number x) (format-postscript-number y)
                                       (format-postscript-number radius))
                               (format stream "fill~%"))
                             coord-seq))))

(defmethod medium-draw-line* ((medium postscript-medium) x1 y1 x2 y2)
  (let ((stream (postscript-medium-file-stream medium)))
    (postscript-actualize-graphics-state stream medium :line-style :color)
    (format stream "newpath~%")
    (format stream "~A ~A moveto ~A ~A lineto~%"
            (format-postscript-number x1) (format-postscript-number y1)
            (format-postscript-number x2) (format-postscript-number y2))
    (format stream "stroke~%")))

(defmethod medium-draw-lines* ((medium postscript-medium) coord-seq)
  (let ((stream (postscript-medium-file-stream medium)))
    (postscript-actualize-graphics-state stream medium :line-style :color)
    (format stream "newpath~%")
    (map-repeated-sequence 'nil 4
                           (lambda (x1 y1 x2 y2)
                             (format stream "~A ~A moveto ~A ~A lineto~%"
                                     (format-postscript-number x1)
                                     (format-postscript-number y1)
                                     (format-postscript-number x2)
                                     (format-postscript-number y2)))
                           coord-seq)
    (format stream "stroke~%")))

(defmethod medium-draw-polygon* ((medium postscript-medium) coord-seq closed filled)
  (assert (evenp (length coord-seq)))
  (let ((stream (postscript-medium-file-stream medium)))
    (postscript-actualize-graphics-state stream medium :line-style :color)
    (format stream "newpath~%")
    (let ((command "moveto"))
      (map-repeated-sequence 'nil 2
                             (lambda (x y)
                               (format stream "~A ~A ~A~%"
                                       (format-postscript-number x)
                                       (format-postscript-number y)
                                       command)
                               (setq command "lineto"))
                             coord-seq))
    (when closed
      (format stream "closepath~%"))
    (format stream (if filled
                       "fill~%"
                       "stroke~%"))))

(defmethod medium-draw-rectangle* ((medium postscript-medium) x1 y1 x2 y2 filled)
  (let ((stream (postscript-medium-file-stream medium)))
    (postscript-actualize-graphics-state stream medium :line-style :color)
    (format stream "newpath~%")
    (format stream "~A ~A moveto ~A ~A lineto ~A ~A lineto ~A ~A lineto~%"
            (format-postscript-number x1) (format-postscript-number y1)
            (format-postscript-number x2) (format-postscript-number y1)
            (format-postscript-number x2) (format-postscript-number y2)
            (format-postscript-number x1) (format-postscript-number y2))
    (format stream "closepath~%")
    (format stream (if filled
                       "fill~%"
                       "stroke~%"))))

(defmethod medium-draw-rectangles* ((medium postscript-medium) position-seq filled)
  (assert (evenp (length position-seq)))
  (let ((stream (postscript-medium-file-stream medium)))
    (postscript-actualize-graphics-state stream medium :line-style :color)
    (format stream "newpath~%")
    (map-repeated-sequence 'nil 4
                           (lambda (x1 y1 x2 y2)
                             (format stream "~A ~A moveto ~A ~A lineto ~A ~A lineto ~A ~A lineto~%"
                                     (format-postscript-number x1) (format-postscript-number y1)
                                     (format-postscript-number x2) (format-postscript-number y1)
                                     (format-postscript-number x2) (format-postscript-number y2)
                                     (format-postscript-number x1) (format-postscript-number y2))
                             (format stream "closepath~%"))
                           position-seq)
    (format stream (if filled
                       "fill~%"
                       "stroke~%"))))

(defmethod medium-draw-ellipse* ((medium postscript-medium) center-x center-y
				 radius1-dx radius1-dy radius2-dx radius2-dy
				 start-angle end-angle filled)
  (let ((stream (postscript-medium-file-stream medium))
        (ellipse (make-ellipse* center-x center-y
                                radius1-dx radius1-dy radius2-dx radius2-dy
                                :start-angle start-angle
                                :end-angle end-angle)))
    (multiple-value-bind (ndx1 ndy1 ndx2 ndy2) (ellipse-normal-radii* ellipse)
      (let* ((angle (atan* ndx1 ndy1))
             (s1 (sqrt (+ (* ndx1 ndx1) (* ndy1 ndy1))))
             (s2 (sqrt (+ (* ndx2 ndx2) (* ndy2 ndy2))))
             (tr (compose-transformation-with-scaling
                  (make-rotation-transformation angle)
                  s1 s2))
             (start-angle (untransform-angle tr start-angle))
             (end-angle (untransform-angle tr end-angle)))
        (postscript-actualize-graphics-state stream medium :line-style :color)
        (format stream "matrix currentmatrix~%")
        (format stream "~A ~A translate~%"
                (format-postscript-number center-x)
                (format-postscript-number center-y))
        (format stream "~A rotate~%"
                (format-postscript-angle angle))
        (format stream "~A ~A scale~%"
                (format-postscript-number s1) (format-postscript-number s2))
        (format stream "newpath~%")
        (format stream "0 0 1 ~A ~A arc~%"
                (format-postscript-angle start-angle)
                (format-postscript-angle end-angle))
        (when filled
          (format stream "0 0 lineto~%"))
        (format stream "setmatrix~%")
        (format stream (if filled
                           "fill~%"
                           "stroke~%"))))))

(defconstant +postscript-fonts+ '(:fix (:roman "Courier"
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

(defconstant +postscript-font-sizes+ '(:normal 14
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
  (declare (ignore align-x align-y toward-x toward-y))
  (setq string (if (characterp string)
                   (make-string 1 :initial-element string)
                   (subseq string start end)))
  (with-slots (file-stream document-fonts) medium
    (with-graphics-state (medium)
      (when transform-glyphs
        ;;
        ;; Now the harder part is that we also want to transform the glyphs,
        ;; which is rather painless in Postscript. BUT: the x/y coordinates
        ;; we get are already transformed coordinates, so what I do is
        ;; untransform them again and simply tell the postscript interpreter
        ;; our transformation matrix. --GB
        ;;
        (multiple-value-setq (x y) (untransform-position (medium-transformation medium) x y))
        (multiple-value-bind (mxx mxy myx myy tx ty) (get-transformation (medium-transformation medium))
          (format file-stream "initmatrix [~A ~A ~A ~A ~A ~A] concat~%"
                  (format-postscript-number mxx)
                  (format-postscript-number mxy)
                  (format-postscript-number myx)
                  (format-postscript-number myy)
                  (format-postscript-number tx)
                  (format-postscript-number ty))))
      (multiple-value-bind (font size)
          (text-style->postscript-font (medium-text-style medium))
        (pushnew font document-fonts :test #'string=)
        (format file-stream "/~A findfont ~D scalefont setfont~%" font size)
        (format file-stream "~A ~A moveto~%"
                (format-postscript-number x) (format-postscript-number y))
        (format file-stream "(~A) show~%" (postscript-escape-string string))))))

;;;;
;;;; POSTSCRIPT-GRAFT
;;;;

(defclass postscript-graft (basic-sheet sheet-leaf-mixin)
  ((width  :initform 210 :reader postscript-graft-width)
   (height :initform 297 :reader postscript-graft-height)))

(defmethod graft-orientation ((graft postscript-graft))
  :graphics)

(defmethod graft-units ((graft postscript-graft))
  :device)

(defmethod graft-width ((graft postscript-graft) &key (units :device))
  (* (postscript-graft-width graft)
     (ecase units
       (:device         (/ 720 254))
       (:inches         (/ 10 254))
       (:millimeters    1)
       (:screen-sized   (/ (postscript-graft-width graft))))))

(defmethod graft-height ((graft postscript-graft) &key (units :device))
  (* (postscript-graft-height graft)
     (ecase units
       (:device         (/ 720 254))
       (:inches         (/ 10 254))
       (:millimeters    1)
       (:screen-sized   (/ (postscript-graft-height graft))))))

(defun make-postscript-graft ()
  (make-instance 'postscript-graft))

(defmethod sheet-region ((sheet postscript-graft))
  (make-rectangle* 0 0
                   (graft-width sheet :units (graft-units sheet))
                   (graft-height sheet :units (graft-units sheet))))

(defmethod graft ((sheet postscript-graft))
  sheet)
