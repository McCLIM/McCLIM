;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)

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
   (document-fonts :initform '())))

(defun make-postscript-medium (file-stream device-type
                               multi-page scale-to-fit
                               orientation header-comments)
  (declare (ignore device-type multi-page scale-to-fit))
  (let ((title (or (getf header-comments :title)
                   *default-postscript-title*))
        (for (or (getf header-comments :for)
                 *default-postscript-for*)))
    (make-instance 'postscript-medium
                   :file-stream file-stream
                   :title title :for for
                   :orientation orientation)))

(defmacro with-output-to-postscript-stream ((stream-var file-stream
                                            &key device-type
                                            multi-page scale-to-fit
                                            (orientation :portrait)
                                            header-comments)
                                            &body body)
  `(let ((,stream-var (make-postscript-medium ,file-stream ,device-type
                                              ,multi-page ,scale-to-fit
                                              ,orientation ,header-comments)))
    (invoke-with-output-to-postscript-stream ,stream-var
     #'(lambda (,stream-var) ,@body))))

(defmethod invoke-with-output-to-postscript-stream ((medium postscript-medium) continuation)
  (funcall continuation medium))

(defmethod invoke-with-output-to-postscript-stream :before ((medium postscript-medium) continuation)
  (declare (ignore continuation))
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
    (format file-stream "newpath~%")))

(defmethod invoke-with-output-to-postscript-stream :after ((medium postscript-medium) continuation)
  (declare (ignore continuation))
  (with-slots (file-stream current-page document-fonts) medium
    (format file-stream "showpage~%~%")
    (format file-stream "%%Trailer~%")
    (format file-stream "%%Pages: ~D~%" current-page)
    (format file-stream "%%DocumentNeededResources: ~{font ~A~%~^%%+ ~}" (reverse document-fonts))
    (format file-stream "%%EOF~%")
    (finish-output file-stream)))

(defun new-page (stream)
  (with-slots (file-stream current-page) stream
    (format file-stream "showpage~%")
    (format file-stream "%%Page: ~D ~:*~D~%" (incf current-page))))

(defconstant +postscript-line-joints+ '(:miter 0
                                        :round 1
                                        :bevel 2
                                        :none 0))

(defconstant +postscript-line-caps+ '(:butt 0
                                      :round 1
                                      :square 2 ; extended butt caps
                                      :no-end-point 0))

(defconstant +postscript-default-line-dashes+ '(3 3))

(defun postscript-line-style (stream medium)
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

(defmethod medium-color-rgb (medium (ink (eql +foreground-ink+)))
  (medium-color-rgb medium (medium-foreground medium)))

(defmethod medium-color-rgb (medium (ink (eql +background-ink+)))
  (medium-color-rgb medium (medium-background medium)))

(defmethod medium-color-rgb (medium (ink color))
  (declare (ignore medium))
  (color-rgb ink))

(defun postscript-color (stream medium)
  (multiple-value-bind (r g b) (medium-color-rgb medium (medium-ink medium))
    (format stream "~,3F ~,3F ~,3F setrgbcolor~%" r g b)))

(defun postscript-line-style-and-color (stream medium)
  (postscript-line-style stream medium)
  (postscript-color stream medium))

(defmacro with-graphics-state ((stream) &body body)
  (let ((gstream (gensym)))
    `(let ((,gstream ,stream))
      (format ,gstream "gsave~%")
      ,@body
      (format ,gstream "grestore~%"))))

(defun format-postscript-number (number)
  (if (not (integerp number))
      (coerce number 'single-float)
      number))

(defun format-postscript-angle (angle)
  (format-postscript-number (* angle (/ 180 pi))))

(defmethod medium-draw-point* ((medium postscript-medium) x y)
  (let ((stream (postscript-medium-file-stream medium)))
    (with-graphics-state (stream)
      (format stream "~A ~A ~A 0 360 arc~%"
	      (format-postscript-number x) (format-postscript-number y)
              (format-postscript-number
               (/ (line-style-thickness (medium-line-style medium)) 2)))
      (format stream "0 setlinewidth~%")
      (postscript-color stream medium)
      (format stream "fill~%"))))

(defmethod medium-draw-points* ((medium postscript-medium) coord-seq)
  (let ((stream (postscript-medium-file-stream medium)))
    (with-graphics-state (stream)
      (format stream "0 setlinewidth~%")
      (postscript-color stream medium)
      (loop with radius = (/ (line-style-thickness (medium-line-style medium)) 2)
	    for (x y) on coord-seq by #'cddr
	    do (progn
		 (format stream "~A ~A ~A 0 360 arc~%"
                         (format-postscript-number x) (format-postscript-number y)
                         (format-postscript-number radius))
		 (format stream "fill~%")
		 (format stream "newpath~%"))))))

(defmethod medium-draw-line* ((medium postscript-medium) x1 y1 x2 y2)
  (let ((stream (postscript-medium-file-stream medium)))
    (with-graphics-state (stream)
      (postscript-line-style-and-color stream medium)
      (format stream "~A ~A moveto ~A ~A lineto~%"
              (format-postscript-number x1) (format-postscript-number y1)
              (format-postscript-number x2) (format-postscript-number y2))
      (format stream "stroke~%"))))

(defmethod medium-draw-lines* ((medium postscript-medium) coord-seq)
  (let ((stream (postscript-medium-file-stream medium)))
    (with-graphics-state (stream)
      (postscript-line-style-and-color stream medium)
      (loop with points = (apply #'vector coord-seq)
            for i below (length coord-seq) by 4
            do
            (format stream "~A ~A moveto ~A ~A lineto~%"
                    (format-postscript-number (aref points i))
                    (format-postscript-number (aref points (1+ i)))
                    (format-postscript-number (aref points (+ i 2)))
                    (format-postscript-number (aref points (+ i 3))))
            finally (format stream "stroke~%")))))

(defmethod medium-draw-polygon* ((medium postscript-medium) coord-seq closed filled)
  (assert (evenp (length coord-seq)))
  (let ((stream (postscript-medium-file-stream medium)))
    (with-graphics-state (stream)
      (format stream "~A ~A moveto~%"
              (format-postscript-number (car coord-seq))
              (format-postscript-number (cadr coord-seq)))
      (loop for (x y) on (cddr coord-seq) by #'cddr
            do (format stream "~A ~A lineto~%"
                       (format-postscript-number x) (format-postscript-number y))
            finally (format stream "~%"))
      (when closed
        (format stream "closepath~%"))
      (postscript-line-style-and-color stream medium)
      (format stream (if filled
                         "fill~%"
                         "stroke~%")))))

(defmethod medium-draw-rectangle* ((medium postscript-medium) x1 y1 x2 y2 filled)
  (let ((stream (postscript-medium-file-stream medium)))
    (with-graphics-state (stream)
      (format stream "~A ~A moveto ~A ~A lineto ~A ~A lineto ~A ~A lineto~%"
              (format-postscript-number x1) (format-postscript-number y1)
              (format-postscript-number x2) (format-postscript-number y1)
              (format-postscript-number x2) (format-postscript-number y2)
              (format-postscript-number x1) (format-postscript-number y2))
      (format stream "closepath~%")
      (postscript-line-style-and-color stream medium)
      (format stream (if filled
                         "fill~%"
                         "stroke~%")))))

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
        (with-graphics-state (stream)
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
          (when filled
            (format stream "0 0 lineto~%"))
          (format stream "setmatrix~%")
          (postscript-line-style-and-color stream medium)
          (format stream (if filled
                             "fill~%"
                           "stroke~%")))))))

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
  (with-slots (family face size) text-style
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
  (declare (ignore toward-x toward-y transform-glyphs))
  (setq string (if (characterp string)
                   (make-string 1 :initial-element string)
                   (subseq string start end)))
  (with-slots (file-stream document-fonts) medium
      (with-graphics-state (file-stream)
        (multiple-value-bind (font size)
            (text-style->postscript-font (medium-text-style medium))
          (pushnew font document-fonts :test #'string=)
          (format file-stream "/~A findfont ~D scalefont setfont~%" font size)
          (format file-stream "~A ~A moveto~%"
                  (format-postscript-number x) (format-postscript-number y))
          (format file-stream "(~A) show~%" (postscript-escape-string string))))))
