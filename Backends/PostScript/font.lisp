;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

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
;;; - Kerning, ligatures.
;;; - device fonts

(in-package :CLIM-POSTSCRIPT)

(defclass font-info ()
  ((name :type string :initarg :name :reader font-info-name)
   (ascent :initarg :ascent :reader font-info-ascent)
   (descent :initarg :descent :reader font-info-descent)
   (italic-angle :initarg :italic-angle :reader font-info-italic-angle)
   (char-names :initform (make-array 256 :initial-element nil)
               :reader font-info-char-names)
   (char-infos :initform (make-hash-table :test 'equal)
               :reader font-info-char-infos)))

(defclass char-metrics ()
  ((width :initarg :width :reader char-width)
   (ascent :initarg :ascent :reader char-ascent)
   (descent :initarg :descent :reader char-descent)
   (xmin :initarg :xmin :reader char-xmin)
   (xmax :initarg :xmax :reader char-xmax)))

;;;
(defvar *font-metrics* (make-hash-table :test 'equal))

(defun define-font-metrics (name ascent descent angle char-infos)
  (let ((font-info (make-instance 'font-info
                                  :name name
                                  :ascent ascent
                                  :descent descent
                                  :italic-angle angle)))
    (setf (gethash name *font-metrics*) font-info)
    (loop for (code name width ascent descent xmin xmax) in char-infos
         do (when (>= code 0)
              (setf (aref (font-info-char-names font-info) code)
                    name))
         (setf (gethash name (font-info-char-infos font-info))
               (make-instance 'char-metrics
                              :width width
                              :ascent ascent
                              :descent descent
			      :xmin xmin
			      :xmax xmax)))))

;;;
(defun text-size-in-font (font-name size string start end)
  (declare (string string))
  (unless end (setq end (length string)))
  (let* ((font-info (or (gethash font-name *font-metrics*)
                        (error "Unknown font ~S." font-name)))
         (char-names (font-info-char-names font-info))
         (char-metrics (font-info-char-infos font-info))
         (scale (/ size 1000))
         (width 0) (upper-width 0)
         (upper-height 0)
         (descent 0) (ascent 0) (upper-baseline 0))
    (loop for i from start below end
       for char = (aref string i)
       do (cond ((char= char #\Newline)
                 (maxf upper-width width) (setf width 0)
                 (incf upper-baseline (+ ascent descent))
                 (maxf upper-height (+ ascent descent))
                 (setf descent 0) (setf ascent 0))
                (t (let ((metrics (gethash (aref *iso-latin-1-symbolic-names* (char-code char))
                                           char-metrics)))
                     (incf width (char-width metrics))
                     (maxf ascent (char-ascent metrics))
                     (maxf descent (char-descent metrics))))))
    (values (* scale (max width upper-width))
            (* scale (+ ascent descent upper-height))
            (* scale width)
            (* scale upper-height)
            (* scale (+ upper-height ascent))))) ;?

;;;
(defconstant +postscript-fonts+
  '(:fix ((:roman . "Courier")
          (:bold . "Courier-Bold")
          (:italic . "Courier-Oblique")
          ((:bold :italic) . "Courier-BoldOblique"))
    :serif ((:roman . "Times-Roman")
            (:bold . "Times-Bold")
            (:italic . "Times-Italic")
            ((:bold :italic) . "Times-BoldItalic"))
    :sans-serif ((:roman . "Helvetica")
                 (:bold . "Helvetica-Bold")
                 (:italic . "Helvetica-Oblique")
                 ((:bold :italic) . "Helvetica-BoldOblique"))))

(defconstant +postscript-font-sizes+
  '(:normal 14
    :tiny 8
    :very-small 10
    :small 12
    :large 18
    :very-large 20
    :huge 24))

(defmethod text-style-mapping ((port postscript-port) text-style
                               &optional character-set)
  (declare (ignore character-set))
  (or (gethash text-style (port-text-style-mappings port))
      (multiple-value-bind (family face size) (text-style-components text-style)
        (let* ((family-fonts (or (getf +postscript-fonts+ family)
                                 (getf +postscript-fonts+ :fix)))
               (font-name (cdr (or (assoc face family-fonts :test #'equal)
                                   (assoc :roman family-fonts))))
               (size-number (if (numberp size)
                                (round size)
                                (or (getf +postscript-font-sizes+ size)
                                    (getf +postscript-font-sizes+ :normal)))))
          (cons font-name size-number)))))

(defmethod (setf text-style-mapping)
    (mapping (port postscript-port) (text-style text-style)
     &optional character-set)
  (declare (ignore character-set))
  (unless (and (consp mapping)
               (stringp (car mapping))
               (numberp (cdr mapping)))
    (error "Mapping a text style to a style specification is not~
    implemented."))
  (when (not (gethash (car mapping) *font-metrics*))
    (cerror "Ignore." "Mapping text style ~S to an unknown font ~S."
            text-style (car mapping)))
  (setf (gethash text-style (port-text-style-mappings port))
        mapping))

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

(defmethod climi::text-bounding-rectangle*
    ((medium postscript-medium) string
     &key text-style (start 0) end)
  (when (characterp string)
    (setf string (make-string 1 :initial-element string)))
  (unless end (setf end (length string)))
  (unless text-style (setf text-style (medium-text-style medium)))
  (destructuring-bind (psfont . size)
      (text-style-mapping (port medium)
                          (merge-text-styles text-style
                                             (medium-merged-text-style medium)))
    (let ((scale (/ size 1000)))
      (cond ((= start end)
             (values 0 0 0 0))
            (t
             (let ((position-newline (position #\newline string :start start)))
               (cond ((not (null position-newline))
                      (multiple-value-bind (width ascent descent left right
                                                  font-ascent font-descent
                                                  direction first-not-done)
                          (psfont-text-extents psfont string
                                               :start start :end position-newline)
                        (multiple-value-bind (minx miny maxx maxy)
                            (climi::text-bounding-rectangle*
                             medium string :text-style text-style
                             :start (1+ position-newline) :end end)
                          (values (* scale (min minx left))
                                  (* scale (- ascent))
                                  (* scale (max maxx right))
                                  (* scale (+ descent maxy))))))
                     (t
                      (multiple-value-bind (width ascent descent left right
                                                  font-ascent font-descent
                                                  direction first-not-done)
                          (psfont-text-extents psfont string
                                               :start start :end end)
                        (values (* scale left)
                                (* scale (- font-ascent))
                                (* scale right)
                                (* scale font-descent)))))))))))

(defun psfont-text-extents (font string &key (start 0) (end (length string)))
  (let* ((font-info (or (gethash font *font-metrics*)
			(error "Unknown font ~S." font)))
	 (char-metrics (font-info-char-infos font-info))
	 (width (loop for i from start below end
		   sum (char-width (gethash (aref *iso-latin-1-symbolic-names* (char-code (char string i)))
					    char-metrics)))))
    (values
     width
     (font-info-ascent font-info)
     (font-info-descent font-info)
     (char-xmin (gethash (aref *iso-latin-1-symbolic-names* (char-code (char string start)))
			 char-metrics))
     (- width (- (char-width (gethash (aref *iso-latin-1-symbolic-names* (char-code (char string (1- end))))
				      char-metrics))
		 (char-xmax (gethash (aref *iso-latin-1-symbolic-names* (char-code (char string (1- end))))
				     char-metrics))))
     (font-info-ascent font-info)
     (font-info-descent font-info)
     0 end)))
     



(defmethod text-size ((medium postscript-medium) string
                      &key text-style (start 0) end)
  (when (characterp string) (setq string (string string)))
  (unless end (setq end (length string)))
  (destructuring-bind (font . size)
      (text-style-mapping (port medium)
                          (merge-text-styles text-style
                                             (medium-merged-text-style medium)))
    (text-size-in-font font size
                       string start (or end (length string)))))
