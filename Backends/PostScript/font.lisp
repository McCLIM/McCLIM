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
   (descent :initarg :descent :reader char-descent)))

;;;
(defvar *font-metrics* (make-hash-table :test 'equal))

(defun define-font-metrics (name ascent descent angle char-infos)
  (let ((font-info (make-instance 'font-info
                                  :name name
                                  :ascent ascent
                                  :descent descent
                                  :italic-angle angle)))
    (setf (gethash name *font-metrics*) font-info)
    (loop for (code name width ascent descent) in char-infos
         do (when (>= code 0)
              (setf (aref (font-info-char-names font-info) code)
                    name))
         (setf (gethash name (font-info-char-infos font-info))
               (make-instance 'char-metrics
                              :width width
                              :ascent ascent
                              :descent descent)))))

;;;
(defun text-size-in-font (font-name size string start end)
  (declare (string string))
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
                (t (let ((metrics (gethash (aref char-names (char-code char))
                                           char-metrics)))
                     (incf width (char-width metrics))
                     (maxf ascent (char-ascent metrics))
                     (maxf descent (char-descent metrics))))))
    (values (* scale (max width upper-width))
            (* scale (+ ascent descent upper-height))
            (* scale width)
            (* scale upper-height)
            (* scale (+ upper-height ascent))))) ;?
