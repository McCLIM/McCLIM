;;; -*- Mode: Lisp; Package: CLIM-GRAPHIC-FORMS; -*-

;;; (c) 2007 Jack D. Unrue (jdunrue (at) gmail (dot) com)
;;; based on the null backend by:

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

(defclass graphic-forms-pixmap (climi::mirrored-pixmap) ())

(defmethod medium-copy-area ((from-drawable graphic-forms-medium)
                             from-x from-y width height
                             (to-drawable graphic-forms-medium)
                             to-x to-y)
  ())

(defmethod medium-copy-area ((from-drawable graphic-forms-medium)
                             from-x from-y width height
                             (to-drawable graphic-forms-pixmap)
                             to-x to-y)
  ())

(defmethod medium-copy-area ((from-drawable graphic-forms-pixmap)
                             from-x from-y width height
                             (to-drawable graphic-forms-medium)
                             to-x to-y)
  ())

(defmethod medium-copy-area ((from-drawable graphic-forms-pixmap)
                             from-x from-y width height
                             (to-drawable graphic-forms-pixmap)
                             to-x to-y)
  ())
