;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: DEFGENERICs and stuff
;;;   Created: 2001-08-12
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2001 by Gilbert Baumann

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

;;;; Changes

;;;  When        Who    What
;;; --------------------------------------------------------------------------------------
;;;  2001-08-12  GB     created
;;;

;; This is just an ad hoc list. Would it be a good idea to include all
;; (exported) generic functions here? --GB

(defgeneric point-x (point))
(defgeneric point-y (point))

(defgeneric transform-region (transformation region))

;; 8.3.4.1 Grafting and Degrafting of Mediums

(defgeneric allocate-medium (port sheet))
(defgeneric deallocate-medium (port medium))
(defgeneric make-medium (port sheet))
(defgeneric engraft-medium (medium port sheet))
(defgeneric degraft-medium (medium port sheet))

;; 8.4.1 Repaint Protocol Functions

(defgeneric queue-repaint (sheet repaint-event))
(defgeneric handle-repaint (sheet region))
(defgeneric repaint-sheet (sheet region))

;; 9 Ports, Grafts, and Mirrored Sheets

;; (defgeneric portp (object))
;; find-port function

;; 9.4.1 Mirror Functions

(defgeneric sheet-direct-mirror (sheet))
(defgeneric sheet-mirrored-ancestor (sheet))
(defgeneric sheet-mirror (sheet))
(defgeneric realize-mirror (port mirrored-sheet))
(defgeneric destroy-mirror (port mirrored-sheet))
(defgeneric raise-mirror (port sheet))
(defgeneric bury-mirror (port sheet))
(defgeneric port (object))

;; 9.4.2 Internal Interfaces for Native Coordinates

(defgeneric sheet-native-transformation (sheet))
(defgeneric sheet-native-region (sheet))
(defgeneric sheet-device-transformation (sheet))
(defgeneric sheet-device-region (sheet))
(defgeneric invalidate-cached-transformations (sheet))
(defgeneric invalidate-cached-regions (sheet))

;;; Graphics ops

(defgeneric medium-draw-point* (medium x y))
(defgeneric medium-draw-points* (medium coord-seq))
(defgeneric medium-draw-line* (medium x1 y1 x2 y2))
(defgeneric medium-draw-lines* (medium coord-seq))
(defgeneric medium-draw-polygon* (medium coord-seq closed filled))
(defgeneric medium-draw-rectangle* (medium left top right bottom filled))
(defgeneric medium-draw-ellipse* (medium center-x center-y
				  radius-1-dx radius-1-dy radius-2-dx radius-2-dy
				  start-angle end-angle filled))
(defgeneric medium-draw-text* (medium string x y
			       start end
			       align-x align-y
			       toward-x toward-y transform-glyphs))

;;;

(defgeneric medium-foreground (medium))
(defgeneric medium-background (medium))
(defgeneric medium-ink (medium))
(defgeneric medium-transformation (medium))
(defgeneric medium-clipping-region (medium))
(defgeneric medium-line-style (medium))
(defgeneric medium-line-style (medium))
(defgeneric medium-text-style (medium))
(defgeneric medium-default-text-style (medium))

(defgeneric (setf medium-foreground) (new-value medium))
(defgeneric (setf medium-background) (new-value medium))
(defgeneric (setf medium-ink) (new-value medium))
(defgeneric (setf medium-transformation) (new-value medium))
(defgeneric (setf medium-clipping-region) (new-value medium))
(defgeneric (setf medium-line-style) (new-value medium))
(defgeneric (setf medium-line-style) (new-value medium))
(defgeneric (setf medium-text-style) (new-value medium))
(defgeneric (setf medium-default-text-style) (new-value medium))

(defgeneric medium-sheet (medium))

;;;

(defmacro with-special-choices ((sheet) &body body)
  "Macro for optimizing drawing with graphical system dependant mechanisms."
  (let ((fn (gensym "FN.")))
    `(labels ((,fn (,sheet)
               ,@body))
      (declare (dynamic-extent #',fn))
      (invoke-with-special-choices #',fn ,sheet))))

(defgeneric invoke-with-special-choices (continuation sheet))

;; fall back, where to put this?

(defmethod invoke-with-special-choices (continuation (sheet T))
  (funcall continuation sheet))



