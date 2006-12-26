;;; -*- Mode: Lisp; -*-

;;;  (c) copyright 2005 by Gilbert Baumann <gilbert@base-engineering.com>
;;;  (c) copyright 2006 David Lichteblau (david@lichteblau.com)

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :clim-gtkairo)

;;; Locking rule for this file: Dokumented entry points in the CLIM
;;; package use WITH-GTK, internal functions can rely on that.

(defun df (x) (coerce x 'double-float))

(defclass gtkairo-medium (climi::basic-medium clim:medium)
    ((port :initarg :port :accessor port)))

(defclass metrik-medium-mixin () ())
(defclass cairo-metrik-medium (metrik-medium-mixin cairo-medium) ())
(defclass gdk-metrik-medium (metrik-medium-mixin gdk-medium) ())

(defgeneric invoke-with-medium (fn medium))

(defmacro with-medium ((medium) &body body)
  `(invoke-with-medium (lambda () ,@body) ,medium))

(defgeneric metrik-medium-for (medium))

(defun gtkwidget-gdkwindow (widget)
  (cffi:foreign-slot-value widget 'gtkwidget 'gdkwindow))

(defun medium-mirror (medium)
  (or (climi::port-lookup-mirror (port medium) (medium-sheet medium))
      (error "oops, drawing operation on unmirrored sheet ~A" medium)))

(defmethod engraft-medium :after ((medium gtkairo-medium) port sheet)
  )

(defmethod degraft-medium :after ((medium gtkairo-medium) port sheet)
  )

(defvar *medium-type* :cairo)

#+(or)
(setf *medium-type* :gdk)

#+(or)
(setf *medium-type* :cairo)

(defmethod make-medium ((port gtkairo-port) sheet)
  (make-instance (ecase *medium-type*
		   (:gdk 'gdk-medium)
		   (:cairo 'cairo-medium))
    :port port
    :sheet sheet))

;; copy&paste from medium.lisp|CLX:
;; this seems to work, but find out why all of these +nowhere+s are coming from
;; and kill them at the source...
(defun clipping-region->rect-seq (clipping-region)
  (loop
     for region in (nreverse (mapcan
			      (lambda (v) (unless (eq v +nowhere+) (list v)))
			      (region-set-regions clipping-region
						  :normalize :y-banding)))
     as rectangle = (bounding-rectangle region)
     for clip-x = (round-coordinate (rectangle-min-x rectangle))
     for clip-y = (round-coordinate (rectangle-min-y rectangle))
     collect (list clip-x
		    clip-y
		    (- (round-coordinate (rectangle-max-x rectangle)) clip-x)
		    (- (round-coordinate (rectangle-max-y rectangle)) clip-y))))

(defun untransform-size (transformation size)
  (multiple-value-bind (dx dy) (untransform-distance transformation size 0)
    (sqrt (+ (expt dx 2) (expt dy 2)))))

(defun transform-size (transformation size)
  (multiple-value-bind (dx dy) (transform-distance transformation size 0)
    (sqrt (+ (expt dx 2) (expt dy 2)))))

(defmethod invalidate-mirror ((mirror drawable-mirror) sheet)
  (declare (ignore sheet)))

(defmethod invalidate-mirror ((mirror widget-mirror) sheet)
  (let* ((drawable (mirror-drawable mirror))
	 (real-drawable (mirror-real-drawable mirror)))
    (unless (cffi:pointer-eq drawable real-drawable)
      (let* ((region (climi::sheet-mirror-region sheet))
	     (width (floor (bounding-rectangle-max-x region)))
	     (height (floor (bounding-rectangle-max-y region))))
	(cffi:with-foreign-object (r 'gdkrectangle)
	  (setf (cffi:foreign-slot-value r 'gdkrectangle 'width) width)
	  (setf (cffi:foreign-slot-value r 'gdkrectangle 'height) height)
	  (gdk_window_invalidate_rect real-drawable r 0))))))


;;;; ------------------------------------------------------------------------
;;;;  Text Styles
;;;;

;;; Diverse dieser Funktionen werden auf Mediums aufgerufen, deren Sheet
;;; noch keinen Mirror hat, und muessen tatsaechlich schon die richtige
;;; Antwort liefern.  Daher leite ich einfach generell all diese
;;; Anfragen auf ein zuvor angelegtes Medium fuer das Root-Fenster um.

(let ((hash (make-hash-table :test 'equal)))
  (defmethod text-style-ascent (text-style (medium gtkairo-medium))
    (let ((key (cons (class-name (class-of medium)) text-style)))
      (or #-debug-metrik (gethash key hash)
	  (setf (gethash key hash)
		(text-style-ascent text-style (metrik-medium-for medium)))))))

(let ((hash (make-hash-table :test 'equal)))
  (defmethod text-style-descent (text-style (medium gtkairo-medium))
    (let ((key (cons (class-name (class-of medium)) text-style)))
      (or #-debug-metrik (gethash key hash)
	  (setf (gethash key hash)
		(text-style-descent text-style (metrik-medium-for medium)))))))

(let ((hash (make-hash-table :test 'equal)))
  (defmethod text-style-height (text-style (medium gtkairo-medium))
    (let ((key (cons (class-name (class-of medium)) text-style)))
      (or #-debug-metrik (gethash key hash)
	  (setf (gethash key hash)
		(text-style-height text-style (metrik-medium-for medium)))))))

(let ((hash (make-hash-table :test 'equal)))
  (defmethod text-style-width (text-style (medium gtkairo-medium))
    (let ((key (cons (class-name (class-of medium)) text-style)))
      (or #-debug-metrik (gethash key hash)
	  (setf (gethash key hash)
		(text-style-width text-style (metrik-medium-for medium)))))))

(let ((hash (make-hash-table :test 'equal)))
  (defmethod text-style-fixed-width-p (text-style (medium gtkairo-medium))
    (let ((key (cons (class-name (class-of medium)) text-style)))
      (or #-debug-metrik (gethash key hash)
	  (setf (gethash key hash)
		(text-style-fixed-width-p text-style
					  (metrik-medium-for medium)))))))

(defmethod text-size
    ((medium gtkairo-medium) string &key text-style (start 0) end)
  (with-gtk ()
    (when (characterp string) (setf string (string string)))
    (setf text-style (or text-style (medium-text-style medium)))
    (setf text-style
	  (merge-text-styles text-style (medium-default-text-style medium)))
    (text-size (metrik-medium-for medium)
	       string
	       :text-style text-style
	       :start start
	       :end (or end (length string)))))

(defmethod climi::text-bounding-rectangle*
    ((medium gtkairo-medium) string &key text-style (start 0) end)
  (with-gtk ()
    (when (characterp string) (setf string (string string)))
    (setf text-style (or text-style (medium-text-style medium)))
    (setf text-style
	  (merge-text-styles text-style (medium-default-text-style medium)))
    (climi::text-bounding-rectangle* (metrik-medium-for medium)
				     string
				     :text-style text-style
				     :start start
				     :end (or end (length string)))))

;;;; ------------------------------------------------------------------------
;;;;  Hmm
;;;;

(defmethod medium-current-text-style ((medium gtkairo-medium))
  (merge-text-styles (medium-text-style medium)
		     (medium-default-text-style medium)))

(defmethod medium-merged-text-style ((medium gtkairo-medium))
  (merge-text-styles (medium-text-style medium)
		     (medium-default-text-style medium)))

;;;; ------------------------------------------------------------------------
