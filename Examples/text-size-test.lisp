;;; -*- Mode: Lisp; -*-

;;;  (c) 2006 David Lichteblau (david@lichteblau.com)

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

(in-package :clim-demo)

(define-application-frame text-size-test ()
    ()
  (:panes
   (canvas :application
	   :min-width 600
	   :display-time t
	   :display-function 'display-canvas)
   (text (make-pane 'text-field :value "ytmM"))
   (family
    (with-radio-box ()
      (make-pane 'toggle-button :label "Fixed" :id :fixed)
      (radio-box-current-selection
       (make-pane 'toggle-button :label "Serif" :id :serif))
      (make-pane 'toggle-button :label "Sans Serif" :id :sans-serif)))
   (face
    (with-radio-box (:type :some-of)
      (make-pane 'toggle-button :label "Bold" :id :bold)
      (make-pane 'toggle-button :label "Italic" :id :italic)))
   (rectangle
    (with-radio-box ()
      (radio-box-current-selection
       (make-pane 'toggle-button :label "Text-Size" :id :text-size))
      (make-pane 'toggle-button :label "Text-Bounding-Rectangle" :id :text-bounding-rectangle)))
   (size
    (make-pane 'slider
	       :orientation :horizontal
	       :value 200
	       :min-value 1
	       :max-value 1000)))
  (:layouts
   (default
       (vertically ()
	 (labelling (:label "Text") text)
	 (horizontally ()
	   (labelling (:label "Family") family)
	   (labelling (:label "Face") face)
           (labelling (:label "Rectangle") rectangle))
	 (labelling (:label "Size") size)
	 canvas))))

(defmethod display-canvas (frame stream)
  (window-clear stream)
  (let* ((pane-width (rectangle-width (sheet-region stream)))
	 (pane-height (rectangle-height (sheet-region stream)))
	 (str (gadget-value (find-pane-named frame 'text)))
	 (size (gadget-value (find-pane-named frame 'size)))
	 (family (gadget-id (gadget-value (find-pane-named frame 'family))))
	 (faces
	  (mapcar #'gadget-id (gadget-value (find-pane-named frame 'face))))
         (rectangle (gadget-id (gadget-value (find-pane-named frame 'rectangle))))
	 (face (if (cdr faces) '(:bold :italic) (car faces)))
	 (style (make-text-style family face size)))
    (multiple-value-bind (width height final-x final-y baseline)
	(text-size stream str :text-style style)
      (let ((x1 (/ (- pane-width width) 2))
	    (y1 (/ (- pane-height height) 2)))
	(draw-line* stream
		    0 (+ y1 baseline)
		    pane-width (+ y1 baseline)
		    :ink +green+)
	(draw-text* stream str x1 (+ y1 baseline) :text-style style)
	;; Here an attempt at testing text with newlines, results are garbage
	;; even with CLIM-CLX:
;;;	(setf (stream-cursor-position stream) (values x1 y1))
;;;	(with-text-style (stream style)
;;;	  (write-string str stream))
        (ecase rectangle
          ((:text-size)
           (draw-rectangle* stream
                            x1 y1
                            (+ x1 width) (+ y1 height)
                            :ink +red+
                            :filled nil)
           (draw-rectangle* stream
                            x1 y1
                            (+ x1 final-x) (+ y1 final-y)
                            :ink +blue+
                            :filled nil))
          ((:text-bounding-rectangle)
           (multiple-value-bind (left top right bottom)
               (climi::text-bounding-rectangle* (sheet-medium stream) str :text-style style)
             (draw-rectangle* stream 
                              (+ x1 left) (+ y1 baseline top)
                              (+ x1 right) (+ y1 baseline bottom)
                              :ink +purple+
                              :filled nil))))))))

(define-text-size-test-command (com-quit-text-size-test :menu "Quit") ()
  (frame-exit *application-frame*))

(define-text-size-test-command (com-update :menu "Update") ()
  (display-canvas *application-frame*
		  (frame-standard-output *application-frame*)))
