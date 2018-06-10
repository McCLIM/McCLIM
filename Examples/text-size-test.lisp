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
   (text (make-pane 'text-editor :height 200 :value "ytmM"))
   (family
    (with-radio-box ()
      (make-pane 'toggle-button :label "Fixed" :id :fix)
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
	       :value 120
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

(defun draw-vstrecke (stream x y1 y2 &rest args &key ink &allow-other-keys)
  (draw-line* stream (- x 10) y1 (+ x 10) y1 :ink ink)
  (draw-line* stream (- x 10) y2 (+ x 10) y2 :ink ink)
  (apply #'draw-arrow* stream x y1 x y2 args))

(defun draw-hstrecke (stream y x1 x2 &rest args &key ink &allow-other-keys)
  (draw-line* stream x1 (- y 10) x1 (+ y 10) :ink ink)
  (draw-line* stream x2 (- y 10) x2 (+ y 10) :ink ink)
  (apply #'draw-arrow* stream x1 y x2 y args))

(defun legend-text-style ()
  (make-text-style :sans-serif :roman :small))

(defun draw-legend (stream &rest entries)
  (let* ((style (legend-text-style))
	 (y 2)
	 (h (nth-value 1 (text-size stream "dummy" :text-style style))))
    (dolist (entry entries)
      (when entry
	(incf y h)
	(let ((y* (+ 0.5 (round (- y (/ h 2))))))
	  (apply #'draw-line* stream 2 y* 35 y* (cdr entry)))
	(draw-text* stream (car entry) 40 y :text-style style)))))

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
	 (style (make-text-style family face size))
	 (medium (sheet-medium stream)))
    (multiple-value-bind (width height final-x final-y baseline)
	(text-size stream str :text-style style)
      (let* ((x1 (/ (- pane-width width) 2))
	     (y1 (/ (- pane-height height) 2))
	     (ybase (+ y1 baseline)))
	(draw-text* stream
		    (format nil "fixed-width-p: ~(~A~)"
			    (handler-case
				(text-style-fixed-width-p style medium)
			      (error (c)
				c)))
		    2
		    pane-height
		    :text-style (legend-text-style))
	(draw-legend stream
		     (list "Ascent"
			   ;; :line-style (make-line-style :dashes '(1.5))
			   :ink +black+)
		     (list "Descent" :ink +black+)
		     (list "Height"
			   :line-style (make-line-style :thickness 2)
			   :ink +black+)
		     (list "Width (Avg.)" :ink +black+)
		     (list "Baseline" :ink +green+)
		     (when (eq rectangle :text-bounding-rectangle)
		       (list "Bounding rectangle" :ink +purple+))
		     (when (eq rectangle :text-size)
		       (list "Text size (width/height)" :ink +red+))
		     (when (eq rectangle :text-size)
		       (list "Text size (final x/y)" :ink +blue+)))
	(draw-vstrecke stream
		      (- x1 20)
		      ybase
		      (- ybase (text-style-ascent style medium))
		      ;; :line-style (make-line-style :dashes '(1.5))
		      :ink +black+)
	(draw-vstrecke stream
		      (- x1 20)
		      ybase
		      (+ ybase (text-style-descent style medium))
		      :ink +black+)
	(draw-vstrecke stream
		      (- x1 40)
		      y1
		      (+ y1 (text-style-height style medium))
		      :line-style (make-line-style :thickness 2)
		      :ink +black+)
	(draw-hstrecke stream
		       (- y1 20)
		       x1
		       (+ x1 (text-style-width style medium))
		       :ink +black+)
	(draw-line* stream
		    0 ybase
		    pane-width ybase
		    :ink +green+)
	(draw-text* stream str x1 ybase :text-style style)
	;; Gtkairo's DRAW-TEXT* understands multiple lines.
	;; (CLIM-CLX doesn't like multiple lines much.)
	;;
	;; If we use WRITE-STRING instead of DRAW-TEXT, the frontend will
	;; handle the line breaks, but lines 2..n will start at x = 0 rather
	;; than x = x1, confusing our diagram.
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
	    (draw-line* stream
			0 (+ y1 final-y)
			pane-width (+ y1 final-y)
			:ink +blue+)
	    (draw-line* stream
			(+ x1 final-x) 0
			(+ x1 final-x) pane-height
			:ink +blue+))
          ((:text-bounding-rectangle)
           (multiple-value-bind (left top right bottom)
               (climi::text-bounding-rectangle* medium str :text-style style)
             (draw-rectangle* stream 
                              (+ x1 left) (+ y1 baseline top)
                              (+ x1 right) (+ y1 baseline bottom)
                              :ink +purple+
                              :filled nil))))))))

(define-text-size-test-command (com-quit-text-size-test :menu "Quit") ()
  (frame-exit *application-frame*))

(define-text-size-test-command (com-update-text-size-test :menu "Update") ()
  (display-canvas *application-frame*
		  (frame-standard-output *application-frame*)))
