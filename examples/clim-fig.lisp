;;; -*- Mode: Lisp; Package: CLIM-DEMO -*-

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

(defclass canvas-pane (application-pane)
  ((first-point-x :initform nil)
   (first-point-y :initform nil)
   (canvas-pixmap :initform nil)
   (selected-object :initform nil)))

(defun set-status-line (string)
  (setf (gadget-value (slot-value *application-frame* 'clim-demo::status))
	string))

(defmethod handle-event ((pane canvas-pane) (event pointer-button-press-event))
  (when (= (pointer-event-button event) +pointer-left-button+)
    (with-slots (first-point-x first-point-y canvas-pixmap) pane
      (let ((pixmap-width (round (bounding-rectangle-width (sheet-region pane))))
	    (pixmap-height (round (bounding-rectangle-height (sheet-region pane)))))
	(setq first-point-x (pointer-event-x event))
	(setq first-point-y (pointer-event-y event))
	(with-slots (clim-demo::line-style clim-demo::current-color clim-demo::drawing-mode) *application-frame*
	  (case clim-demo::drawing-mode
	    (:point
	     (draw-point* pane first-point-x first-point-y
			  :ink clim-demo::current-color
                          :line-style clim-demo::line-style))))
	(setf canvas-pixmap (allocate-pixmap pane pixmap-width pixmap-height))
	(copy-to-pixmap pane 0 0 pixmap-width pixmap-height canvas-pixmap))))
  (when (= (pointer-event-button event) +pointer-right-button+)
    (with-slots (first-point-x first-point-y selected-object) pane
      (setq first-point-x (pointer-event-x event)
            first-point-y (pointer-event-y event))
      (setf selected-object (search-selected-object (climi::pane-output-history pane)
                                                    first-point-x
                                                    first-point-y)))))

(defun search-selected-object (record x y)
  (let ((selected-objects))
    (map-over-output-records-containing-position
     #'(lambda (x)
         (push x selected-objects))
     record x y)
    (first selected-objects)))

(defun signum-1 (value)
  (if (zerop value)
      1
      (signum value)))

(defmethod handle-event ((pane canvas-pane) (event pointer-motion-event))
  (with-slots (first-point-x first-point-y canvas-pixmap) pane
      (when (and first-point-x first-point-y canvas-pixmap)
	(let* ((x (pointer-event-x event))
	       (y (pointer-event-y event))
               (radius-x (- x first-point-x))
               (radius-y (- y first-point-y))
	       (pixmap-width (round (bounding-rectangle-width (sheet-region pane))))
	       (pixmap-height (round (bounding-rectangle-height (sheet-region pane)))))
	  (with-slots (clim-demo::current-color clim-demo::drawing-mode clim-demo::constrict-mode) *application-frame*
          (set-status-line (format nil "~:(~A~) from (~D,~D) to (~D,~D)~%"
                                   clim-demo::drawing-mode
                                   (round first-point-x) (round first-point-y)
                                   (round x) (round y)))
	    (with-output-recording-options (pane :record nil)
	      (copy-from-pixmap canvas-pixmap 0 0 pixmap-width pixmap-height pane 0 0)
              (when clim-demo::constrict-mode
                (let ((radius-max (max (abs radius-x) (abs radius-y))))
                  (setf radius-x (* (signum-1 radius-x) radius-max)
                        radius-y (* (signum-1 radius-y) radius-max)
                        x (+ first-point-x radius-x)
                        y (+ first-point-y radius-y))))
	      (case clim-demo::drawing-mode
		(:line
                 (when clim-demo::constrict-mode
                   (if (= (- (pointer-event-x event) first-point-x) radius-x)
                       (setf y first-point-y)
                       (setf x first-point-x)))
		 (draw-line* pane first-point-x first-point-y x y
			     :ink clim-demo::current-color :line-thickness 1))
                (:arrow
                 (when clim-demo::constrict-mode
                   (if (= (- (pointer-event-x event) first-point-x) radius-x)
                       (setf y first-point-y)
                       (setf x first-point-x)))
		 (draw-arrow* pane first-point-x first-point-y x y
                              :ink clim-demo::current-color :line-thickness 1
                              :to-head t :head-width 20 :head-length 20))
		(:rectangle
		 (draw-rectangle* pane first-point-x first-point-y x y :filled nil
				  :ink clim-demo::current-color :line-thickness 1))
                (:ellipse
                 (draw-ellipse* pane first-point-x first-point-y radius-x 0 0 radius-y :filled nil
                                :ink clim-demo::current-color :line-thickness 1)))))))))

(defmethod handle-event ((pane canvas-pane) (event pointer-button-release-event))
  (when (= (pointer-event-button event) +pointer-left-button+)
    (with-slots (first-point-x first-point-y canvas-pixmap) pane
      (let ((pixmap-width (round (bounding-rectangle-width (sheet-region pane))))
	    (pixmap-height (round (bounding-rectangle-height (sheet-region pane)))))
        (set-status-line " ")

	(when (and first-point-x first-point-y canvas-pixmap)
          (copy-from-pixmap canvas-pixmap 0 0 pixmap-width pixmap-height pane 0 0)
          (deallocate-pixmap canvas-pixmap)
          (setf canvas-pixmap nil)
	  (with-slots (clim-demo::line-style
                       clim-demo::current-color
                       clim-demo::fill-mode
                       clim-demo::constrict-mode) *application-frame*
	    (let* ((x (pointer-event-x event))
		   (y (pointer-event-y event))
		   (radius-x (- x first-point-x))
                   (radius-y (- y first-point-y)))
              (when clim-demo::constrict-mode
                (let ((radius-max (max (abs radius-x) (abs radius-y))))
                  (setf radius-x (* (signum-1 radius-x) radius-max)
                        radius-y (* (signum-1 radius-y) radius-max)
                        x (+ first-point-x radius-x)
                        y (+ first-point-y radius-y))))
	      (case (clim-demo::clim-fig-drawing-mode *application-frame*)
		(:line
                 (when clim-demo::constrict-mode
                   (if (= (- (pointer-event-x event) first-point-x) radius-x)
                       (setf y first-point-y)
                       (setf x first-point-x)))
		 (draw-line* pane first-point-x first-point-y x y
			     :ink clim-demo::current-color
                             :line-style clim-demo::line-style))
		(:arrow
                 (when clim-demo::constrict-mode
                   (if (= (- (pointer-event-x event) first-point-x) radius-x)
                       (setf y first-point-y)
                       (setf x first-point-x)))
                 (with-new-output-record (pane)
                   (draw-arrow* pane first-point-x first-point-y x y
                                :ink clim-demo::current-color
                                :line-style clim-demo::line-style
                                :to-head t :head-width 20 :head-length 20)))
		(:rectangle
		 (draw-rectangle* pane first-point-x first-point-y x y :filled clim-demo::fill-mode
				  :ink clim-demo::current-color
                                  :line-style clim-demo::line-style))
                (:ellipse
		 (draw-ellipse* pane first-point-x first-point-y radius-x 0 0 radius-y
                                :filled clim-demo::fill-mode
				:ink clim-demo::current-color :line-style clim-demo::line-style))))
	    (setf (clim-demo::clim-fig-redo-list *application-frame*) nil))
	  (setf first-point-x nil
		first-point-y nil)))))
  (when (= (pointer-event-button event) +pointer-right-button+)
    (with-slots (first-point-x first-point-y selected-object) pane
      (when selected-object
        (multiple-value-bind (old-x old-y) (output-record-position selected-object)
          (setf (output-record-position selected-object)
                (values (+ old-x (- (pointer-event-x event) first-point-x))
                        (+ old-y (- (pointer-event-y event) first-point-y)))))
        (with-output-recording-options (*standard-output* :record nil)
          (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
            (draw-rectangle* pane x1 y1 x2 y2 :ink +background-ink+))
          (replay-output-record (stream-current-output-record *standard-output*)
                                *standard-output*))
        (setf selected-object nil
              first-point-x nil
              first-point-y nil)))))

(in-package :clim-demo)

(defun clim-fig ()
  (loop for port in climi::*all-ports*
      do (destroy-port port))
  (setq climi::*all-ports* nil)
  (run-frame-top-level (make-application-frame 'clim-fig)))

(defun make-colored-button (color &key width height)
  (make-pane 'push-button-pane
	     :label " "
	     :activate-callback
             #'(lambda (gadget)
                 (setf (clim-fig-current-color (gadget-client gadget))
                       color))
	     :width width :height height
	     :normal color :pushed-and-highlighted color
	     :highlighted color))

(defun make-drawing-mode-button (label mode &key width height)
  (make-pane 'push-button-pane
	     :label label
	     :activate-callback
             #'(lambda (gadget)
                 (setf (clim-fig-drawing-mode (gadget-client gadget))
                       mode))
	     :width width :height height))

(defun make-merged-line-style (line-style &key unit thickness joint-shape cap-shape
                               (dashes nil dashes-p))
  (make-line-style :unit (or unit
                             (line-style-unit line-style))
                   :thickness (or thickness
                                 (line-style-thickness line-style))
                   :joint-shape (or joint-shape
                                    (line-style-joint-shape line-style))
                   :cap-shape (or cap-shape
                                  (line-style-cap-shape line-style))
                   :dashes (if dashes-p
                               dashes
                               (line-style-dashes line-style))))

(define-command com-exit ()
  (throw 'exit nil))

(define-command com-undo ()
  (let* ((output-history (clim-fig-output-record *application-frame*))
         (record (first (last (output-record-children output-history)))))
    (if record
        (with-output-recording-options (*standard-output* :record nil)
          (with-bounding-rectangle* (x1 y1 x2 y2) record
            (draw-rectangle* *standard-output* x1 y1 x2 y2 :ink +background-ink+)
            (delete-output-record record output-history)
            (push record (clim-fig-redo-list *application-frame*))
            (replay-output-record output-history *standard-output*
                                  (make-rectangle* x1 y1 x2 y2))))
        (beep))))

(define-command com-redo ()
  (let* ((output-history (clim-fig-output-record *application-frame*))
         (record (pop (clim-fig-redo-list *application-frame*))))
    (if record
        (with-output-recording-options (*standard-output* :record nil)
          (with-bounding-rectangle* (x1 y1 x2 y2) record
            (draw-rectangle* *standard-output* x1 y1 x2 y2 :ink +background-ink+)
            (add-output-record record output-history)
            (replay-output-record output-history *standard-output*
                                  (make-rectangle* x1 y1 x2 y2))))
        (beep))))

(define-command com-clear ()
  (setf (clim-demo::clim-fig-redo-list *application-frame*)
        (append (output-record-children (clim-fig-output-record *application-frame*))
                (clim-demo::clim-fig-redo-list *application-frame*)))
  (window-clear *standard-output*))

(make-command-table 'file-command-table
		    :errorp nil
		    :menu '(("Exit" :command com-exit)))

(make-command-table 'edit-command-table
		    :errorp nil
		    :menu '(("Undo" :command com-undo)
			    ("Redo" :command com-redo)
                            ("Clear" :command com-clear)))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("File" :menu file-command-table)
                            ("Edit" :menu edit-command-table)))

(define-application-frame clim-fig ()
  ((drawing-mode :initform :line :accessor clim-fig-drawing-mode)
   (output-record :accessor clim-fig-output-record)
   (redo-list :initform nil :accessor clim-fig-redo-list)
   (current-color :initform +black+ :accessor clim-fig-current-color)
   (line-style :initform (make-line-style) :accessor clim-fig-line-style)
   (fill-mode :initform nil :accessor clim-fig-fill-mode)
   (constrict-mode :initform nil :accessor clim-fig-constrict-mode)
   (status :initform nil :accessor clim-fig-status))
  (:panes
   (canvas (make-pane 'climi::canvas-pane))
   (menu-bar (climi::make-menu-bar 'menubar-command-table :height 25))
   (line-width-slider :slider
		      :label "Line Width"
		      :value 1
		      :min-value 1
		      :max-value 100
		      :value-changed-callback
                      #'(lambda (gadget value)
                          (declare (ignore gadget))
                          (with-slots (line-style) *application-frame*
                            (setf line-style
                                  (make-merged-line-style line-style
                                                          :thickness (round value)))))
		      :show-value-p t
                      :decimal-places 0
		      :height 50
		      :orientation :horizontal)
   (round-shape-toggle :toggle-button
                       :label "Round Cap/Joint"
                       :value nil
                       :value-changed-callback
                       #'(lambda (gadget value)
                           (declare (ignore gadget))
                           (with-slots (line-style) *application-frame*
                             (let ((cap-shape (if value
                                                  :round
                                                  :butt))
                                   (joint-shape (if value
                                                    :round
                                                    :miter)))
                               (setf line-style
                                     (make-merged-line-style line-style
                                                             :cap-shape cap-shape
                                                             :joint-shape joint-shape))))))
   (fill-mode-toggle :toggle-button
                     :label "Fill"
                     :value nil
                     :value-changed-callback
                     #'(lambda (gadget value)
                         (declare (ignore gadget))
                         (setf (clim-fig-fill-mode *application-frame*) value)))
   (constrict-toggle :toggle-button
                     :label "Constrict"
                     :value nil
                     :value-changed-callback
                     #'(lambda (gadget value)
                         (declare (ignore gadget))
                         (setf (clim-fig-constrict-mode *application-frame*) value)))

   ;; Drawing modes
   (point-button (make-drawing-mode-button "Point" :point))
   (line-button (make-drawing-mode-button "Line" :line))
   (arrow-button (make-drawing-mode-button "Arrow" :arrow))
   (rectangle-button (make-drawing-mode-button "Rectangle" :rectangle))
   (ellipse-button (make-drawing-mode-button "Ellipse" :ellipse))

   ;; Colors
   (black-button (make-colored-button +black+))
   (blue-button (make-colored-button +blue+))
   (green-button (make-colored-button +green+))
   (cyan-button (make-colored-button +cyan+))
   (red-button (make-colored-button +red+))
   (magenta-button (make-colored-button +magenta+))
   (yellow-button (make-colored-button +yellow+))
   (white-button (make-colored-button +white+))
   (turquoise-button (make-colored-button +turquoise+))
   (grey-button (make-colored-button +grey+))
   (brown-button (make-colored-button +brown+))
   (orange-button (make-colored-button +orange+))

   (undo :push-button
         :label "Undo"
         :activate-callback #'(lambda (x)
                                (declare (ignore x))
                                (com-undo)))
   (redo :push-button
         :label "Redo"
         :activate-callback #'(lambda (x)
                                (declare (ignore x))
                                (com-redo)))
   (clear :push-button
          :label "Clear"
          :activate-callback #'(lambda (x)
                                 (declare (ignore x))
                                 (com-clear)))
   (status :text-field :value "CLIM Fig"))
  (:layouts
   (default
     (vertically ()
       menu-bar
       (horizontally ()
         (vertically (:width 150)
           (tabling (:height 60)
             (list black-button blue-button green-button cyan-button)
             (list red-button magenta-button yellow-button white-button)
             (list turquoise-button grey-button brown-button orange-button))
           line-width-slider
           round-shape-toggle
           (horizontally () fill-mode-toggle constrict-toggle)
           point-button line-button arrow-button
           ellipse-button rectangle-button)
         (scrolling (:width 600 :height 400) canvas))
       (horizontally (:height 30) clear undo redo)
       status)))
   (:top-level (clim-fig-frame-top-level)))

(defmethod clim-fig-frame-top-level ((frame application-frame) &key)
  (let ((*standard-input* (frame-standard-input frame))
	(*standard-output* (frame-standard-output frame))
	(*query-io* (frame-query-io frame)))
    (setf (slot-value frame 'output-record)
          (stream-current-output-record *standard-output*)
          (slot-value frame 'status)
          (find-if #'(lambda (pane) (typep pane 'text-field-pane))
                   (frame-panes frame)))
    (catch 'exit
      (loop (read-command (frame-pane frame))))
    (destroy-port (climi::port frame))))
