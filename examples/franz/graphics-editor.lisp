;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-GRAPHICS-EDITOR; Base: 10; Lowercase: Yes -*-

;; $fiHeader: graphics-editor.lisp,v 1.23 1993/10/26 03:21:57 colin Exp $

(in-package :clim-graphics-editor)

"Copyright (c) 1992 Symbolics, Inc.  All rights reserved."


;;; Define a "mix-in" frame class that manages a selected object

;;--- This facility is not part of CLIM itself, but is part of a
;;--- graphical editing system that I have been developing privately,
;;--- which I call Zdrava. --SWM

(define-application-frame selected-object-mixin ()
    ((selected-object :accessor frame-selected-object
		      :initform nil))
  (:command-definer nil))

(defmethod object-selected-p ((frame selected-object-mixin) object)
  (eql object (frame-selected-object frame)))

;; Deselect the current object and select the new one.  Tick the redisplay
;; tick to force the handles to be displayed.
(defmethod select-object ((frame selected-object-mixin) object)
  (when (frame-selected-object frame)
    (deselect-object frame (frame-selected-object frame)))
  (setf (frame-selected-object frame) object)
  (tick-object object))

;; Deselect the current object and tick the redisplay tick.
(defmethod deselect-object ((frame selected-object-mixin) object)
  (setf (frame-selected-object frame) nil)
  (tick-object object))


;;; Define the basic object handle class

;;--- This facility is part of Zdrava.

(defclass object-handle (standard-point)
    ((object :initarg :object :reader handle-object)
     (type :initarg :type :reader handle-type)))

;; Moving a handle causes tha object it is attached to to be reshaped.
(defmethod move-handle ((handle object-handle) x y)
  (setf (point-x handle) x)
  (setf (point-y handle) y)
  (reshape-object (handle-object handle) x y (handle-type handle)))


;;; Define the basic object classes

;;--- This facility is part of Zdrava.

(defclass basic-object (standard-bounding-rectangle)
    ((style :accessor object-style	;all objects have a line style
	    :initarg :style)
     (redisplay-tick :initform 0)))	;redisplay when this changes

;; Ticking the object causes it to be redisplayed
(defmethod tick-object ((object basic-object))
  (incf (slot-value object 'redisplay-tick)))

;; All objects participate in redisplay...
(defmethod draw-object :around ((object basic-object) stream)
  (updating-output (stream :unique-id object
			   :cache-value (slot-value object 'redisplay-tick))
    (call-next-method)
    (when (object-selected-p *application-frame* object)
      (draw-object-handles object stream))))

(defmethod draw-object ((object basic-object) stream)
  (declare (ignore stream ink)))

(defmethod draw-object-handles ((object basic-object) stream)
  (declare (ignore stream ink)))

;; For simplicity, moving an object entails moving its bounding box.
(defmethod move-object ((object basic-object) x y)
  (multiple-value-bind (width height) (bounding-rectangle-size object)
    (clim-utils:bounding-rectangle-set-edges object x y (+ x width) (+ y height))))

;; Moving (or reshaping) requires redisplay
(defmethod move-object :after ((object basic-object) x y)
  (declare (ignore x y))
  (tick-object object))

(defmethod reshape-object :after ((object basic-object) x y type)
  (declare (ignore x y type))
  (tick-object object))


(defclass object-with-handles-mixin ()
    ((handles :initform nil)))

(defmethod object-handles ((object object-with-handles-mixin))
  (when (null (slot-value object 'handles))
    (setf (slot-value object 'handles) (compute-object-handles object)))
  (slot-value object 'handles))

(defun make-handle (object x y type)
  (make-instance 'object-handle :x x :y y :object object :type type))

(defmethod draw-object-handles ((object object-with-handles-mixin) stream)
  (dolist (handle (object-handles object))
    (multiple-value-bind (x y) (point-position handle)
      ;; Present the object as an OBJECT-HANDLE so that commands in the
      ;; application can be written using that presentation type.
      (with-output-as-presentation (stream handle 'object-handle)
	(draw-rectangle* stream (- x 2) (- y 2) (+ x 2) (+ y 2)
			 :filled t)))))

;; Moving (or reshaping) an object changes its handle locations
(defmethod move-object :after ((object object-with-handles-mixin) x y)
  (declare (ignore x y))
  (setf (slot-value object 'handles) nil))

(defmethod reshape-object :after ((object object-with-handles-mixin) x y type)
  (declare (ignore x y type))
  (setf (slot-value object 'handles) nil))


;;; Define the box and arrow classes

;;--- Using Zdrava, a programmer would build these classes out of the
;;--- classes supplied by Zdrava.

(defclass box (object-with-handles-mixin basic-object)
    ((label :initarg :label :reader box-label)
     (arrow-in :initform nil :accessor box-arrow-in)
     (arrow-out :initform nil :accessor box-arrow-out)
     (shape :initarg :shape :accessor box-shape)))

(defun make-box (left top right bottom label style &optional (shape :rectangle))
  (make-instance 'box :left left :top top :right right :bottom bottom
		      :label label :style style :shape shape))

(defmethod compute-object-handles ((object box))
  (with-bounding-rectangle* (left top right bottom) object
    (list (make-handle object left  top :nw)
	  (make-handle object right top :ne)
	  (make-handle object left  bottom :sw)
	  (make-handle object right bottom :se))))

(defmethod draw-object ((object box) stream)
  (with-bounding-rectangle* (left top right bottom) object
    ;; Present the object as a BOX so that commands in the application
    ;; can be written using that presentation type.
    (with-output-as-presentation (stream object 'box :single-box t)
      (ecase (box-shape object)
	(:oval
	  (draw-oval* stream
		      (/ (+ left right) 2) (/ (+ top bottom) 2)
		      (/ (abs (- right left)) 2) (/ (abs (- bottom top)) 2)
		      :filled nil :line-style (object-style object)))
	(:rectangle
	  (draw-rectangle* stream left top right bottom
			   :filled nil :line-style (object-style object))))
      (draw-text* stream
		  (box-label object) (+ left (floor (- right left) 2))
		  (+ top 2) :align-x :center :align-y :top))))

#+allegro
(define-presentation-method highlight-presentation ((type box) record stream state)
  (declare (ignore state))
  (multiple-value-bind (xoff yoff)
      (convert-from-relative-to-absolute-coordinates
	stream (output-record-parent record))
    (with-bounding-rectangle* (left top right bottom) record
      (draw-rectangle* stream
		       (+ xoff (- left 3)) (+ yoff (- top 3))
		       (+ xoff right 3) (+ yoff bottom 3)
		       :ink (make-flipping-ink
			      (if (palette-color-p
				    (frame-palette (pane-frame stream)))
				  +red+
				  +foreground-ink+)
			      +background-ink+)
		       :line-style (make-line-style :thickness 2)
		       :filled nil))))

(defmethod move-object :after ((object box) x y)
  (declare (ignore x y))
  (when (box-arrow-in object)
    (tick-object (box-arrow-in object)))
  (when (box-arrow-out object)
    (tick-object (box-arrow-out object))))

(defmethod reshape-object ((object box) x y type)
  (with-bounding-rectangle* (left top right bottom) object
    (ecase type
      (:nw (setq left x
		 top y))
      (:ne (setq right x
		 top y))
      (:sw (setq left x
		 bottom y))
      (:se (setq right x
		 bottom y)))
    (clim-utils:bounding-rectangle-set-edges object left top right bottom)))

(defmethod reshape-object :after ((object box) x y type)
  (declare (ignore x y type))
  (when (box-arrow-in object)
    (tick-object (box-arrow-in object)))
  (when (box-arrow-out object)
    (tick-object (box-arrow-out object))))


(defclass arrow (object-with-handles-mixin basic-object)
    ((box1 :initarg :box1)
     (box2 :initarg :box2)))

(defun make-arrow (box1 box2 style)
  (let ((arrow (make-instance 'arrow :box1 box1 :box2 box2
			      :style style)))
    (setf (box-arrow-out box1) arrow)
    (setf (box-arrow-in box2) arrow)
    arrow))

(defmethod draw-object ((object arrow) stream)
  (with-slots (box1 box2) object
    (multiple-value-bind (x1 y1) (bounding-rectangle-center* box1)
      (multiple-value-bind (x2 y2) (bounding-rectangle-center* box2)
	(with-output-as-presentation (stream object 'arrow)
	  (draw-arrow* stream x1 y1 x2 y2
		       :line-style (object-style object)))))))

(defmethod compute-object-handles ((object arrow))
  (with-slots (box1 box2) object
    (multiple-value-bind (x1 y1) (bounding-rectangle-center* box1)
      (multiple-value-bind (x2 y2) (bounding-rectangle-center* box2)
	(list (make-handle object x1 y1 nil)
	      (make-handle object x2 y2 nil))))))


;;; The application itself

(define-command-table graphics-editor-file-commands)

(define-command-table graphics-editor-edit-commands
    :menu (("Deselect" :command (com-deselect-object))
	   ("divide1" :divider nil)
	   ("Clear" :command (com-clear))
	   ("divide2" :divider nil)
	   ("Redisplay" :command (com-redisplay))))

(define-command-table graphics-editor-option-commands)

(define-command-table graphics-editor-help-commands)

(define-application-frame graphics-editor (selected-object-mixin)
    ((objects :initform nil)
     (counter :initform 0)
     (last-box :initform nil)
     (style :initform (make-line-style :thickness 1 :dashes nil))
     (shape :initform :rectangle))
  (:command-definer define-graphics-editor-command)
  (:command-table (graphics-editor
		    :inherit-from (accept-values-pane
				   graphics-editor-file-commands
				   graphics-editor-edit-commands
				   graphics-editor-option-commands
				   graphics-editor-help-commands)
		    :inherit-menu :keystrokes
		    :menu (("File" :menu graphics-editor-file-commands
				   :mnemonic #\F :documentation "File Commands")
			   ("Edit" :menu graphics-editor-edit-commands
				   :mnemonic #\E :documentation "Edit Commands")
			   ("Options" :menu graphics-editor-option-commands
				      :mnemonic #\O :documentation
				      "Options")
			   ("Help" :menu graphics-editor-help-commands
				   :mnemonic #\H :documentation "Help Commands"
				   :button-type :help))))
  (:pointer-documentation t)
  ;; Three panes: a display pane, and command menu, and a modeless
  ;; dialog containing the line style options
  (:panes
    (display :application
	     :incremental-redisplay t
	     :display-after-commands t
	     :display-function 'display-objects
	     :scroll-bars :both
	     :initial-cursor-visibility nil)
    (horizontal-options :accept-values
			:min-height :compute :height :compute :max-height :compute
			:display-function
			  `(accept-values-pane-displayer
			     :displayer ,#'(lambda (frame stream)
					     (accept-graphics-editor-options
					       frame stream
					       :orientation :horizontal))))
    (vertical-options :accept-values
		      :min-width :compute :width :compute :max-width :compute
		      :display-function
		        `(accept-values-pane-displayer
			   :displayer ,#'(lambda (frame stream)
					   (accept-graphics-editor-options
					     frame stream
					     :orientation :vertical)))))
  (:layouts
   (default
       (vertically ()
	 horizontal-options
	 (:fill display)))
    (other
      (horizontally ()
	vertical-options
	(:fill display)))))

(defmethod read-frame-command ((frame graphics-editor) &key (stream *standard-input*))
  (read-command (frame-command-table frame) :use-keystrokes t :stream stream))

;;; Presentation types

(define-presentation-type line-thickness ()
    :inherit-from `((completion (1 2 3 4))
		    :name-key identity
		    :printer present-line-thickness
		    :highlighter highlight-line-thickness))

(define-presentation-method describe-presentation-type ((type line-thickness) stream plural-count)
  (declare (ignore plural-count))
  (write-string  "Specify one of four line thicknesses" stream))

(defun present-line-thickness (object stream &key acceptably)
  (declare (ignore acceptably))
  (let ((y (stream-line-height stream)))
    (with-room-for-graphics (stream)
      (draw-rectangle* stream 0 2 16 (- y 2)
		       :filled nil :ink +background-ink+)
      (draw-line* stream 0 (floor y 2) 16 (floor y 2)
		  :line-thickness object))))

(defun highlight-line-thickness (continuation object stream)
  (surrounding-output-with-border (stream)
    (funcall continuation object stream)))

(define-presentation-type line-style-type ()
    :inherit-from `((completion (:solid :dashed))
		    :name-key identity
		    :printer present-line-style
		    :highlighter highlight-line-style))

(define-presentation-method describe-presentation-type ((type line-style-type) stream plural-count)
  (declare (ignore plural-count))
  (write-string  "Specify solid or dashed line style" stream))

(defun present-line-style (object stream &key acceptably)
  (declare (ignore acceptably))
  (let ((y (stream-line-height stream)))
    (with-room-for-graphics (stream)
      (draw-rectangle* stream 0 2 16 (- y 2)
		       :filled nil :ink +background-ink+)
      (draw-line* stream 0 (floor y 2) 16 (floor y 2)
		  :line-dashes (and (eq object :dashed)
				    #(2 2))))))

(defun highlight-line-style (continuation object stream)
  (surrounding-output-with-border (stream)
    (funcall continuation object stream)))

(define-presentation-type-abbreviation object-shape ()
  '((member :oval :rectangle) :name-key identity
			      :printer present-object-shape
			      :highlighter highlight-object-shape))

(define-presentation-type object-shape ()
    :inherit-from `((completion (:oval :rectangle))
		    :name-key identity
		    :printer present-object-shape
		    :highlighter highlight-object-shape))

(define-presentation-method describe-presentation-type ((type object-shape) stream plural-count)
  (declare (ignore plural-count))
  (write-string  "Specify rectangle or oval shape" stream))

(defun present-object-shape (object stream &key acceptably)
  (declare (ignore acceptably))
  (let ((y (stream-line-height stream)))
    (multiple-value-bind (left top bottom right)
	(values 0 2 16 (- y 2))
      (with-room-for-graphics (stream)
	(ecase object
	  (:rectangle
	    (draw-rectangle* stream left top (* 2 bottom) (* 2 right) :filled nil))
	  (:oval
	    (draw-oval* stream
			(/ (+ left right) 2)
			(/ (+ top bottom) 2)
			(/ (abs (- right left)) 2)
			(/ (abs (- bottom top)) 2) :filled nil)))))))

(defun highlight-object-shape (continuation object stream)
  (surrounding-output-with-border (stream)
    (funcall continuation object stream)))

(defmethod accept-graphics-editor-options ((frame graphics-editor) stream
					   &key (orientation :horizontal))
  (with-slots (style shape) frame
    (flet ((accept (stream type default prompt query-id)
	     (let (object ptype changed)
	       (formatting-cell (stream :align-x (ecase orientation
						   (:horizontal :center)
						   (:vertical :left)))
		 (multiple-value-setq (object ptype changed)
		   (accept type
			   :stream stream :default default
			   :query-identifier query-id :prompt prompt)))
	       ptype
	       (values object changed))))
      (declare (dynamic-extent #'accept))
      (terpri stream)
      (formatting-table (stream :x-spacing '(3 :character))
	(flet ((do-body (stream)
		 (let ((thickness (line-style-thickness style))
		       (dashes (line-style-dashes style)))
		   (multiple-value-bind (thickness thickness-changed)
		       (accept stream 'line-thickness thickness
			       "Thickness" 'thickness)
		     (declare (ignore ignore))
		     (multiple-value-bind (dashes dashes-changed)
			 (accept stream 'line-style-type (if dashes :dashed :solid)
				 "Line Style" 'dashes)
		       (declare (ignore ignore))
		       (setq dashes (eq dashes :dashed))
		       (multiple-value-bind (new-shape shape-changed)
			   (accept stream 'object-shape shape
				   "Shape" 'shape)
			 (when (or thickness-changed dashes-changed shape-changed)
			   (setq style (make-line-style :thickness thickness
							:dashes dashes))
			   (setq shape new-shape)
			   (when (frame-selected-object frame)
			     (setf (object-style (frame-selected-object frame))
				   (slot-value frame 'style))
			     (setf (box-shape (frame-selected-object frame))
				   (slot-value frame 'shape))
			     (tick-object (frame-selected-object frame))
			     (redisplay-frame-pane frame 'display)))))))))
	  (ecase orientation
	    (:horizontal
	      (formatting-row (stream) (do-body stream)))
	    (:vertical
	      (formatting-column (stream) (do-body stream)))))))))

(defmethod display-objects ((frame graphics-editor) stream)
  (dolist (object (slot-value frame 'objects))
    (draw-object object stream)))

(define-graphics-editor-command com-create-box
    ((left 'integer)
     (top 'integer))
  (com-deselect-object)
  (let ((stream (get-frame-pane *application-frame* 'display))
	(right left)
	(bottom top)
	(rectangle-drawn nil)
	(box nil)
	(label (format nil "Box ~D" (slot-value *application-frame* 'counter)))
	(last-box (slot-value *application-frame* 'last-box))
	(style (slot-value *application-frame* 'style))
	(shape (slot-value *application-frame* 'shape))
	(flipping-ink #+allegro (make-flipping-ink
				  (if (palette-color-p
					(frame-palette *application-frame*))
				      +blue+
				      +foreground-ink+)
				  +background-ink+)
		      #-allegro +flipping-ink+))
    ;;--- Zdrava supplies primitives to input basic objects such as
    ;;--- points, lines, rectangles and polygons, circles and ellipses,
    ;;--- and so forth.  Using Zdrava, the following code would be a
    ;;--- replaced by 3 or 4 lines of code.
    (block track-pointer
      (with-output-recording-options (stream :draw t :record nil)
	(tracking-pointer (stream :multiple-window t)
	  (:pointer-motion (window x y)
	   (when rectangle-drawn
	     (draw-rectangle* stream left top right bottom
			      :filled nil :ink flipping-ink)
	     (setq rectangle-drawn nil))
	   (when (eql window stream)
	     (setq right x
		   bottom y)
	     (draw-rectangle* stream left top right bottom
			      :filled nil :ink flipping-ink)
	     (setq rectangle-drawn t)))
	  (:pointer-button-release (event)
	   (when (eql (event-sheet event) stream)
	     (when rectangle-drawn
	       (draw-rectangle* stream left top right bottom
				:filled nil :ink flipping-ink)
	       (setq rectangle-drawn nil))
	     ;; If the mouse didn't move very far, don't bother
	     ;; creating a box.  Just deselect the current object.
	     (when (or (> (abs (- right left)) 3)
		       (> (abs (- bottom top)) 3))
	       (multiple-value-bind (width height)
		   (text-size stream label)
		 (when (< (- right left) width)
		   (setq right (+ left width 8)))
		 (when (< (- bottom top) height)
		   (setq bottom (+ top height 4))))
	       (setq box (make-box left top right bottom label style shape))
	       (return-from track-pointer)))
	   (beep stream)
	   (return-from com-create-box)))))
    (when box
      (setf (slot-value *application-frame* 'objects)
	    (append (slot-value *application-frame* 'objects) (list box)))
      (setf (slot-value *application-frame* 'last-box) box)
      (incf (slot-value *application-frame* 'counter))
      (when last-box
	(let ((arrow (make-arrow last-box box style)))
	  (setf (slot-value *application-frame* 'objects)
		(append (slot-value *application-frame* 'objects) (list arrow))))))))

;; A mouse click over blank area creates a new box.
(define-presentation-to-command-translator create-box
    (blank-area com-create-box graphics-editor
     :gesture :select :menu nil)
    (x y)
  (list x y))

;; Select an object by clicking "select" (Mouse-Left) on it.
(define-graphics-editor-command com-select-object
    ((object 'box :gesture :select))
  (select-object *application-frame* object)
  (setf (slot-value *application-frame* 'style) (object-style object)
	(slot-value *application-frame* 'shape) (box-shape object)))

;; Deselect an object by clicking the Deselect menu button, or by
;; clicking over blank area without moving the mouse.
(define-command (com-deselect-object :command-table graphics-editor-edit-commands
				     :menu ("Deselect" :mnemonic #\D
					    :documentation "Deselect all objects"))
    ()
  (when (frame-selected-object *application-frame*)
    (deselect-object *application-frame* (frame-selected-object *application-frame*))))

;; Move an object by clicking Mouse-Middle on it and dragging the mouse.
(define-graphics-editor-command com-move-object
    ((object 'box :gesture :describe))
  (let ((stream (get-frame-pane *application-frame* 'display)))
    (with-bounding-rectangle* (left top right bottom) object
      (multiple-value-bind (x y dx dy)
	  (dragging-output (stream :repaint t :finish-on-release t)
	    ;; Use a rectangle as feedback
	    (draw-rectangle* stream left top right bottom
			     :filled nil))
	(move-object object (- x dx) (- y dy))))))

;; Delete an object by clicking "delete" (shift-Mouse-Middle) on it.
(define-graphics-editor-command com-delete-object
    ((object 'basic-object :gesture :delete))
  (delete-object *application-frame* object)
  (when (eql object (slot-value *application-frame* 'last-box))
    (setf (slot-value *application-frame* 'last-box) nil)))

(defmethod delete-object ((frame graphics-editor) (object basic-object))
  (setf (slot-value frame 'objects)
	(delete object (slot-value frame 'objects))))

(defmethod delete-object :after ((frame graphics-editor) (object box))
  (when (box-arrow-in object)
    (delete-object frame (box-arrow-in object)))
  (when (box-arrow-out object)
    (delete-object frame (box-arrow-out object))))

(defmethod delete-object :after ((frame graphics-editor) (object arrow))
  (with-slots (box1 box2) object
    (setf (box-arrow-out box1) nil)
    (setf (box-arrow-in box2) nil)))

;; Add a menu item that deletes the selected object
(add-menu-item-to-command-table 'graphics-editor "Delete"
  :function 'delete-selected-object
  :keystroke '(#\d :control))

(defun delete-selected-object (gesture numeric-arg)
  (declare (ignore gesture numeric-arg))
  (and (frame-selected-object *application-frame*)
       `(com-delete-object ,(frame-selected-object *application-frame*))))

;; Move a handle by clicking Mouse-Middle on it and dragging the mouse.
(define-graphics-editor-command com-move-handle
    ((handle 'object-handle :gesture :describe))
  (let ((stream (get-frame-pane *application-frame* 'display)))
    (multiple-value-bind (x y) (point-position handle)
      (multiple-value-bind (x y dx dy)
	  (dragging-output (stream :repaint t :finish-on-release t)
	    (draw-rectangle* stream (- x 2) (- y 2) (+ x 2) (+ y 2)
			     :filled t))
	(move-handle handle (- x dx) (- y dy))))))

;; OK, I added a menu button to clear the window.
(define-command (com-clear :command-table
			   graphics-editor-edit-commands
			   :keystroke (#\\ :control)
			   :menu ("Clear" :mnemonic #\C
				  :documentation "Clear all graphics"))
    ()
  (with-slots (objects selected-object last-box) *application-frame*
    (setq objects nil
	  selected-object nil
	  last-box nil)
    (window-clear (get-frame-pane *application-frame* 'display))))

;; OK, I added a menu button to redisplay the window, too, although
;; it's only here for debugging.
(define-command (com-redisplay :command-table graphics-editor-edit-commands
			       :keystroke (:r :meta)
			       :menu  ("Redisplay" :mnemonic #\R
						   :documentation "Redisplay windows"))
    ()
  (redisplay-frame-pane *application-frame* 'display :force-p t))

(define-command (com-quit :command-table graphics-editor-file-commands
			  :keystroke (:x :meta)
			  :menu ("Quit" :documentation "Quit application" :mnemonic #\Q)) ()
  (frame-exit *application-frame*))

(define-command (com-help :command-table graphics-editor-help-commands
			  :menu ("On Application" :mnemonic #\A
				 :documentation "Help on Application")) ()
  (notify-user *application-frame* "No Help Available"
	       :style :error))

(define-command (com-change-layout :command-table graphics-editor-option-commands
				   :keystroke (:l :meta)
				   :menu ("Change application layout" :mnemonic #\l
					  :documentation "Change layout"))
    ()
  (let* ((layouts (frame-all-layouts *application-frame*))
	 (old-layout (frame-current-layout *application-frame*))
	 (new-layout (or (second (member old-layout layouts))
			 (car layouts))))
    (setf (frame-current-layout *application-frame*)
      new-layout)))


(define-demo "Graphics Editor" graphics-editor
  :left 100 :top 100 :width 800 :height 500)
