;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-
;; $fiHeader: bitmap-editor.lisp,v 1.16 1993/07/27 01:45:10 colin Exp $

(in-package :clim-demo)

(define-application-frame bitmap-editor ()
    ((rows :initarg :rows :initform 8)
     (cell-size :initarg :cell-size :initform 10)
     (columns :initarg :columns :initform 8)
     (array :initarg :array :initform nil)
     (current-color :initarg :current-color :initform 1)
     (colors :initarg :colors
	     :initform (list +white+ +black+)))
  (:panes
    (palette :accept-values
	     :width :compute :height :compute
	     :scroll-bars :vertical
	     :display-function '(accept-values-pane-displayer 
				  :displayer display-palette))
    (edit-pane :application
	       :scroll-bars :both
	       :width :compute :height :compute
	       :initial-cursor-visibility nil)
    (pattern-pane :application
		  :scroll-bars nil
		  :width :compute :height :compute
		  :initial-cursor-visibility nil))
  (:layouts 
    (default (horizontally ()
	       palette edit-pane pattern-pane))))

(defmethod initialize-instance :after ((frame bitmap-editor) &key)
  (with-slots (rows columns array) frame
    (setf array (make-array (list rows columns) :initial-element 0))))

(defun display-palette (frame stream)
  (with-slots (colors current-color) frame
    (flet ((display-color (object stream)
	     (with-room-for-graphics (stream)
	       (draw-rectangle* stream 0 0 30 10 :ink object))))
      (formatting-item-list (stream :n-columns 2 :x-spacing 30)
	(formatting-cell (stream)
	  (setf current-color
	    (position
	     (accept `((completion ,colors)
		       :name-key ,#'identity
		       :printer ,#'display-color)
		     :view '(radio-box-view 
			     :orientation :vertical
			     :toggle-button-options (:indicator-type nil))
		     :stream stream
		     :default (nth current-color colors)
		     :query-identifier 'colors
		     :prompt "Colors")
	     colors)))
	(formatting-cell (stream)
	  (formatting-item-list (stream :n-columns 1 :y-spacing 10)
	    (formatting-cell (stream)
	      (accept-values-command-button (stream)
		"Add color"
		(add-new-color frame)))
	    (formatting-cell (stream)
	      (accept-values-command-button (stream)
		"Edit Color"
		(replace-current-color frame)))
	    (formatting-cell (stream)
	      (accept-values-command-button (stream)
		"Delete Color"
		(delete-current-color frame)))))))))


(defun replace-current-color (frame)
  (declare (ignore frame))
  ;;--- Exercise for the reader
  )

(defun delete-current-color (frame)
  (declare (ignore frame))
  ;;--- Exercise for the reader
  )

(defun add-new-color (frame)
  (let ((fr (make-application-frame 'color-chooser)))
    (run-frame-top-level fr)
    (with-slots (colors current-color) frame
      (setq current-color (length colors)
	    colors (append colors (list (frame-color fr)))))))


(define-bitmap-editor-command (com-choose-options :menu t)
    ()
  (let* ((stream (frame-standard-input *application-frame*))
	 (frame *application-frame*)
	 (rows (slot-value frame 'rows))
	 (columns (slot-value frame 'columns))
	 (cell-size (slot-value frame 'cell-size))
	 (view '(slider-view :show-value-p t)))
    (accepting-values (stream :own-window t :label "Editor options")
      (setq rows (accept '(integer 1 256) 
			 :view view
			 :default rows
			 :prompt "Rows"
			 :stream stream))
      (terpri stream)
      (setq columns (accept '(integer 1 256) 
			    :view view
			    :default columns
			    :prompt "Columns"
			    :stream stream))
      (terpri stream)
      (setq cell-size (accept '(integer 10 100) 
			      :view view
			      :default cell-size
			      :prompt "Cell Size"
			      :stream stream))
      (terpri stream))
    (setf (slot-value frame 'rows) rows
	  (slot-value frame 'columns) columns
	  (slot-value frame 'cell-size) cell-size
	  (slot-value frame 'array)
	  (adjust-array (slot-value frame 'array) (list rows columns)
			:initial-element 0))
    (display-everything frame)))
    
(define-bitmap-editor-command (com-bitmap-editor-quit :menu "Quit") ()
  (frame-exit *application-frame*))
    
(defmethod display-grid (frame pane)
  (with-slots (rows columns cell-size) frame
    (let ((maxx (* rows (1+ cell-size)))
	  (maxy (* columns (1+ cell-size))))
      (dotimes (i (1+ rows))
	(draw-line* pane 0 (* i (1+ cell-size)) maxx (* i (1+ cell-size))))
      (dotimes (i (1+ columns))
	(draw-line* pane (* i (1+ cell-size)) 0 (* i (1+ cell-size)) maxy)))))

(defmethod display-cells (frame pane)
  (with-slots (rows columns cell-size) frame
    (dotimes (i rows)
      (dotimes (j columns)
	(display-cell frame pane i j)))))

(define-presentation-type bitmap-editor-cell ())

(define-presentation-method presentation-typep (object (type bitmap-editor-cell))
  (and (listp object) (= 2 (length object))))

(defun display-cell (frame pane i j)
  (with-slots (cell-size array colors) frame
    (let ((x (* j (1+ cell-size)))
	  (y (* i (1+ cell-size))))
      (with-output-as-presentation (pane (list i j) 'bitmap-editor-cell)
	(draw-rectangle* pane (+ x 2) 
			 (+ y 2)
			 (+ x (- cell-size 1))
			 (+ y (- cell-size 1))
			 :ink (nth  (aref array i j) colors)
			 :filled t)))))

(defun display-pattern (frame)
  (let ((stream (get-frame-pane frame 'pattern-pane)))
    (with-slots (array rows columns colors) frame
      (window-clear stream)
      (surrounding-output-with-border (stream)
	(draw-rectangle* stream 10 10 (+ 10 rows) (+ 10 columns)
			 :ink (make-pattern array colors))))))

(defmethod run-frame-top-level :before ((frame bitmap-editor) &key)
  (display-everything frame))

(defun display-everything (frame)
  (let ((stream (get-frame-pane frame 'edit-pane)))
    (window-clear stream)
    (display-grid frame stream)
    (display-cells frame stream)
    (display-pattern frame)))

(define-bitmap-editor-command com-toggle-cell
    ((presentation 'presentation)
     (cell 'bitmap-editor-cell))
  (let ((frame *application-frame*))
    (destructuring-bind (i j) cell
      (with-slots (array current-color) frame
	(setf (aref array i j) current-color))
      (let ((stream (get-frame-pane frame 'edit-pane)))
	(erase-output-record presentation stream)
	(display-cell frame stream i j)
	(display-pattern frame)))))

(define-presentation-to-command-translator toggle-cell
    (bitmap-editor-cell com-toggle-cell bitmap-editor 
     :gesture :select)
    (presentation object)
  (list presentation object))


(define-demo "Bitmap Editor" bitmap-editor
  :left 100 :top 100 :width 700 :height 500)

