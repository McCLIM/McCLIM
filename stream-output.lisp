;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

;;; Note: in the methods defined on output streams, I often use
;;;	  the sheet's medium as the argument to the draw-* routines.
;;;	  This is so that they don't get recorded if the stream also
;;;	  happens to be an output-recording-stream. - MikeMac 1/7/99

;;; Standard-Output-Stream class

(defclass standard-output-stream (fundamental-character-output-stream)
  ()
  )

(defmethod stream-recording-p ((stream t)) nil)

(defmethod stream-drawing-p ((stream t)) t)

#+ignore(defmethod stream-write-char ((stream standard-output-stream) char)
  (multiple-value-bind (cx cy) (stream-cursor-position stream)
    (cond
     ((eq char #\Newline)
      (setf (stream-cursor-position stream)
            (value 0
                   (+ cy
                      (stream-line-height stream)
                      (stream-vertical-spacing stream)))))
     (t
      (draw-text* (sheet-medium stream) char cx (+ cy (stream-baseline stream)))
      (setf (stream-cursor-position stream)
            (values (+ cx (stream-character-width stream char)) cy))))))



;;; Cursor class

(defclass cursor ()
  ((sheet :initarg :sheet
	  :reader cursor-sheet)
   (x :initform 0)
   (y :initform 0)
   (width :initform 8)
   (visibility :initform nil
	       :accessor cursor-visibility)
   )
  )

(defun cursorp (x)
  (typep x 'cursor))

(defmethod print-object ((cursor cursor) stream)
  (with-slots (x y) cursor
    (print-unreadable-object (cursor stream :type t :identity t)
      (format stream "~D ~D " x y))))

(defmethod (setf cursor-visibility) :after (nv (cursor cursor))
  (if nv
      (display-cursor cursor :draw)
    (display-cursor cursor :erase)))

(defmethod cursor-position ((cursor cursor))
  (with-slots (x y) cursor
    (values x y)))

(defgeneric* (setf cursor-position) (nx ny cursor))

(defmethod* (setf cursor-position) (nx ny (cursor cursor))
  (with-slots (x y visibility) cursor
    (if visibility
	(display-cursor cursor :erase))
    (setq x nx
	  y ny)
    (if visibility
	(display-cursor cursor :draw))))

(defmethod display-cursor ((cursor cursor) state)
  (with-slots (x y sheet width) cursor
    (with-slots (height) sheet
      (case state
	(:draw (draw-rectangle* (sheet-medium (cursor-sheet cursor))
				x y
				(+ x width) (+ y height)
				:filled t
				:ink +foreground-ink+
				))
	(:erase (draw-rectangle* (sheet-medium (cursor-sheet cursor))
				 x y
				 (+ x width) (+ y height)
				 :filled t
				 :ink +background-ink+))))))

;;; Standard-Text-Cursor class

(defclass standard-text-cursor (cursor)
  (
   )
  )



;;; Extended-Output-Stream class

(defclass extended-output-stream (standard-output-stream)
  ((cursor :accessor stream-text-cursor)
   (foreground :initarg :foreground
	       :initform +black+
	       :reader stream-foreground)
   (background :initarg :background
	       :initform +white+
	       :reader stream-background)
   (text-style :initarg :text-style
	       :initform (make-text-style :fix :roman :normal)
	       :reader stream-text-style)
   (vspace :initarg :vertical-spacing
	   :initform 2
	   :reader stream-vertical-spacing)
   (margin :initarg :text-margin
	   :initform nil
	   :writer (setf stream-text-margin))
   (eol :initarg :end-of-line-action
	:initform :wrap
	:accessor stream-end-of-line-action)
   (eop :initarg :end-of-page-action
	:initform :scroll
	:accessor stream-end-of-page-action)
   (view :initarg :default-view
	 :initform nil
	 :reader stream-default-view)
   (baseline :initform 0
	     :reader stream-baseline)
   (height :initform 0)
   )
  )

(defun extended-output-stream-p (x)
  (typep x 'extended-output-stream))

(defmethod initialize-instance :after ((stream extended-output-stream) &rest args)
  (declare (ignore args))
  (setf (stream-text-cursor stream) (make-instance 'standard-text-cursor :sheet stream)))

(defmethod stream-cursor-position ((stream extended-output-stream))
  (cursor-position (stream-text-cursor stream)))

(defgeneric* (setf stream-cursor-position) (x y stream))

(defmethod* (setf stream-cursor-position) (x y (stream extended-output-stream))
  (setf (cursor-position (stream-text-cursor stream)) (values x y)))

(defmethod direct-setf*-stream-cursor-position (x y (stream extended-output-stream))
  ;; This method is used for updating cursor position after output to
  ;; the stream.
  (setf (cursor-position (stream-text-cursor stream)) (values x y)))

(defmethod stream-increment-cursor-position ((stream extended-output-stream) dx dy)
  (multiple-value-bind (x y) (cursor-position (stream-text-cursor stream))
    (setf (cursor-position (stream-text-cursor stream)) (values (+ x dx) (+ y dy)))))

(defmethod scroll-vertical ((stream extended-output-stream) dy)
  (multiple-value-bind (tx ty) (bounding-rectangle-position (sheet-region stream))
    (scroll-extent stream tx (+ ty dy))))

(defmethod scroll-horizontal ((stream extended-output-stream) dx)
  (multiple-value-bind (tx ty) (bounding-rectangle-position (sheet-region stream))
    (scroll-extent stream (+ tx dx) ty)))

(defmacro with-cursor-off (stream &body body)
  `(let* ((cursor (stream-text-cursor ,stream))
	  (visible (cursor-visibility cursor)))
     (unwind-protect
	 (progn
	   (if visible
	       (setf (cursor-visibility cursor) nil))
	   ,@body)
       (if visible
	   (setf (cursor-visibility cursor) t)))))

(defmethod stream-wrap-line ((stream extended-output-stream))
  (let ((margin (stream-text-margin stream)))
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
      (declare (ignore cx))
      (draw-rectangle* (sheet-medium stream) margin cy (+ margin 4) (+ cy (slot-value stream 'height))
		       :ink +foreground-ink+
		       :filled t)))
  (stream-write-char stream #\newline))

(defgeneric stream-write-line (stream line)
  (:documentation
   "Writes the string LINE to STREAM. This function produces no more
than one line of output."))
(defmethod stream-write-line (stream line)
  (with-slots (baseline vspace) stream
     (multiple-value-bind (cx cy) (stream-cursor-position stream)
       (draw-text* (sheet-medium stream) line
                   cx (+ cy baseline vspace)))))

(defmethod stream-write-char ((stream extended-output-stream) char)
  (let* ((cursor (stream-text-cursor stream))
	 (visible (cursor-visibility cursor))
	 (medium (sheet-medium stream))
	 (port (port stream))
	 (text-style (medium-text-style medium))
	 (new-baseline (text-style-ascent text-style medium))
	 (new-height (text-style-height text-style medium))
	 (margin (stream-text-margin stream))
	 (view-height (port-mirror-height port stream)))
    (if visible
	(setf (cursor-visibility cursor) nil))
    (with-slots (baseline height vspace) stream
      (multiple-value-bind (cx cy) (stream-cursor-position stream)
	(when (> new-baseline baseline)
	  (when (or (> baseline 0)
		    (> height 0))
	    (scroll-vertical stream (- new-baseline baseline))
	    )
	  (setq baseline new-baseline))
	(if (> new-height height)
	    (setq height new-height))
	(cond
	 ((eq char #\Newline)
	  (setq cx 0
		cy (+ cy height vspace))
	  (when (> (+ cy height vspace) view-height)
	    (ecase (stream-end-of-page-action stream)
	      (:scroll
	       (scroll-vertical stream (+ height vspace)))
	      (:wrap
	       (setq cy 0))
	      (:allow
	       )))
	  (draw-rectangle* medium cx cy (+ margin 4) (+ cy height)
			   :ink +background-ink+
			   :filled t)
	  (setq baseline 0
		height 0)
	  (setf (stream-cursor-position stream) (values cx cy)))
	 (t
	  (let ((width (stream-character-width stream char :text-style text-style)))
	    (when (>= (+ cx width) margin)
	      (ecase (stream-end-of-line-action stream)
		(:wrap
		 (let ((current-baseline baseline))
		   (stream-wrap-line stream)
		   (multiple-value-bind (new-cx new-cy) (stream-cursor-position stream)
		     (setq cx new-cx
			   cy new-cy
			   baseline current-baseline)
                     (setf (stream-cursor-position stream) (values cx cy)))))
		(:scroll
		 (scroll-horizontal stream width))
		(:allow
		 )))
	    (stream-write-line stream (string char))
	    (setq cx (+ cx width))
	    (direct-setf*-stream-cursor-position cx cy stream))))))
    (if visible
	(setf (cursor-visibility cursor) t))))

(defmethod stream-write-string ((stream extended-output-stream) string
				&optional (start 0) end)
  (if (null end)
      (setq end (length string)))
  (with-cursor-off stream
      (loop for i from start below end
	    for char = (aref string i)
	    do (stream-write-char stream char))))

;(defmethod stream-write-string ((stream extended-output-stream) string
;				&optional (start 0) end)
;  (if (null end)
;      (setq end (length string)))
;  (with-room-for-line
;      (loop for i from start below end
;	    for char = (aref string i)
;	    do (do-char))))

(defmethod stream-character-width ((stream extended-output-stream) char &key (text-style nil))
  (port-character-width (port stream)
			(or text-style (medium-text-style (sheet-medium stream)))
			char))

(defmethod stream-string-width ((stream extended-output-stream) string
				&key (start 0) (end nil) (text-style nil))
  (if (null text-style)
      (setq text-style (medium-text-style (sheet-medium stream))))
  (cond
   ((stringp string)
    (if (null end)
	(setq end (length string)))
    (loop for i from start below end
	  for char = (aref string i)
	  sum (or (stream-character-width stream char :text-style text-style)
		  0) into line-width
	  if (eql char #\Newline)
	  maximize line-width into max-line-width
	  finally (return (values line-width max-line-width))))
   (t
    (let ((width (stream-character-width stream string :text-style text-style)))
      (values width width)))))

(defmethod stream-text-margin ((stream extended-output-stream))
  (with-slots (margin) stream
    (or margin
	(- (port-mirror-width (port stream) (or (stream-default-view stream) stream))
	   6))))

(defmethod stream-line-height ((stream extended-output-stream) &key (text-style nil))
  (port-line-height (port stream) (or text-style (medium-text-style (sheet-medium stream)))))

(defmethod stream-line-column ((stream extended-output-stream))
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (declare (ignore y))
    (floor x (stream-string-width stream " "))))

(defmethod stream-start-line-p ((stream extended-output-stream))
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (zerop x)))

(defmacro with-room-for-graphics ((&optional (stream t)
				   &key (first-quadrant t) height (move-cursor t) record-type)
				  &body body)
  (declare (ignore first-quadrant height move-cursor record-type body)
           (type symbol stream))
  (when (eq stream t)
    (setq stream '*standard-output*))
  '(error "WITH-ROOM-FOR-GRAPHICS not implemented!"))

(defmacro with-end-of-line-action ((stream action) &body body)
  (declare (type symbol stream))
  (when (eq stream t)
    (setq stream '*standard-output*))
  (let ((sym (gensym)))
    `(let ((,sym (stream-end-of-line-action ,stream)))
       (setf (stream-end-of-line-action ,stream) ,action)
       (unwind-protect
	   (progn ,@body)
	 (setf (stream-end-of-line-action ,stream) ,sym)))))

(defmacro with-end-of-page-action ((stream action) &body body)
  (declare (type symbol stream))
  (when (eq stream t)
    (setq stream '*standard-output*))
  (let ((sym (gensym)))
    `(let ((,sym (stream-end-of-page-action ,stream)))
       (setf (stream-end-of-page-action ,stream) ,action)
       (prog1
	   (block ,@body)
	 (setf (stream-end-of-page-action ,stream) ,sym)))))

(defmethod beep (&optional medium)
  (when (null medium)
    (setq medium (sheet-medium *standard-output*)))
  (medium-beep medium))
