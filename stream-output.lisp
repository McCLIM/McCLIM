;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
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

(in-package :clim-internals)

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
(define-protocol-class cursor ())

(defgeneric cursor-sheet (cursor))

(defgeneric cursor-position (cursor))

(defgeneric* (setf cursor-position) (x y cursor))

(defgeneric cursor-active (cursor))
(defgeneric (setf cursor-active) (value cursor))

(defgeneric cursor-state (cursor))
(defgeneric (setf cursor-state) (value cursor))

(defgeneric cursor-focus (cursor))

(defgeneric cursor-visibility (cursor))
(defgeneric (setf cursor-visibility) (visibility cursor))

;;; Cursor-Mixin class
(defclass cursor-mixin ()
  ((sheet :initarg :sheet
	  :reader cursor-sheet)
   (x :initform 0 :initarg :x-position)
   (y :initform 0 :initarg :y-position)
   (width :initform 8)
   (visibility :initform nil
	       :accessor cursor-visibility)))

(defgeneric cursor-height (cursor))

(defmethod print-object ((cursor cursor-mixin) stream)
  (with-slots (x y) cursor
    (print-unreadable-object (cursor stream :type t :identity t)
      (format stream "~D ~D " x y))))

(defmethod (setf cursor-visibility) :around (nv (cursor cursor-mixin))
  (let ((ov (slot-value cursor 'visibility)))
    (prog1
	(call-next-method)
      (when (not (eq ov nv))
	(flip-screen-cursor cursor)))))

(defmethod cursor-position ((cursor cursor-mixin))
  (with-slots (x y) cursor
    (values x y)))

(defmethod* (setf cursor-position) (nx ny (cursor cursor-mixin))
  (with-slots (x y visibility) cursor
    (if visibility
	(flip-screen-cursor cursor))
    (multiple-value-prog1
	(setf (values x y) (values nx ny)))
    (if visibility
	(flip-screen-cursor cursor))))

(defmethod flip-screen-cursor ((cursor cursor-mixin))
  (with-slots (x y sheet width) cursor
    (let ((height (cursor-height cursor)))
      (draw-rectangle* (sheet-medium (cursor-sheet cursor))
                       x y
                       (+ x width) (+ y height)
                       :filled t
                       :ink +flipping-ink+))))


(defmethod display-cursor ((cursor cursor-mixin) state)
  (with-slots (x y sheet width) cursor
    (let ((height (cursor-height cursor)))
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

(defclass standard-text-cursor (cursor-mixin cursor)
  ())

(defmethod cursor-height ((cursor standard-text-cursor))
  (slot-value (cursor-sheet cursor) 'height))


;;; Extended-Output-Stream class

(define-protocol-class extended-output-stream
    (fundamental-character-output-stream)
  ;; CLIM Specification says that E-O-S is a subclass of
  ;; OUTPUT-STREAM, but it does not says what is it.
  ()
  )

;;; Stream text cursor protocol
(defgeneric stream-text-cursor (stream))
(defgeneric (setf stream-text-cursor) (cursor stream))

(defgeneric stream-cursor-position (stream))
(defgeneric* (setf stream-cursor-position) (x y stream))

(defgeneric stream-increment-cursor-position (stream dx dy))

;;; Text protocol
(defgeneric stream-character-width (stream character &key text-style))

(defgeneric stream-string-width (stream character &key start end text-style))

(defgeneric stream-text-margin (stream))
(defgeneric (setf stream-text-margin) (margin stream))

(defgeneric stream-line-height (stream &key text-style))

(defgeneric stream-vertical-spacing (stream))

(defgeneric stream-baseline (stream))

(defgeneric stream-end-of-line-action (stream))
(defgeneric (setf stream-end-of-line-action) (action stream))

(defgeneric stream-end-of-page-action (stream))
(defgeneric (setf stream-end-of-page-action) (action stream))

;;; Standard-Extended-Output-Stream class

(defclass standard-extended-output-stream (extended-output-stream
                                           standard-output-stream)
  ((cursor :accessor stream-text-cursor)
   (foreground :initarg :foreground
	       :initform +black+
	       :reader stream-foreground)
   (background :initarg :background
	       :initform +white+
	       :reader stream-background)
   (text-style :initarg :text-style
	       :initform *default-text-style*
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
	 :initform +textual-view+
	 :accessor stream-default-view)
   (baseline :initform 0
	     :reader stream-baseline)
   ;; What is this? --GB
   (height :initform 0)

   ;; When the stream takes part in the space alloction protocol, this
   ;; remembers our demand:
   (seos-current-width  :initform 0)
   (seos-current-height :initform 0) ))

(defmethod compose-space ((pane standard-extended-output-stream) &key width height)
  (with-slots (seos-current-width seos-current-height) pane
    (make-space-requirement :width seos-current-width
                            :height seos-current-height)))

(defmethod initialize-instance :after ((stream standard-extended-output-stream) &rest args)
  (declare (ignore args))
  (setf (stream-text-cursor stream) (make-instance 'standard-text-cursor :sheet stream)))

(defmethod stream-cursor-position ((stream standard-extended-output-stream))
  (cursor-position (stream-text-cursor stream)))

(defmethod* (setf stream-cursor-position) (x y (stream standard-extended-output-stream))
  (setf (cursor-position (stream-text-cursor stream)) (values x y)))

(defun stream-set-cursor-position (stream x y)
  (setf (stream-cursor-position stream) (values x y)))

(defmethod stream-increment-cursor-position ((stream standard-extended-output-stream) dx dy)
  (multiple-value-bind (x y) (cursor-position (stream-text-cursor stream))
    (let ((dx (or dx 0))
	  (dy (or dy 0)))
    (setf (cursor-position (stream-text-cursor stream)) (values (+ x dx) (+ y dy))))))

;;;

(defmethod handle-repaint :around ((stream standard-extended-output-stream)
                                   region)
  (declare (ignorable region))
  (let ((cursor (stream-text-cursor stream)))
    (if (cursor-visibility cursor)
	(progn
	  ;; Erase the cursor so that the subsequent flip operation will make a
	  ;; cursor, whether or not the next-method erases the location of the
	  ;; cursor.
	  ;; XXX clip to region?  No one else seems to...
          ;; Sure clip to region! --GB
	  (display-cursor cursor :erase)
	  (call-next-method)
	  (flip-screen-cursor cursor))
	(call-next-method))))

(defmethod scroll-vertical ((stream standard-extended-output-stream) dy)
  (multiple-value-bind (tx ty) (bounding-rectangle-position (sheet-region stream))
    (scroll-extent stream tx (+ ty dy))))

(defmethod scroll-horizontal ((stream standard-extended-output-stream) dx)
  (multiple-value-bind (tx ty) (bounding-rectangle-position (sheet-region stream))
    (scroll-extent stream (+ tx dx) ty)))

(defmacro with-cursor-off (stream &body body)
  `(letf (((cursor-visibility (stream-text-cursor ,stream)) nil))
     ,@body))

(defmethod stream-wrap-line ((stream standard-extended-output-stream))
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
                   cx (+ cy baseline)))))

(defvar *inhibit-record-closing* nil
  "This is bound by things like stream-write-char to inhibit gratuitous
calls to stream-close-text-output-record during cursor movement. This is
necessary to build output records with more than one character in them.") 

(defmethod stream-write-char ((stream standard-extended-output-stream) char)
  (let* ((cursor       (stream-text-cursor stream))
	 (visible      (cursor-visibility cursor))
	 (medium       (sheet-medium stream))
	 (port         (port stream))
	 (text-style   (medium-text-style medium))
	 (new-baseline (text-style-ascent text-style medium))
	 (new-height   (text-style-height text-style medium))
	 (margin       (stream-text-margin stream))
         (%view-height (bounding-rectangle-height
                        (or (pane-viewport stream)
                            stream)))
	 (view-height  (bounding-rectangle-height
                        stream)))
    (if visible
	(setf (cursor-visibility cursor) nil))
    (with-slots (baseline height vspace) stream
      (multiple-value-bind (cx cy) (stream-cursor-position stream)
	(when (> new-baseline baseline)
          ;;(when (or (> baseline 0)
          ;;          (> height 0))
          ;;  (scroll-vertical stream (- new-baseline baseline))
          ;;  ) ; the beginning of the line should be moved down, but not the whole stream -- APD, 2002-06-18
	  (setq baseline new-baseline))
	(if (> new-height height)
	    (setq height new-height))
	(cond
	 ((eq char #\Newline)
	  (setq cx 0
		cy (+ cy height vspace))
	  (when (> (+ cy height) view-height)
	    (ecase (stream-end-of-page-action stream)
	      ((:scroll :allow)
               (let ((jump 0))
                 (with-slots (seos-current-width seos-current-height) stream
                   (setf seos-current-width  (max (bounding-rectangle-width stream))
                         seos-current-height (max (+ cy height)))
                   (change-space-requirements stream
                                              :width seos-current-width
                                              :height seos-current-height))
                 )
	       ;;(scroll-vertical stream (+ height vspace))
               )
	      (:wrap
	       (setq cy 0))))
          (unless (eq :allow (stream-end-of-page-action stream))
	      (scroll-extent stream 0 (max 0 (- (+ cy height) %view-height))))
	    
	  
	  ;; mikemac says that this "erase the new line" behavior is
	  ;; required by the stream text protocol, but I don't see
	  ;; it.  I'm happy to put this back in again, but in the
	  ;; meantime it makes debugging of updating-output a bit easier
	  ;; not to have "extra" records laying around.  If/When it goes
	  ;; back in... the draw-rectangle has to happen on the stream,
	  ;; not the medium. -- moore
	  #+nil(draw-rectangle* medium cx cy (+ margin 4) (+ cy height)
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
	    (let ((*inhibit-record-closing* T))
	      (setf (stream-cursor-position stream) (values cx cy))))))))
    (if visible
	(setf (cursor-visibility cursor) t))))

(defmethod stream-write-string ((stream standard-extended-output-stream) string
				&optional (start 0) end)
  (if (null end)
      (setq end (length string)))
  (with-cursor-off stream
      (loop for i from start below end
	    for char = (aref string i)
	    do (stream-write-char stream char))))

;(defmethod stream-write-string ((stream standard-extended-output-stream) string
;				&optional (start 0) end)
;  (if (null end)
;      (setq end (length string)))
;  (with-room-for-line
;      (loop for i from start below end
;	    for char = (aref string i)
;	    do (do-char))))

(defmethod stream-character-width ((stream standard-extended-output-stream) char &key (text-style nil))
  (with-sheet-medium (medium stream)
    (text-style-character-width (or text-style (medium-text-style medium))
                                medium
                                char)))

(defmethod stream-string-width ((stream standard-extended-output-stream) string
				&key (start 0) (end nil) (text-style nil))
  (with-sheet-medium (medium stream)
    (if (null text-style)
        (setq text-style (medium-text-style (sheet-medium stream))))
    (multiple-value-bind (total-width total-height final-x final-y baseline)
        (text-size medium string :text-style text-style
                   :start start :end end)
      (declare (ignore total-height final-y baseline))
      (values final-x total-width))))

(defmethod stream-text-margin ((stream standard-extended-output-stream))
  (with-slots (margin) stream
    (or margin
	(- (bounding-rectangle-width (or (pane-viewport stream)
                                         stream))
	   6))))

(defmethod stream-line-height ((stream standard-extended-output-stream) &key (text-style nil))
  (+ (text-style-height (or text-style (medium-text-style (sheet-medium stream)))
			(sheet-medium stream))
     (stream-vertical-spacing stream)))

(defmethod stream-line-column ((stream standard-extended-output-stream))
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (declare (ignore y))
    (floor x (stream-string-width stream " "))))

(defmethod stream-start-line-p ((stream standard-extended-output-stream))
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (declare (ignore y))
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
  (when (eq stream t)
    (setq stream '*standard-output*))
  (check-type stream symbol)
  `(letf (((stream-end-of-line-action ,stream) ,action))
     ,@body))

(defmacro with-end-of-page-action ((stream action) &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (check-type stream symbol)
  `(letf (((stream-end-of-page-action ,stream) ,action))
     ,@body))

(defmethod beep (&optional medium)
  (when (null medium)
    (setq medium (sheet-medium *standard-output*)))
  (medium-beep medium))
