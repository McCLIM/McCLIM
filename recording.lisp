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

(defclass output-record (standard-bounding-rectangle)
  ((x :initarg :x-position
      :initform 0)
   (y :initarg :y-position
      :initform 0)
   (parent :initarg :parent
	   :initform nil)
   (children :initform nil
	     :reader output-record-children)
   )
  (:default-initargs :min-x 0 :min-y 0 :max-x 0 :max-y 0))

(defun output-record-p (x)
  (typep x 'output-record))

(defclass displayed-output-record (output-record)
  (
   ))

(defun displayed-output-record-p (x)
  (typep x 'displayed-output-record))

(defmethod initialize-instance :after ((record displayed-output-record) &rest args
				       &key size
				       &allow-other-keys)
  (declare (ignore args size)))

(defmethod output-record-position ((record displayed-output-record))
  (with-slots (x y) record
    (values x y)))

(defmethod setf*-output-record-position (nx ny (record displayed-output-record))
  (with-slots (x y) record
    (setq x nx
	  y ny)))

(defmethod output-record-start-cursor-position ((record displayed-output-record))
  (values nil nil))

(defmethod setf*-output-record-start-cursor-position (x y (record displayed-output-record))
  (declare (ignore x y))
  nil)

(defmethod output-record-end-cursor-position ((record displayed-output-record))
  (values nil nil))

(defmethod setf*-output-record-end-cursor-position (x y (record displayed-output-record))
  (declare (ignore x y))
  nil)

(defun replay (record stream &optional region)
  (let ((old-record-p (stream-recording-p stream))
	(old-draw-p (stream-drawing-p stream)))
    (unwind-protect
	(progn
	  (setf (stream-recording-p stream) nil
		(stream-drawing-p stream) t)
	  (replay-output-record record stream region))
      (setf (stream-recording-p stream) old-record-p
	    (stream-drawing-p stream) old-draw-p))))

(defmethod replay-output-record ((record output-record) stream
				 &optional region x-offset y-offset)
  (loop for child in (output-record-children record)
	do (replay-output-record child stream region x-offset y-offset)))

(defmethod erase-output-record ((record output-record) stream)
  (declare (ignore stream))
  nil)

(defmethod output-record-hit-detection-rectangle* ((record output-record))
  (bounding-rectangle* record))

(defmethod output-record-refined-sensitivity-test ((record output-record) x y)
  (region-contains-position-p (output-record-hit-detection-rectangle* record) x y))

(defmethod highlight-output-record ((record output-record) stream state)
  (multiple-value-bind (x1 y1 x2 y2) (output-record-hit-detection-rectangle* record)
    (ecase state
      (:highlight
       (draw-rectangle* stream x1 y1 x2 y2 :filled nil :ink +foreground-ink+))
      (:unhighlight
       (draw-rectangle* stream x1 y1 x2 y2 :filled nil :ink +background-ink+)))))

(defmethod add-output-record (child (record output-record))
  (with-slots (children) record
    (push child children))
  (with-slots (parent) child
    (setf parent record)))

(defmethod delete-output-record (child (record output-record) &optional (errorp t))
  (with-slots (children) record
    (if (and errorp
	     (not (member child children)))
	(error "~S is not a child of ~S" child record))
    (setq children (delete child children))))

(defmethod clear-output-record ((record output-record))
  (with-slots (children x1 y1 x2 y2) record
    (setq children nil
	  x1 0
	  y1 0
	  x2 0
	  y2 0)))

(defmethod output-record-count ((record output-record))
  (length (output-record-children record)))

(defmethod map-over-output-records-containing-position (function (record output-record) x y
							&optional (x-offset 0) (y-offset 0))
  (declare (dynamic-extent function)
	   (ignore x-offset y-offset))
  (loop for child in (output-record-children record)
	if (region-contains-position-p (output-record-hit-detection-rectangle* child) x y)
	do (funcall function child)))

(defmethod map-over-output-records-overlaping-region (function (record output-record) region
						      &optional (x-offset 0) (y-offset 0))
  (declare (dynamic-extent function)
	   (ignore x-offset y-offset))
  (with-bounding-rectangle* (l1 t1 r1 b1) region
    (loop for child in (output-record-children record)
	  do (with-bounding-rectangle* (l2 t2 r2 b2) child
	       (if (and (<= l2 r1)
			(>= r2 l1)
			(<= b2 t1)
			(>= t2 b1))
		   (funcall function child))))))

(defmethod recompute-extent-for-new-child ((record output-record) child)
  (with-bounding-rectangle* (left top right bottom) record
    (recompute-extent-for-changed-child record child left top right bottom)))

(defmethod recompute-extent-for-changed-child ((record output-record) child
					       old-min-x old-min-y old-max-x old-max-y)
  (declare (ignore child old-min-x old-min-y old-max-x old-max-y))
  (error "I don't understand RECOMPUTE-EXTENT-FOR-CHANGED-CHILD - mikemac"))

(defmethod tree-recompute-extent ((record output-record))
  (with-slots (parent children x1 y1 x2 y2) record
    (if (null children)
	(setq x1 0
	      y1 0
	      x2 0
	      y2 0)
      (with-bounding-rectangle* (left top right bottom) (first children)
	(loop for child in (rest children)
	      do (with-bounding-rectangle* (l1 t1 r1 b1) child
		   (setq left (min left l1 r1)
			 top (min top t1 b1)
			 right (max right l1 r1)
			 bottom (max bottom t1 b1))))
	(setq x1 left
	      y1 top
	      x2 right
	      y2 bottom)))
    (if parent
	(recompute-extent-for-changed-child parent record x1 y1 x2 y2))))

(defclass standard-sequence-output-record (displayed-output-record)
  (
   ))

(defclass standard-tree-output-record (displayed-output-record)
  (
   ))


;;; Graphics recording classes

(defclass graphics-displayed-output-record (displayed-output-record)
  ((ink :initarg :ink)
   (clip :initarg :clipping-region)
   (transform :initarg :transformation)
   (line-style :initarg :line-style)
   (text-style :initarg :text-style)
   ))

(defun graphics-displayed-output-record-p (x)
  (typep x 'graphics-displayed-output-record))


;;; stream-output-history-mixin class

(defclass stream-output-history-mixin ()
  ((output-history :initform (make-instance 'standard-sequence-output-record)
		   :reader stream-output-history)
   (recording-p :initform t
		:accessor stream-recording-p)
   (drawing-p :initform t
	      :accessor stream-drawing-p)
   ))

(defmethod scroll-vertical :around ((stream stream-output-history-mixin) dy)
  (declare (ignore dy))
  (with-output-recording-options (stream :record nil)
    (call-next-method)))

(defmethod scroll-horizontal :around ((stream stream-output-history-mixin) dx)
  (declare (ignore dx))
  (with-output-recording-options (stream :record nil)
    (call-next-method)))

(defmethod repaint-sheet ((stream stream-output-history-mixin) region)
  (replay (stream-output-history stream) stream region))

(defmethod handle-event ((stream stream-output-history-mixin) (event window-repaint-event))
  (repaint-sheet stream nil))

(defmethod handle-event ((stream stream-output-history-mixin) (event pointer-button-press-event))
  (with-slots (button x y) event
    (format *debug-io* "button ~D pressed at ~D,~D~%" button x y)))


;;; standard-tree-output-history class

(defclass standard-tree-output-history (stream-output-history-mixin)
  (
   ))

(defmethod initialize-instance :after ((history standard-tree-output-history) &rest args)
  (declare (ignore args))
  (with-slots (output-history) history
    (setq output-history (make-instance 'standard-tree-output-record))))


;;; Output-Recording-Stream class

(defclass output-recording-stream (standard-tree-output-history)
  ((current-output-record
    :accessor stream-current-output-record)
   (drawing-p :initform t :accessor stream-drawing-p)
   ))

(defun output-recording-stream-p (x)
  (typep x 'output-recording-stream))

(defmethod initialize-instance :after ((stream output-recording-stream) &rest args)
  (declare (ignore args))
  (setf (stream-current-output-record stream) (stream-output-history stream)))

(defmethod stream-add-output-record ((stream output-recording-stream) record)
  (add-output-record record (stream-current-output-record stream)))

(defmethod stream-replay ((stream output-recording-stream) &optional region)
  (replay (stream-output-history stream) stream region))

(defclass standard-output-recording-stream (output-recording-stream)
  (
   ))

(defmacro with-output-recording-options ((stream &key (record t) (draw t)) &body body)
  (let ((old-record (gensym))
	(old-draw (gensym)))
    `(with-slots (recording-p drawing-p) ,stream
       (let ((,old-record recording-p)
	     (,old-draw drawing-p))
	 (unwind-protect
	     (progn
	       (setq recording-p ,record
		     drawing-p ,draw)
	       ,@body)
	   (setq recording-p ,old-record
		 drawing-p ,old-draw))))))


;;; graphics and text recording classes

(eval-when (compile load eval)
  
  (defun compute-class-vars (names)
    (cons (list 'stream :initarg :stream)
	  (loop for name in names
		collecting (list name :initarg (intern (symbol-name name) :keyword)))))

  (defun compute-arg-list (names)
    (loop for name in names
	  nconcing (list (intern (symbol-name name) :keyword) name)))
  )

(defun make-merged-medium (sheet ink clip transform line-style text-style)
  (let ((medium (make-medium (port sheet) sheet)))
    (setf (medium-ink medium) ink)
    (setf (medium-clipping-region medium) clip)
    (setf (medium-transformation medium) transform)
    (setf (medium-line-style medium) line-style)
    (setf (medium-text-style medium) text-style)
    medium))

(defmacro def-grecording (name (&rest args) &body body)
  (let ((method-name (intern (format nil "MEDIUM-~A*" name)))
	(class-name (intern (format nil "~A-OUTPUT-RECORD" name)))
	(old-medium (gensym))
	(new-medium (gensym)))
    `(eval-when (eval load compile)
       (defclass ,class-name (graphics-displayed-output-record)
	 ,(compute-class-vars args))
       (defmethod initialize-instance :after ((graphic ,class-name) &rest args)
	 (declare (ignore args))
	 (with-slots (x1 y1 x2 y2
		      stream ink clipping-region transformation
		      line-style text-style
		      ,@args) graphic
	   (multiple-value-bind (lf tp rt bt) (progn ,@body)
	     (setq x1 lf
		   y1 tp
		   x2 rt
		   y2 bt))))
       (defmethod ,method-name :around ((stream stream-output-history-mixin) ,@args)
	 (with-sheet-medium (medium stream)
	   (when (stream-recording-p stream)
	     (let ((record (make-instance ',class-name
			     :stream stream
			     :ink (medium-ink medium)
			     :clipping-region (medium-clipping-region medium)
			     :transformation (medium-transformation medium)
			     :line-style (medium-line-style medium)
			     :text-style (medium-text-style medium)
			     ,@(compute-arg-list args))))
	       (add-output-record record (stream-output-history stream))
	       ))
	   (when (stream-drawing-p stream)
	     (call-next-method))))
       (defmethod replay-output-record ((record ,class-name) stream
					&optional region x-offset y-offset)
	 (declare (ignore region x-offset y-offset))
	 (with-slots (ink clip transform line-style text-style ,@args) record
	   (let ((,old-medium (sheet-medium stream))
		 (,new-medium (make-merged-medium stream ink clip transform line-style text-style)))
	     (unwind-protect
		 (progn
		   (setf (sheet-medium stream) ,new-medium)
		   (setf (medium-sheet ,new-medium) stream)
		   (,method-name ,new-medium ,@args))
	       (setf (sheet-medium stream) ,old-medium))))))))

(def-grecording draw-point (x y)
  (values x y x y))

(def-grecording draw-points (coord-seq)
  (loop for (x y) on coord-seq by #'cddr
	minimize x into min-x
	minimize y into min-y
	maximize x into max-x
	maximize y into max-y
	finally (return (values min-x min-y max-x max-y))))

(def-grecording draw-line (x1 y1 x2 y2)
  (values (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)))

(def-grecording draw-lines (coord-seq)
  (loop for (x y) on coord-seq by #'cddr
	minimize x into min-x
	minimize y into min-y
	maximize x into max-x
	maximize y into max-y
	finally (return (values min-x min-y max-x max-y))))

(def-grecording draw-polygon (coord-seq closed filled)
  (loop for (x y) on coord-seq by #'cddr
	minimize x into min-x
	minimize y into min-y
	maximize x into max-x
	maximize y into max-y
	finally (return (values min-x min-y max-x max-y))))

(def-grecording draw-rectangle (left top right bottom filled)
  (values (min left right) (min top bottom) (max left right) (max top bottom)))

(def-grecording draw-ellipse (center-x center-y
			      radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			      start-angle end-angle filled)
  (values center-x center-y center-x center-y))

;(def-grecording draw-text (string x y start end
;			   align-x align-y toward-x toward-y transform-glyphs)
;  (let* ((width (stream-string-width stream string
;                                     :start start :end end
;                                     :text-style text-style))
;         (ascent (text-style-ascent text-style (port (sheet-medium stream))))
;         (descent (text-style-descent text-style (port (sheet-medium stream))))
;         (height (+ ascent descent))
;         left top right bottom)
;    (ecase align-x
;      (:left (setq left x
;                   right (+ x width)))
;      (:right (setq left (- x width)
;                    right x))
;      (:center (setq left (- x (round width 2))
;                     right (+ x (round width 2)))))
;    (ecase align-y
;      (:baseline (setq top (- y height)
;                       bottom (+ y descent)))
;      (:top (setq top y
;                  bottom (+ y height)))
;      (:bottom (setq top (- y height)
;                     bottom y))
;      (:center (setq top (- y (floor height 2))
;                     bottom (+ y (ceiling height 2)))))
;    (values left top right bottom)))


;;; Text recording class

(defclass text-displayed-output-record (displayed-output-record)
  ((strings :initform nil)
   (baseline :initform 0)
   (max-height :initform 0)
   (start-x :initarg :start-x)
   (start-y :initarg :start-y)
   (end-x)
   (end-y)
   (wrapped :initform nil
	    :accessor text-record-wrapped)))

(defun text-displayed-output-record-p (x)
  (typep x 'text-displayed-output-record))

(defmethod print-object ((self text-displayed-output-record) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (if (slot-boundp self 'start-x)
	(with-slots (start-x start-y strings) self
	  (format stream "~D,~D ~S" start-x start-y (mapcar #'third strings)))
      (format stream "empty"))))

(defmethod add-character-output-to-text-record ((text-record text-displayed-output-record)
						character text-style width height
						new-baseline)
  (with-slots (strings baseline max-height end-x end-y) text-record
    (if (and strings (eq (second (first (last strings))) text-style))
	(vector-push-extend character (third (first (last strings))))
      (setq strings (nconc strings (list (list end-x text-style (make-array 1 :initial-element character :element-type 'character :adjustable t :fill-pointer t))))))
    (setq baseline (max baseline new-baseline)
	  end-x (+ end-x width)
	  end-y (max end-y new-baseline)
	  max-height (max max-height height)
	  )
      ))

(defmethod add-string-output-to-text-record ((text-record text-displayed-output-record)
					     string start end text-style width height
					     new-baseline)
  (setq string (subseq string start end))
  (with-slots (strings baseline max-height end-x) text-record
    (setq baseline (max baseline new-baseline)
	  strings (nconc strings (list (list end-x text-style (make-array (length string) :initial-contents string :element-type 'character :adjustable t :fill-pointer t))))
	  end-x (+ end-x width)
	  max-height (max max-height height)
	  )))

(defmethod replay-output-record ((record text-displayed-output-record) stream
				 &optional region x-offset y-offset)
  (declare (ignore x-offset y-offset))
  (with-slots (strings baseline max-height start-x start-y wrapped) record
    (let ((old-medium (sheet-medium stream))
	  (new-medium (make-medium (port stream) stream)))
      (unwind-protect
	  (progn
	    (setf (sheet-medium stream) new-medium)
	    (setf (medium-sheet new-medium) stream)
	    (loop for y = (+ start-y baseline)
		  for (x text-style string) in strings
		  do (setf (medium-text-style new-medium) text-style)
		     (draw-text* stream string x y
				 :text-style text-style :clipping-region region))
	    (if wrapped
		(draw-rectangle* (sheet-medium stream)
				 (+ wrapped 0) start-y (+ wrapped 4) (+ start-y max-height)
				 :ink +foreground-ink+
				 :filled t)))
	(setf (sheet-medium stream) old-medium)))))

(defmethod output-record-start-cursor-position ((record text-displayed-output-record))
  (with-slots (start-x start-y) record
    (values start-x start-y)))

(defmethod output-record-end-cursor-position ((record text-displayed-output-record))
  (with-slots (end-x end-y) record
    (values end-x end-y)))

(defmethod text-displayed-output-record-string ((record text-displayed-output-record))
  (with-slots (strings) record
    (loop for result = ""
	  for s in strings
	  do (setq result (concatenate 'string result (third s)))
	     finally (return result))))



(defmethod get-text-record ((stream output-recording-stream))
  (let ((trec (stream-current-output-record stream)))
    (unless (text-displayed-output-record-p trec)
      (setq trec (make-instance 'text-displayed-output-record))
      (add-output-record trec (stream-output-history stream))
      (setf (stream-current-output-record stream) trec)
      (with-slots (start-x start-y end-x end-y) trec
	  (multiple-value-bind (cx cy) (stream-cursor-position stream)
	    (setq start-x cx
		  start-y (+ cy (stream-vertical-spacing stream))
		  end-x start-x
		  end-y start-y))))
    trec))

(defmethod stream-write-char :around ((stream output-recording-stream) char)
  (when (stream-recording-p stream)
    (get-text-record stream))
  (call-next-method)
  (when (stream-recording-p stream)
    (cond
     ((not (or (eql char #\return)
	       (eql char #\newline)))
      (let* ((medium (sheet-medium stream))
	     (text-style (medium-text-style medium))
	     (trec (get-text-record stream))
	     (port (port stream)))
	(add-character-output-to-text-record
	 trec char text-style
	 (stream-character-width stream char :text-style text-style)
	 (text-style-height text-style port)
	 (text-style-ascent text-style port))))
     (t
      (let ((trec (make-instance 'text-displayed-output-record)))
	(add-output-record trec (stream-output-history stream))
	(setf (stream-current-output-record stream) trec)
	(with-slots (start-x start-y end-x end-y) trec
	  (multiple-value-bind (cx cy) (stream-cursor-position stream)
	    (setq start-x cx
		  start-y (+ cy (stream-vertical-spacing stream))
		  end-x start-x
		  end-y start-y))))))))

(defmethod stream-wrap-line :before ((stream output-recording-stream))
  (when (stream-recording-p stream)
    (setf (text-record-wrapped (get-text-record stream)) (stream-text-margin stream))))