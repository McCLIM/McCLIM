;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
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

(defclass output-record-mixin ()
  ((x :initarg :x-position
      :initform 0)
   (y :initarg :y-position
      :initform 0)
   (parent :initarg :parent
	   :initform nil
           :reader output-record-parent)))

(defmethod initialize-instance :after ((record output-record-mixin) &rest args)
  (declare (ignore args))
  (with-slots (x1 y1 x2 y2) record
    (setq x1 0
	  y1 0
	  x2 0
	  y2 0)))

(defclass output-record (standard-bounding-rectangle output-record-mixin)
  ((children :initform nil
	     :reader output-record-children))
  (:default-initargs :min-x 0 :min-y 0 :max-x 0 :max-y 0))

(defun output-record-p (x)
  (typep x 'output-record))

(defclass displayed-output-record (standard-bounding-rectangle output-record-mixin)
  ((ink :initarg :ink :reader displayed-output-record-ink)))

(defun displayed-output-record-p (x)
  (typep x 'displayed-output-record))

(defmethod initialize-instance :after ((record output-record) &rest args
				       &key size
				       &allow-other-keys)
  (declare (ignore args size)))

(defmethod output-record-position ((record output-record-mixin))
  (with-slots (x y) record
    (values x y)))

(defmethod setf*-output-record-position (nx ny (record output-record-mixin))
  (with-slots (x y) record
    (setq x nx
	  y ny)))

(defmethod setf*-output-record-position :before (nx ny (record output-record))
  (multiple-value-bind (old-x old-y) (output-record-position record)
    (loop with dx = (- nx old-x)
          and dy = (- ny old-y)
          for child in (output-record-children record)
          do (multiple-value-bind (x y) (output-record-position child)
               (setf*-output-record-position (+ x dx) (+ y dy) child)))))

(defmethod setf*-output-record-position :around (nx ny (record output-record-mixin))
  (declare (ignore nx ny))
  (with-bounding-rectangle* (min-x min-y max-x max-y) record
    (call-next-method)
    (recompute-extent-for-changed-child (output-record-parent record) record
                                        min-x min-y max-x max-y)))

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
  (when (stream-drawing-p stream)
    (let ((old-record-p (stream-recording-p stream)))
      (unwind-protect
           (progn
             (setf (stream-recording-p stream) nil)
             (replay-output-record record stream region))
        (setf (stream-recording-p stream) old-record-p)))))

(defmethod replay-output-record ((record output-record) stream
				 &optional region x-offset y-offset)
  (when (null region)
    (setq region +everywhere+))
  (map-over-output-records-overlaping-region
   #'replay-output-record record region x-offset y-offset
   stream region x-offset y-offset))

(defmethod erase-output-record ((record output-record) stream)
  (declare (ignore stream))
  nil)

(defmethod output-record-hit-detection-rectangle* ((record output-record-mixin))
  (bounding-rectangle* record))

(defmethod output-record-refined-sensitivity-test ((record output-record-mixin) x y)
  (declare (ignore x y))
  t)

(defmethod highlight-output-record ((record output-record-mixin) stream state)
  (multiple-value-bind (x1 y1 x2 y2) (output-record-hit-detection-rectangle* record)
    (ecase state
      (:highlight
       (draw-rectangle* (sheet-medium stream) x1 y1 x2 y2 :filled nil :ink +foreground-ink+))
      (:unhighlight
       (draw-rectangle* (sheet-medium stream) x1 y1 x2 y2 :filled nil :ink +background-ink+)))))

(defclass standard-sequence-output-record (output-record)
  (
   ))

(defclass standard-tree-output-record (output-record)
  (
   ))

(defmethod output-record-children ((output-record output-record))
  (with-slots (children) output-record
    (reverse children)))

(defmethod add-output-record (child (record output-record))
  (with-slots (children) record
    (push child children))
  (with-slots (parent) child
    (setf parent record)))

(defmethod add-output-record :before (child (record output-record))
  (when (null (output-record-children record))
    (with-bounding-rectangle* (min-x min-y max-x max-y) child
    (with-slots (x1 y1 x2 y2) record
      (setq x1 min-x
            y1 min-y
            x2 max-x
            y2 max-y)))))

(defmethod add-output-record :after (child (record output-record))
  (recompute-extent-for-new-child record child))

(defmethod delete-output-record (child (record output-record) &optional (errorp t))
  (with-slots (children) record
    (if (and errorp
	     (not (member child children)))
	(error "~S is not a child of ~S" child record))
    (setq children (delete child children))))

(defmethod delete-output-record :after (child (record output-record) &optional (errorp t))
  (declare (ignore errorp))
  (with-bounding-rectangle* (x1 y1 x2 y2) child
    (recompute-extent-for-changed-child record child x1 y1 x2 y2)))

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
							&optional (x-offset 0) (y-offset 0)
                                                        &rest function-args)
  (declare (dynamic-extent function)
	   (ignore x-offset y-offset))
  (loop for child in (output-record-children record)
	when (and (region-contains-position-p
                   (multiple-value-call #'make-bounding-rectangle
                     (output-record-hit-detection-rectangle* child))
                   x y)
                  (output-record-refined-sensitivity-test child x y))
        do (apply function child function-args)))

(defmethod map-over-output-records-overlaping-region (function (record output-record) region
						      &optional (x-offset 0) (y-offset 0)
                                                      &rest function-args)
  (declare (dynamic-extent function)
	   (ignore x-offset y-offset))
  (loop for child in (output-record-children record)
        do (when (region-intersects-region-p region child)
             (apply function child function-args))))

(defmethod recompute-extent-for-new-child ((record output-record) child)
  (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2) record
    (with-slots (parent x1 y1 x2 y2) record
      (with-bounding-rectangle* (x1-child y1-child x2-child y2-child) child
        (setq x1 (min x1 x1-child)
              y1 (min y1 y1-child)
              x2 (max x2 x2-child)
              y2 (max y2 y2-child)))
      (when parent
        (recompute-extent-for-changed-child parent record old-x1 old-y1 old-x2 old-y2)))))

(defmethod recompute-extent-for-changed-child :around ((record output-record) child
						       old-min-x old-min-y old-max-x old-max-y)
  (declare (ignore child old-min-x old-min-y old-max-x old-max-y))
  (let ((old-rectangle (multiple-value-call #'make-bounding-rectangle
                         (bounding-rectangle* record))))
    (call-next-method)
    (with-slots (parent x1 y1 x2 y2) record
      (when (and parent
                 (region-equal old-rectangle record))
      	(recompute-extent-for-changed-child parent record x1 y1 x2 y2)))))

(defmethod recompute-extent-for-changed-child ((record output-record) changed-child
					       old-min-x old-min-y old-max-x old-max-y)
  (with-slots (children x1 y1 x2 y2) record
    (with-bounding-rectangle* (new-x1 new-y1 new-x2 new-y2) changed-child
      (setq x1 (min x1 new-x1)
	    y1 (min y1 new-y1)
	    x2 (max x2 new-x2)
	    y2 (max y2 new-y2)))
    (if (null children)
        (clear-output-record record)
	(when (or (coordinate= x1 old-min-x)
		  (coordinate= y1 old-min-y)
		  (coordinate= x2 old-max-x)
		  (coordinate= y2 old-max-y))
	  (with-bounding-rectangle* (left top right bottom) (first children)
	    (loop for child in (rest children)
		  do (with-bounding-rectangle* (x1-child y1-child x2-child y2-child) child
		       (setq left (min left x1-child)
			     top  (min top y1-child)
			     right  (max right x2-child)
			     bottom (max bottom y2-child))))
	    (setq x1 left
		  y1 top
		  x2 right
		  y2 bottom))))))

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


;;; Graphics recording classes

(defclass graphics-displayed-output-record (displayed-output-record)
  ((clip :initarg :clipping-region
         :documentation "Clipping region in user coordinates.")
   (transform :initarg :transformation)
   (line-style :initarg :line-style)
   (text-style :initarg :text-style)
   ))

(defun graphics-displayed-output-record-p (x)
  (typep x 'graphics-displayed-output-record))


;;; stream-output-history-mixin class

(defclass stream-output-history-mixin ()
  ())

(defclass standard-sequence-output-history (standard-sequence-output-record stream-output-history-mixin)
  ())

(defclass standard-tree-output-history (standard-tree-output-record stream-output-history-mixin)
  ())


;;; Output-Recording-Stream class

(defclass output-recording-stream ()
  ((recording-p :initform t :accessor stream-recording-p)
   (drawing-p :initform t :accessor stream-drawing-p)
   (output-history :initform (make-instance 'standard-tree-output-history)
                   :reader stream-output-history)
   (current-output-record :accessor stream-current-output-record)))

(defun output-recording-stream-p (x)
  (typep x 'output-recording-stream))

(defclass standard-output-recording-stream (output-recording-stream)
  (
   ))

(defmethod initialize-instance :after ((stream output-recording-stream) &rest args)
  (declare (ignore args))
  (setf (stream-current-output-record stream) (stream-output-history stream)))

(defmethod stream-add-output-record ((stream output-recording-stream) record)
  (add-output-record record (stream-output-history stream)))

(defmethod stream-replay ((stream output-recording-stream) &optional region)
  (replay (stream-output-history stream) stream region))

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

(defmethod scroll-vertical :around ((stream output-recording-stream) dy)
  (declare (ignore dy))
  (with-output-recording-options (stream :record nil)
    (call-next-method)))

(defmethod scroll-horizontal :around ((stream output-recording-stream) dx)
  (declare (ignore dx))
  (with-output-recording-options (stream :record nil)
    (call-next-method)))

(defmethod repaint-sheet ((stream output-recording-stream) region)
  (stream-replay stream region))

(defmethod handle-event ((stream output-recording-stream) (event window-repaint-event))
  (repaint-sheet stream (window-event-region event)))

(defmethod handle-event ((stream output-recording-stream) (event pointer-button-press-event))
  (with-slots (button x y) event
    (format *debug-io* "button ~D pressed at ~D,~D~%" button x y)))

#|
(defmethod handle-event :after ((stream output-recording-stream) (event pointer-button-press-event))
  (highlight-output-record (stream-current-output-record stream) stream :highlight)
  (highlight-output-record (stream-output-history stream) stream :highlight))

(defmethod handle-event :before ((stream output-recording-stream) (event pointer-button-release-event))
  (highlight-output-record (stream-current-output-record stream) stream :unhighlight)
  (highlight-output-record (stream-output-history stream) stream :unhighlight))
|#


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
    ;; First set transformation, then clipping region!
    (setf (medium-transformation medium) transform)
    (setf (medium-clipping-region medium) clip)
    (setf (medium-line-style medium) line-style)
    (setf (medium-text-style medium) text-style)
    medium))

(defmacro def-grecording (name (&rest args) &body body)
  (let ((method-name (intern (format nil "MEDIUM-~A*" name)))
	(class-name (intern (format nil "~A-OUTPUT-RECORD" name)))
	(old-medium (gensym))
	(new-medium (gensym))
	(border (gensym)))
    `(progn
       (defclass ,class-name (graphics-displayed-output-record)
	 ,(compute-class-vars args))
       (defmethod initialize-instance :after ((graphic ,class-name) &rest args)
	 (declare (ignore args))
	 (with-slots (x1 y1 x2 y2
		      stream ink clipping-region transform
		      line-style text-style
		      ,@args) graphic
           (let ((,border (1+ (/ (line-style-thickness line-style) 2))))
             (multiple-value-bind (lf tp rt bt) (progn ,@body)
               (setq x1 (- lf ,border)
                     y1 (- tp ,border)
                     x2 (+ rt ,border)
                     y2 (+ bt ,border))))))
       (defmethod ,method-name :around ((stream output-recording-stream) ,@args)
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
	       (stream-add-output-record stream record)))
	   (when (stream-drawing-p stream)
	     (call-next-method))))
       (defmethod replay-output-record ((record ,class-name) stream
					&optional (region +everywhere+) x-offset y-offset)
	 (declare (ignore x-offset y-offset))
	 (with-slots (ink clip transform line-style text-style ,@args) record
	   (let ((,old-medium (sheet-medium stream))
		 (,new-medium (make-merged-medium stream ink (region-intersection clip
                                                                (untransform-region transform region))
                                                  transform line-style text-style)))
             (finish-output *error-output*)
	     (unwind-protect
		 (progn
		   (setf (sheet-medium stream) ,new-medium)
		   (setf (medium-sheet ,new-medium) stream)
		   (,method-name ,new-medium ,@args))
	       (setf (sheet-medium stream) ,old-medium))))))))

(def-grecording draw-point (point-x point-y)
  (with-transformed-position (transform point-x point-y)
     (values point-x point-y point-x point-y)))

(def-grecording draw-points (coord-seq)
  (with-transformed-positions (transform coord-seq)
     (loop for (x y) on coord-seq by #'cddr
           minimize x into min-x
           minimize y into min-y
           maximize x into max-x
           maximize y into max-y
           finally (return (values min-x min-y max-x max-y)))))

(def-grecording draw-line (point-x1 point-y1 point-x2 point-y2)
  (with-transformed-position (transform point-x1 point-y1)
    (with-transformed-position (transform point-x2 point-y2)
      (values (min point-x1 point-x2) (min point-y1 point-y2)
              (max point-x1 point-x2) (max point-y1 point-y2)))))

(def-grecording draw-lines (coord-seq)
  (with-transformed-positions (transform coord-seq)
     (loop for (x y) on coord-seq by #'cddr
           minimize x into min-x
           minimize y into min-y
           maximize x into max-x
           maximize y into max-y
           finally (return (values min-x min-y max-x max-y)))))

(def-grecording draw-polygon (coord-seq closed filled)
  (with-transformed-positions (transform coord-seq)
     (loop for (x y) on coord-seq by #'cddr
           minimize x into min-x
           minimize y into min-y
           maximize x into max-x
           maximize y into max-y
           finally (return (values min-x min-y max-x max-y)))))

(def-grecording draw-rectangle (left top right bottom filled)
  ;; XXX transformation!!!
  (values (min left right) (min top bottom) (max left right) (max top bottom)))

(def-grecording draw-ellipse (center-x center-y
			      radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			      start-angle end-angle filled)
  ;; XXX transformation!!!
  (let ((radius-dx (abs (+ radius-1-dx radius-2-dx)))
        (radius-dy (abs (+ radius-1-dy radius-2-dy))))
    (values (- center-x radius-dx) (- center-y radius-dy)
            (+ center-x radius-dx) (+ center-y radius-dy))))

(def-grecording draw-text (string point-x point-y start end
			   align-x align-y toward-x toward-y transform-glyphs)
  ;; XXX transformation!!!
 (let* ((width (stream-string-width stream string
                                    :start start :end end
                                    :text-style text-style))
        (ascent (text-style-ascent text-style (port (sheet-medium stream))))
        (descent (text-style-descent text-style (port (sheet-medium stream))))
        (height (+ ascent descent))
        left top right bottom)
   (ecase align-x
     (:left (setq left point-x
                  right (+ point-x width)))
     (:right (setq left (- point-x width)
                   right point-x))
     (:center (setq left (- point-x (round width 2))
                    right (+ point-x (round width 2)))))
   (ecase align-y
     (:baseline (setq top (- point-y height)
                      bottom (+ point-y descent)))
     (:top (setq top point-y
                 bottom (+ point-y height)))
     (:bottom (setq top (- point-y height)
                    bottom point-y))
     (:center (setq top (- point-y (floor height 2))
                    bottom (+ point-y (ceiling height 2)))))
   (values left top right bottom)))


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

(defmethod tree-recompute-extent ((text-record text-displayed-output-record))
  (with-slots (parent start-x start-y end-x end-y x1 y1 x2 y2) text-record
    (setq x1 start-x
	  x2 end-x
	  y1 start-y
	  y2 end-y)
    (recompute-extent-for-changed-child parent text-record start-x start-y end-x end-y)))

(defmethod add-character-output-to-text-record ((text-record text-displayed-output-record)
						character text-style width height
						new-baseline)
  (with-slots (strings baseline max-height start-y end-x end-y) text-record
    (if (and strings (eq (second (first (last strings))) text-style))
	(vector-push-extend character (third (first (last strings))))
      (setq strings (nconc strings (list (list end-x text-style (make-array 1 :initial-element character :element-type 'character :adjustable t :fill-pointer t))))))
    (setq baseline (max baseline new-baseline)
	  end-x (+ end-x width)
	  max-height (max max-height height)
	  end-y (max end-y (+ start-y max-height))
	  )
      )
  (tree-recompute-extent text-record))

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
		     (draw-text* (sheet-medium stream) string x y
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
      (with-slots (start-x start-y end-x end-y x1 y1 x2 y2) trec
	  (multiple-value-bind (cx cy) (stream-cursor-position stream)
	    (setq start-x cx
		  start-y (+ cy (stream-vertical-spacing stream))
		  end-x start-x
		  end-y start-y
		  x1 start-x
		  x2 end-x
		  y1 start-y
		  y2 end-y))))
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
	(with-slots (start-x start-y end-x end-y x1 y1 x2 y2) trec
	  (multiple-value-bind (cx cy) (stream-cursor-position stream)
	    (setq start-x cx
		  start-y (+ cy (stream-vertical-spacing stream))
		  end-x start-x
		  end-y start-y
		  x1 start-x
		  x2 end-x
		  y1 start-y
		  y2 end-y))))))))

(defmethod stream-wrap-line :before ((stream output-recording-stream))
  (when (stream-recording-p stream)
    (setf (text-record-wrapped (get-text-record stream)) (stream-text-margin stream))))
