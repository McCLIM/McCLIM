;; FIXME: clim-cairo had more fixes here

(in-package :climi)

(defclass out-compositum (design)
  ((ink  :initarg :ink  :reader compositum-ink)
   (mask :initarg :mask :reader compositum-mask)))

(defmethod print-object ((object out-compositum) stream)
  (print-unreadable-object (object stream :identity nil :type t)
    (format stream "~S ~S ~S ~S"
            :ink (compositum-ink object)
            :mask (compositum-mask object))))

(defmethod compose-out ((ink design) (mask design))
  (make-instance 'out-compositum
    :ink ink
    :mask mask))

;; FIXME: See bug 17.
;;;(defmethod transform-region (transformation (design design))
;;;  (make-instance 'transformed-design
;;;                 :transformation transformation
;;;                 :design design)
;;;  (call-next-method))

(defmethod clim:handle-repaint :around ((s clim:sheet-with-medium-mixin) r)
  (let ((m (clim:sheet-medium s))
        (r (clim:bounding-rectangle
            (clim:region-intersection r (clim:sheet-region s)))))
    (unless (eql r clim:+nowhere+)
      ;; Test case: Start CLIM-DEMO::DEMODEMO and watch the header string.
      ;; At the beginning, the text is nicely antialiased.  Then start any
      ;; demo and move the new window around over the header.  As the
      ;; header gets exposed again, the text is apparently redrawn
      ;; multiple times and looks like crap.  This fixes it:
      (clim:with-drawing-options (m :clipping-region r)
        (clim:draw-design m r :ink clim:+background-ink+)
        (call-next-method s r)))))

;; cairo hack: adjust rectangle coordinates by half a pixel each to avoid
;; anti-aliasing (and follow-up output artifacts)
(defun highlight-output-record-rectangle (record stream state)
  (with-identity-transformation (stream)
    (multiple-value-bind (x1 y1 x2 y2)
        (output-record-hit-detection-rectangle* record)
      (ecase state
        (:highlight	 
	  (draw-rectangle* (sheet-medium stream)
			   (+ (ceiling x1) 0.5d0)
			   (+ (ceiling y1) 0.5d0)
			   (- (floor (1- x2)) 0.5d0)
			   (- (floor (1- y2)) 0.5d0)
			   ;; XXX +FLIPPING-INK+? 
			   :filled nil :ink +foreground-ink+))
        (:unhighlight
	  ;; FIXME: repaint the hit detection rectangle. It could be
	  ;; bigger than
	  ;; the bounding rectangle.
	  (repaint-sheet stream record)))
      (force-output stream))))
