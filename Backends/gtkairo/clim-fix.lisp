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

(defmethod clim:handle-repaint :after ((s clim:sheet-with-medium-mixin) r)
  (medium-force-output (sheet-medium s)))

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
			   (+ (floor (1- x2)) 0.5d0)
			   (+ (floor (1- y2)) 0.5d0)
			   ;; XXX +FLIPPING-INK+? 
			   :filled nil :ink +foreground-ink+))
        (:unhighlight
	  ;; FIXME: repaint the hit detection rectangle. It could be
	  ;; bigger than
	  ;; the bounding rectangle.
	  (repaint-sheet stream record))))))
