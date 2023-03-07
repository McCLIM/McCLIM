;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2023 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The implementation of displaying and drawing of the "text" buffer. Here we
;;; implement thins like a text direction and margins.

(in-package #:climi)

;;; We need to use a stealth mixin here because cluffer calls change-class on
;;; open/closed-line so our class would be lost in no time.
;;;
;;; Note that the logical line may span multiple lines on the screen due to
;;; the line wrap.  Also the line direction is not always left-to-right.
;;;
;;; Instead of having "x" and "y" parts of various positions and offsets we
;;; use points in the geometrical sense so we don't need to deal with multiple
;;; values directly. That improves the code robustness and clarity.
(stealth-mixin:define-stealth-mixin edward-line () (cluffer:line)
  ((start-position)                  ; cursor start position
   (end-position)                    ; cursor end position
   (baseline)                        ; baseline offset
   (line-offset)                     ; single line offset
   (block-offset)                    ; whole block offset
   (end-of-page-action)
   (end-of-line-action)))

;;; cursor-position-from-coordinates
;;; coordinates-from-cursor-position

;;; OK, this is a stretch, but let's pretend that lines are output records.
(defmethod replay-output-record ((record edward-line) sheet
                                 &optional region dx dy)
  (let ((text (line-string record))
        (start (start-position record)))
    (multiple-value-bind (x y) (point-position start)
      (draw-text* sheet text (+ x dx) (+ y dy)
                  :aling-x :left :align-y :top
                  :order-x :left :order-y :center))))

(deftype horizontal-direction (member :left-to-right :right-to-left))
(deftype vertical-direction   (member :top-to-bottom :bottom-to-top))
(deftype direction (or horizontal-direction vertical-direction))

(defun redraw-line (line)
  )

(defclass edward-sheet-mixin ()
  ((page-region)                        ; accounts for margins
   (line-direction :type (:member :lr :rl :tb :bt))
   (page-direction :type (:member :lr :rl :tb :bt))
   (end-of-page-action)
   (end-of-line-action)))
