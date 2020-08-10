(in-package #:climi)

;;;;
;;;;  Drawing Utilities for Concrete Gadgets
;;;;

;;; Labels

(defmethod compose-label-space
    ((gadget labelled-gadget-mixin) &key (wider 0) (higher 0))
  (with-slots (label align-x align-y) gadget
    (let* ((text-style (pane-text-style gadget))
           (as (text-style-ascent text-style gadget))
           (ds (text-style-descent text-style gadget))
           (w  (+ (text-size gadget label :text-style text-style) wider))
           (h  (+ as ds higher)))
      (make-space-requirement :width w  :min-width w  :max-width  +fill+
                              :height h :min-height h :max-height +fill+))))

(defmethod draw-label* ((pane labelled-gadget-mixin) x1 y1 x2 y2
                        &key (ink +foreground-ink+))
  (with-slots (align-x align-y label) pane
    (let* ((text-style (pane-text-style pane))
           (as (text-style-ascent text-style pane))
           (ds (text-style-descent text-style pane))
           (w  (text-size pane label :text-style text-style)))
      (draw-text* pane label
                  (case align-x
                    ((:left) x1)
                    ((:right) (- x2 w))
                    ((:center) (/ (+ x1 x2 (- w)) 2))
                    (otherwise x1)) ; defensive programming
                  (case align-y
                    ((:top) (+ y1 as))
                    ((:center) (/ (+ y1 y2 (- as ds)) 2))
                    ((:bottom) (- y2 ds))
                    (otherwise (/ (+ y1 y2 (- as ds)) 2))) ;defensive programming
                  ;; Giving the text-style here shouldn't be neccessary --GB
                  :text-style text-style
                  :ink ink))))

(defun display-gadget-background (gadget color x1 y1 x2 y2)
  (draw-rectangle* gadget x1 y1 x2 y2 :ink color :filled t))
