(in-package #:climi)

;;;;
;;;;  Drawing Utilities for Concrete Gadgets
;;;;

;;; Labels

(defmethod reinitialize-instance :after ((instance labelled-gadget-mixin)
                                         &key (label nil label-supplied-p))
  (when label-supplied-p
    (setf (gadget-label instance) label)))

(defmethod (setf gadget-label) :after (new-value (gadget labelled-gadget-mixin))
  (change-space-requirements gadget)
  (repaint-sheet gadget (sheet-region gadget)))

(defgeneric compose-label-space (gadget &key wider higher))

(defmethod compose-label-space ((gadget labelled-gadget-mixin) &key (wider 0) (higher 0))
  (multiple-value-bind (text-width text-height)
      (text-size gadget (gadget-label gadget) :text-style (pane-text-style gadget))
    (let ((width  (+ text-width  wider))
          (height (+ text-height higher)))
      (make-space-requirement :width      width
                              :min-width  width
                              :max-width  +fill+
                              :height     height
                              :min-height height
                              :max-height +fill+))))

(defgeneric draw-label* (pane x1 y1 x2 y2 &key ink))

(defmethod draw-label* ((pane labelled-gadget-mixin) x1 y1 x2 y2
                        &key (ink +foreground-ink+))
  (with-slots (align-x align-y label) pane
    (let ((text-style (pane-text-style pane)))
      (draw-text* pane label
                  (ecase align-x
                    (:left x1)
                    (:right x2)
                    (:center (/ (+ x1 x2) 2)))
                  (ecase align-y
                    (:top y1)
                    (:center (/ (+ y1 y2) 2))
                    (:bottom y2))
                  :align-x align-x :align-y align-y
                  ;; Giving the text-style here shouldn't be neccessary --GB
                  :text-style text-style
                  :ink ink))))

(defun display-gadget-background (gadget color x1 y1 x2 y2)
  (draw-rectangle* gadget x1 y1 x2 y2 :ink color :filled t))
