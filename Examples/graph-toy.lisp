;;; ---------------------------------------------------------------------------
;;;   License: LGPL
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2017 by Kyle Nusbaum <knusbaum@sdf.org>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Graph Toy Demo
;;;
;;; Shows how to redisplay an application pane in response to asynchronous
;;; events.

(in-package #:clim-demo)

(defun sub-array-for (arr length)
  (let ((orig-len (length arr)))
    (if (<= orig-len length)
        arr
        (make-array length :displaced-to arr
                           :displaced-index-offset (- orig-len length)))))

(defun display-main (frame stream)
  (when (zerop (length (val-array frame)))
    (return-from display-main nil))
  (let* ((left-x-padding 30)
         (pane (find-pane-named frame 'main-display))
         (values (sub-array-for (val-array frame)
                                (max-xvals frame)))
         (maximum (reduce #'max values :initial-value 0))
         (minimum (reduce #'min values :initial-value 0))
         (variance (- maximum minimum))
         (rect (bounding-rectangle (sheet-region pane)))
         (width (rectangle-width rect))
         (height (- (rectangle-height rect) 100))
         (w-step (/ (- width left-x-padding) (length values)))
         (h-step (if (> variance 0) (/ height variance) 0))
         (y-label-count (min (truncate (/ height 100)) variance))
         (step-fn-x (let ((current left-x-padding))
                      (lambda ()
                        (truncate (setf current (+ current w-step))))))
         (fn-y (lambda (y)
                 (+ (- height (truncate (* h-step (+ y (- minimum))))) 50))))

    ;; Draw the title
    (draw-text* pane (title frame)
                (/ width 2) 2 :align-x :center :align-y :top)

    ;; Draw the Y labels
    (loop with line-ink = (make-rgb-color 0.8 0.8 0.8)
          for i from 0 to y-label-count
          ;; Count based on the pre-truncated value (basic-val) to avoid skew
          for basic-val = minimum then (+ basic-val (/ variance y-label-count))
          for value = (truncate basic-val)
          for ypos = (funcall fn-y value)
          do (draw-text* pane (format nil "~a" value) 10 ypos)
             (draw-line* pane left-x-padding ypos width ypos :ink line-ink))

    ;; Draw the graph data
    (reduce (lambda (acc yval)
              (let ((xpos (funcall step-fn-x))
                    (ypos (funcall fn-y yval)))
                (when (draw-values frame)
                  (let ((label (format nil "~a" yval)))
                    ;; Use :align-x :right for the rightmost labels so
                    ;; the label text cannot extend beyond the line
                    ;; for the data point (otherwise the pane and thus
                    ;; frame could grow).
                    (draw-text* pane label xpos ypos
                                :align-x (if (> xpos (* width 3/4))
                                             :right
                                             :center)
                                :align-y :bottom)))
                (unless (null acc)
                  (draw-line* pane (car acc) (cdr acc) xpos ypos))
                (cons xpos ypos)))
            values :initial-value nil)
    (format stream "Length: ~a~%" (length (val-array frame)))))

(define-application-frame graph-toy ()
  ((val-array :initform (make-array 8 :adjustable t :fill-pointer 0) :accessor val-array)
   (title :initform "Clim Example Graph" :initarg :title :accessor title)
   (max-xvals :initform 50 :initarg :max-xvals :accessor max-xvals)
   (last-yval :initform 20 :accessor last-yval)
   (draw-values :initform t :initarg :draw-values :accessor draw-values))
  (:menu-bar nil)
  (:default-initargs
   :width 800
   :height 600)
  (:panes
   (main-display
    :application
    :scroll-bars nil
    :display-function 'display-main
    :display-time :command-loop))
  (:layouts (default main-display)))

(defclass new-value-event (window-manager-event)
  ((val :initarg :val :accessor val)))

(defmethod handle-event ((frame graph-toy) (event new-value-event))
  (when (eq (frame-state frame) :enabled)
    (vector-push-extend (val event) (val-array frame))
    (let ((val-array-len (length (val-array frame)))
          (xval-count (max-xvals frame)))
      (when (>= val-array-len (* xval-count 2))
        (loop for i from 0 to (- xval-count 1)
           for j from (- val-array-len xval-count)
           do (setf (elt (val-array frame) i) (elt (val-array frame) j)))
        (setf (fill-pointer (val-array frame)) xval-count))
      (redisplay-frame-pane frame 'main-display)
      (incf (last-yval frame) (- (random 9) 4))
      (when (member (frame-state frame) '(:enabled :shrunk))
        (schedule-event (frame-top-level-sheet frame)
                        (make-instance 'new-value-event :sheet frame
                                       :val (last-yval frame))
                        0.5)))))

(defmethod run-frame-top-level :before ((graph graph-toy) &key)
  (let* ((sheet (frame-top-level-sheet graph))
         (event (make-instance 'new-value-event :sheet graph
                                                :val (last-yval graph))))
    (queue-event sheet event)))
