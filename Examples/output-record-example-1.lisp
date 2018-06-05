(cl:in-package #:common-lisp-user)

(defpackage #:output-record-example-1
  (:use #:clim-lisp))

(in-package #:output-record-example-1)

(defclass list-output-history (clim:stream-output-history-mixin)
  ((%children :initform '() :accessor children)))

(defmethod clim:bounding-rectangle* ((region list-output-history))
  (values 0 0 100 100))

(defmethod clim:clear-output-record ((record list-output-history))
  (setf (children record) '()))

(defmethod clim:replay-output-record ((record list-output-history) stream
                                      &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (loop for child in (children record)
        do (clim:replay-output-record child stream)))

(defmethod clim:map-over-output-records-containing-position
    (function (record list-output-history) x y
     &optional x-offset y-offset
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (loop for child in (children record)
        do (clim:with-bounding-rectangle* (min-x min-y max-x max-y) child
             (when (and (<= min-x x max-x) (<= min-y y max-y))
               (funcall function function-args)))))

(defclass circle-output-record (clim:graphics-displayed-output-record)
  ((%center-x :initarg :center-x :accessor center-x)
   (%center-y :initarg :center-y :accessor center-y)))

(defmethod clim:bounding-rectangle* ((region circle-output-record))
  (let ((center-x (center-x region))
        (center-y (center-y region)))
    (values (- center-x 20)
            (- center-y 20)
            (+ center-x 20)
            (+ center-y 20))))

(defmethod clim:replay-output-record ((record circle-output-record) stream
                                      &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (clim:with-bounding-rectangle* (min-x min-y max-x max-y) record
    (clim:draw-ellipse* stream
                        (/ (+ min-x max-x) 2) (/ (+ min-y max-y) 2)
                        (/ (- max-x min-x) 3) 0
                        0 (/ (- max-y min-y) 3)
                        :filled t
                        :ink clim:+red+)))

(defclass pane (clim:application-pane)
  ()
  (:default-initargs
   :output-record (make-instance 'list-output-history)
   :display-time nil))

(clim:define-application-frame output-record-example-1 ()
  ()
  (:panes (application (clim:make-pane 'pane :scroll-bars nil))
          (interactor :interactor))
  (:layouts (default (clim:vertically ()
                       (7/10 application)
                       (3/10 interactor)))))

(define-output-record-example-1-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-output-record-example-1-command (com-insert-circle :name t)
    ((x 'integer) (y 'integer))
  (let ((pane (clim:find-pane-named clim:*application-frame* 'application )))
    (push (make-instance 'circle-output-record
            :center-x x
            :center-y y
            :parent (clim:stream-output-history pane))
          (children (clim:stream-output-history pane)))))

(define-output-record-example-1-command (com-move :name t)
    ((x 'integer) (y 'integer))
  (let* ((pane (clim:find-pane-named clim:*application-frame* 'application ))
         (history (clim:stream-output-history pane))
         (first (first (children history))))
    (setf (center-x first) x
          (center-y first) y)))

(defun output-record-example-1 ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'output-record-example-1)))
  
(defmethod clim:redisplay-frame-panes :after
    ((frame output-record-example-1) &key force-p)
  (declare (ignore force-p))
  (let ((pane (clim:find-pane-named clim:*application-frame* 'application )))
    (clim:with-output-recording-options (pane :record nil :draw t)
      (clim:draw-rectangle* pane 0 0 1000 1000
                            :filled t
                            :ink clim:+background-ink+))
    (clim:replay (clim:stream-output-history pane) pane)))
