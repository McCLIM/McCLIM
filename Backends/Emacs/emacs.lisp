;;; emacs.lisp -- Experimental McCLIM backend for Emacs (via SLIME)
;;;
;;; This file provides a backend to render McCLIM graphics in Emacs via SLIME.
;;; CLIM drawing operations are mapped onto an SVG canvas and shipped to Emacs
;;; via socket connection.
;;;
;;; This is all in one file for the moment because I find that I get lost
;;; otherwise.
;;;
;;; - [X] Define Emacs backend with SVG medium
;;; - [X] Display results in SLIME REPL
;;; - [ ] Drawing operations
;;;   - [X] Rectangle
;;;   - [X] Polygon
;;;   - [ ] Circle
;;;   - [X] Ellipse
;;;   - [ ] Line
;;;   - [ ] Points
;;;   - [ ] Text
;;; - [ ] Support mouse events
;;; - [ ] Support mouse-sensitive regions for accepting input
;;; - [ ] Commands
;;; - [ ] Menus
;;; - [ ] ... what else?

(in-package #:common-lisp-user)
(defpackage #:clim-emacs
  (:use #:clim #:climi #:clime #:climb #:clim-lisp)
  (:import-from #:climi #:left #:right #:top #:bottom
                #:filled #:ink
                #:center-x #:center-y
                #:radius-1-dx #:radius-1-dy
                #:radius-2-dx #:radius-2-dy
                #:draw-rectangle-output-record #:draw-ellipse-output-record
                #:draw-polygon-output-record #:draw-text-output-record
                #:draw-point-output-record #:draw-points-output-record))



(in-package #:clim-emacs)
(declaim (optimize (debug 3) (safety 3) (speed 1)))


;;;; Port

(defvar *emacs-command-table* (make-command-table "Emacs"))

(defclass emacs-port (basic-port)
  ((id)))

(defmethod find-port-type ((type (eql :emacs)))
  (values 'emacs-port 'identity))

(defmethod initialize-instance :after ((port emacs-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "EMACS-PORT-")))


;;;; Medium

(defclass emacs-medium (basic-medium)
  ())

(defmethod make-medium ((port emacs-port) sheet)
  (make-instance 'emacs-medium :port port :sheet sheet))

(defmethod medium-draw-rectangle* ((medium emacs-medium) x1 y1 x2 y2 filled))
(defmethod medium-draw-polygon* ((medium emacs-medium) coord-seq closed filled))
(defmethod medium-draw-ellipse* ((medium emacs-medium) cx cy r1dx r1dy r2dx r2dy sa ea filled))
  

;;;; Stream

(defclass clim-emacs-stream (sheet-leaf-mixin
                             sheet-parent-mixin
                             sheet-transformation-mixin
                             sheet-mute-input-mixin
                             sheet-mute-repainting-mixin
                             climi::updating-output-stream-mixin
                             basic-sheet
                             standard-extended-output-stream
                             extended-input-stream
                             permanent-medium-sheet-output-mixin
                             standard-output-recording-stream)
  ((port :initform nil :initarg port :accessor port)))
  
(defmacro with-output-to-emacs ((stream-var) &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,stream-var)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-emacs-stream #',cont))))

(defun invoke-with-output-to-emacs-stream (continuation)
  (with-port (port :emacs)
    (let ((stream (make-instance 'clim-emacs-stream :port port)))
      (sheet-adopt-child (find-graft :port port) stream)
      (prog1 (funcall continuation stream)
        (let ((output (stream-output-history stream)))
          (swank::send-to-emacs (list :write-clime
                                      (output-record-to-svg output)
                                      (presentations-for-emacs stream))))))))

(defun output-record-to-svg (record)
  (multiple-value-bind (x-min y-min x-max y-max) (bounding-rectangle* record)
    (let ((width  (ceiling (- x-max x-min)))
          (height (ceiling (- y-max y-min))))
      (shapes-to-svg (output-history-shapes record) width height))))

(defun presentations-for-emacs (stream)
  (let (ids)
    (multiple-value-bind (x0 y0) (bounding-rectangle* (stream-output-history stream))
      (labels ((visit (record)
                 (when (typep record 'presentation)
                   (push (list (register-presentation record)
                               (emacs-map-area record x0 y0)
                               (tooltip record))
                         ids))
                 (map-over-output-records #'visit record)))
        (visit (stream-output-history stream))))
    ids))

(defun emacs-map-area (record x0 y0)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* record)
    ;; Syntax follows https://www.gnu.org/software/emacs/manual/html_node/elisp/Image-Descriptors.html
    (let ((left   (floor (- x1 x0)))
          (top    (floor (- y1 y0)))
          (right  (ceiling (- x2 x0)))
          (bottom (ceiling (- y2 y0))))
      (cons '#:rect (cons (cons left top) (cons right bottom))))))

(defvar *presentations* (make-array 0 :adjustable t :fill-pointer 0)
  "Vector of presentations (identified by index.)")

(defun register-presentation (presentation)
  (vector-push-extend presentation *presentations*))

(defmethod stream-accept (stream type &rest keywords)
  (declare (ignore keywords))
  (swank:y-or-n-p-in-emacs "STREAM-ACCEPT"))


;;;; Tooltips

(defgeneric tooltip (presentation)
  (:documentation "Return a tooltip string describing PRESENTATION.")
  (:method ((p presentation))
    (with-output-to-string (s)
      (let ((*print-right-margin* 60))
        (cl:describe (presentation-object p) s)))))


;;;; Output records

(defvar *debug-output-tree* nil
  "Most recently processed output tree, for debugging purposes.")

;;; Convert McCLIM's internal output record format into a simple list
;;; representation with (0,0) as the upper-left corner.

(defun output-history-shapes (root)
  "Return the list of shapes in the output history rooted at ROOT."
  (setf *debug-output-tree* root)
  (let (shapes)
    (multiple-value-bind (x-min y-min x-max y-max) (bounding-rectangle* root)
      (assert (< x-min x-max))
      (assert (< y-min y-max))
      (map-over-output-record-tree (lambda (record)
                                     (push (output-record-to-list record x-min y-min) shapes))
                                   root)
      (remove nil shapes))))

(defun map-over-output-record-tree (fn record)
  "Call FN on RECORD and all descendents of RECORD."
  (flet ((visit (child)
           (map-over-output-record-tree fn child)))
  (funcall fn record)
  (map-over-output-records #'visit record)))
  
(defun output-record-to-list (record &optional (x-min 0) (y-min 0))
  "Return a simple list representation of an output record.
   Optionally translate coordinates relative to the given origin.
   Return NIL if RECORD is not a recognized shape-drawing output record."
  (labels ((x (x) (- x x-min))   ;; Translate top-left corner to (0,0)
           (y (y) (- y y-min)))
    (typecase record
      (draw-rectangle-output-record
       (with-slots (left top right bottom filled ink) record
         ;; NB: left/right and top/bottom positions aren't dependable
         (list :rectangle
               (x (min left right))
               (y (min bottom top))
               (abs (- right left))
               (abs (- bottom top))
               filled
               ink)))
      (draw-ellipse-output-record
       (with-slots (center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy filled ink)
           record
         (flet ((distance (x y)
                  (sqrt (+ (expt x 2) (expt y 2)))))
           (list :ellipse (x center-x) (y center-y)
                 (distance radius-1-dx radius-1-dy)
                 (distance radius-2-dx radius-2-dy)
                 filled ink)))))))


;;;; SVG

;;; Convert the list representation into SVG shapes.

(defun shapes-to-svg (shapes width height)
  (with-output-to-string (stream)
    (format stream "~&<svg viewBox='0 0 ~D ~D' xmlns='http://www.w3.org/2000/svg'>~%"
            (ceiling width) (ceiling height))
    (loop for shape in shapes do (format-svg shape stream))
    (format stream "~&</svg>~%")))

(defun format-svg (shape &optional stream)
  "Print SHAPE to STREAM in SVG format.
   If STREAM is NIL then return the SVG shape as a string."
  (alexandria:destructuring-ecase shape
    ((:rectangle x y w h filled ink)
     (format stream "~&<rect x='~F' y='~F' width='~F' height='~F' fill='~A' stroke='~A'/>~%"
             x y w h
             (svg-color (if filled ink nil))
             (svg-color (if filled nil ink))))
    ((:ellipse cx cy r1 r2 filled ink)
     (format stream "~&<ellipse cx='~F' cy='~F' rx='~F' ry='~F' fill='~A' stroke='~A'/>~%"
             cx cy r1 r2
             (svg-color (if filled ink nil))
             (svg-color (if filled nil ink))))))

(defun svg-color (ink)
  "Return an SVG color string representing INK (which may be NIL.)"
  (if ink
      (multiple-value-bind (r g b a) (color-rgba ink)
        (format nil "rgb(~f%, ~f%, ~f%, ~f)" (* r 100) (* g 100) (* b 100) a))
      "none"))

