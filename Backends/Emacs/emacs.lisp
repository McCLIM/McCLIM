;;; emacs.lisp -- Experimental McCLIM backend for Emacs (via SLIME)
;;;
;;; This file provides a backend to render McCLIM graphics in Emacs via SLIME.
;;; CLIM drawing operations are mapped onto an SVG canvas and shipped to Emacs
;;; via socket connection.
;;;
;;; Depends on CL-SVG from Quicklisp.
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
  (:local-nicknames (#:svg #:cl-svg)))
(in-package #:clim-emacs)
(declaim (optimize (debug 3) (safety 3) (speed 1)))


;;;; ----------------------------------------------------------------------
;;;; Port
;;;; ----------------------------------------------------------------------

(defvar *emacs-command-table* (make-command-table "Emacs"))

(defclass emacs-port (basic-port)
  ((id)))

(defmethod find-port-type ((type (eql :emacs)))
  (values 'emacs-port 'identity))

(defmethod initialize-instance :after ((port emacs-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "EMACS-PORT-")))


;;;; ----------------------------------------------------------------------
;;;; Medium
;;;; ----------------------------------------------------------------------

(defclass emacs-medium (basic-medium)
  ((scene :initform (svg:make-svg-toplevel 'svg:svg-1.1-toplevel))))

(defmethod make-medium ((port emacs-port) sheet)
  (make-instance 'emacs-medium :port port :sheet sheet))

(defmethod medium-draw-rectangle* ((medium emacs-medium) x1 y1 x2 y2 filled)
  (let* ((color (svg-color (medium-ink medium)))
         (clear (svg-color +transparent-ink+)))
    (svg:draw (slot-value medium 'scene)
              (:rect :x x1 :y y1 :width (- x2 x1) :height (- y2 y1)
                     :fill (if filled color clear)
                     :stroke (if filled clear color)))))

(defmethod medium-draw-polygon* ((medium emacs-medium) coord-seq closed filled)
  (svg:draw (slot-value medium 'scene)
            (:polygon :points (coord-seq-to-coord-list coord-seq))))


(defmethod medium-draw-ellipse* ((medium emacs-medium) center-x center-y
                                 radius-1-dx radius-1-dy
                                 radius-2-dx radius-2-dy
                                 start-angle end-angle filled)
  (flet ((distance (x y)
           (sqrt (+ (expt x 2) (expt y 2)))))
    (let* ((color (svg-color (medium-ink medium)))
           (clear (svg-color +transparent-ink+))
           (rx (distance radius-1-dx radius-1-dy))
           (ry (distance radius-2-dx radius-2-dy)))
      (svg:draw (slot-value medium 'scene)
                (:ellipse :cx center-x :cy  center-y :rx rx :ry ry
                          :stroke (if filled clear color)
                          :fill   (if filled color clear))))))

(defun svg-color (ink)
  (multiple-value-bind (r g b a) (color-rgba ink)
    (format nil "rgb(~f%, ~f%, ~f%, ~f)"
            (* r 100) (* g 100) (* b 100) a)))

(defun coord-seq-to-coord-list (seq)
  "Convert sequence (X1 Y2 ... Xn Yn) to list ((X1 X2) ... (Xn Yn))."
  (loop for (x y) on (coerce seq 'list) by #'cddr
        collect (list x y)))

(defun svg (medium)
  (slot-value medium 'scene))
  
;; medium attributes:
;;   medium-foreground medium-background ink transformation clipping-region
;;   line-style default-text-style text-style current-text-style
  

;;;; ----------------------------------------------------------------------
;;;; Stream
;;;; ----------------------------------------------------------------------

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
        (let* ((scene (slot-value (sheet-medium stream) 'scene)))
          (multiple-value-bind (min-x min-y max-x max-y)
              (bounding-rectangle* (stream-output-history stream))
            (push (format nil "~A ~A ~A ~A" min-x min-y (- max-x min-x) (- max-y min-y))
                  (slot-value scene 'svg::attributes))
            (push :view-box (slot-value scene 'svg::attributes))
            (clim:map-over-output-records
             (lambda (x) (format swank::*current-standard-output* "~A" x) (finish-output *terminal-io*))
             (stream-output-history stream))
            (swank::send-to-emacs (list :write-clime 
                                        (with-output-to-string (svg)
                                          (svg:stream-out svg scene))
                                        (presentations-for-emacs stream)))))))))

(defun presentations-for-emacs (stream)
  (let (ids)
    (labels ((visit (record)
               (when (typep record 'presentation)
                 (push (list (register-presentation record) (emacs-map-area record))
                       ids))
               (map-over-output-records #'visit record)))
      (visit (stream-output-history stream)))
    ids))

(defun emacs-map-area (record)
  (multiple-value-bind (x0 y0 x1 y2) (bounding-rectangle* record)
    ;; Syntax follows https://www.gnu.org/software/emacs/manual/html_node/elisp/Image-Descriptors.html
    `(#:rect . ((,x0 ,y0) . (,x1 ,y2)))))

(defvar *presentations* (make-array 0 :adjustable t :fill-pointer 0)
  "Vector of presentations (identified by index.)")

(defun register-presentation (presentation)
  (vector-push-extend presentation *presentations*))

(defmethod stream-accept (stream type &rest keywords)
  (declare (ignore keywords))
  (swank:y-or-n-p-in-emacs "STREAM-ACCEPT"))
