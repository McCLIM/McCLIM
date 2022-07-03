;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2002 Gilbert Baumann <gbaumann@common-lisp.net>
;;;  (c) copyright 2001 Arnaud Rouanet <rouanet@emi.u-bordeaux.fr>
;;;  (c) copyright 2001 Julien Boninfan
;;;  (c) copyright 2002-2004 Timothy Moore <tmoore@common-lisp.net>
;;;  (c) copyright 2002 Alexey Dejneka
;;;  (c) copyright 2004-2009 Andy Hefner <ahefner@common-lisp.net>
;;;  (c) copyright 2006-2008 Christophe Rhodes <crhodes@common-lisp.net>
;;;  (c) copyright 2014-2016 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2017 Peter <craven@gmx.net>
;;;  (c) copyright 2017-2019 Daniel Kochmański <daniel@turtleware.eu>
;;;  (c) copyright 2017,2018 Cyrus Harmon <cyrus@bobobeach.com>
;;;  (c) copyright 2018,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; 3.2.2 Polygons and Polylines

(in-package #:silex)

(defclass standard-polyline (cached-bbox-mixin polyline)
  ((points :initarg :points :reader polygon-points)
   (closed :initarg :closed)))

(defclass standard-polygon (cached-bbox-mixin polygon)
  ((points :initarg :points :reader polygon-points)))

(defmethod slots-for-pprint-object append ((object standard-polyline))
  '(points closed))

(defmethod slots-for-pprint-object append ((object standard-polygon))
  '(points))

(defmethod print-object ((region standard-polyline) sink)
  (maybe-print-readably (region sink)
    (print-unreadable-object (region sink :identity t :type t))))

(defmethod print-object ((region standard-polygon) sink)
  (maybe-print-readably (region sink)
    (print-unreadable-object (region sink :identity t :type t))))

(defun make-polyline (point-seq &key closed)
  (assert (every #'pointp point-seq))
  (setf point-seq (clean-up-polyline-points point-seq closed))
  (if (< (length point-seq) 2)
      +nowhere+
      (make-instance 'standard-polyline :points point-seq :closed closed)))

(defun make-polyline* (coord-seq &key closed)
  (make-polyline (coord-seq->point-seq coord-seq) :closed closed))

(defun make-polygon (point-seq)
  (assert (every #'pointp point-seq))
  (setf point-seq (clean-up-polygon-points point-seq))
  (if (< (length point-seq) 3)
      +nowhere+
      (make-instance 'standard-polygon :points point-seq)))

(defun make-polygon* (coord-seq)
  (make-polygon (coord-seq->point-seq coord-seq)))

(defun make-regular-polygon (cx cy r n)
  (collect (coords)
    (loop repeat n
          with x0 = cx
          with y0 = (- cy r)
          for angle from 0 by (/ (* 2 pi) n)
          for tr = (make-rotation-transformation* angle cx cy)
          do (multiple-value-bind (x1 y1) (transform-position tr x0 y0)
               (coords (make-point x1 y1))))
    (make-polygon (coords))))

(defmethod map-over-polygon-coordinates (fun (region standard-polygon))
  (with-slots (points) region
    (mapc (lambda (p) (funcall fun (point-x p) (point-y p))) points)))

(defmethod map-over-polygon-segments (fun (region standard-polygon))
  (with-slots (points) region
    (do ((q points (cdr q)))
        ((null (cdr q))
         (funcall fun
                  (point-x (car q)) (point-y (car q))
                  (point-x (car points)) (point-y (car points))))
      (funcall fun
               (point-x (car q)) (point-y (car q))
               (point-x (cadr q)) (point-y (cadr q))))))

(defmethod map-over-polygon-coordinates (fun (region standard-polyline))
  (with-slots (points) region
    (mapc (lambda (p) (funcall fun (point-x p) (point-y p))) points)))

(defmethod map-over-polygon-segments (fun (region standard-polyline))
  (with-slots (points closed) region
    (do ((q points (cdr q)))
        ((null (cdr q))
         (when closed
           (funcall fun
                    (point-x (car q)) (point-y (car q))
                    (point-x (car points)) (point-y (car points)))))
      (funcall fun (point-x (car q)) (point-y (car q))
               (point-x (cadr q)) (point-y (cadr q))))))

(defmethod polyline-closed ((region standard-polyline))
  (with-slots (closed) region
    closed))

(defmethod bezigon-points ((object polyline))
  (polygon-points object))

(defmethod bezigon-points ((object polygon))
  (polygon-points object))

(defmethod bezigon-order ((object polyline))
  (declare (ignore object))
  2)

(defmethod bezigon-order ((object polygon))
  (declare (ignore object))
  2)

(defmethod map-over-bezigon-segments (function (object polyline))
  (map-over-polygon-segments function object))

(defmethod map-over-bezigon-segments (function (object polygon))
  (map-over-polygon-segments function object))

(defmethod region-contains-position-p ((region polyline) x y)
  (setf x (coordinate x)
        y (coordinate y))
  (block nil
    (map-over-polygon-segments
     (lambda (x1 y1 x2 y2)
       (when (segment-contains-point-p x1 y1 x2 y2 x y)
         (return t)))
     region)
    nil))

(defmethod region-contains-position-p ((region polygon) x y)
  (let ((insidep nil))
    (and (region-contains-position-p (bounding-rectangle region) x y)
         (progn
           (map-over-polygon-segments
            (lambda (x1 y1 x2 y2)
              (when (segment-contains-point-p x1 y1 x2 y2 x y)
                (return-from region-contains-position-p t))
              (unless (< y1 y2)
                (rotatef y1 y2)
                (rotatef x1 x2))
              (when (and (not (eq (> y1 y) (> y2 y)))
                         (minusp (line-equation x1 y1 x2 y2 x y)))
                (setf insidep (not insidep))))
            region)
           insidep))))

(defun polygon-bounding-rectangle (region)
  (loop for point in (polygon-points region)
        for (x y) = (multiple-value-list (point-position point))
        minimizing x into x1 maximizing x into x2
        minimizing y into y1 maximizing y into y2
        finally (return (values x1 y1 x2 y2))))

(defmethod bounding-rectangle* ((region standard-polyline))
  (polygon-bounding-rectangle region))

(defmethod bounding-rectangle* ((region standard-polygon))
  (polygon-bounding-rectangle region))

(defmethod transform-region (transformation (region standard-polyline))
  (with-slots (points closed) region
    (make-polyline
     (mapcar (lambda (p)
               (multiple-value-bind (x* y*)
                   (transform-position transformation (point-x p) (point-y p))
                 (make-point x* y*)))
             points)
     :closed closed)))

(defmethod transform-region (transformation (region standard-polygon))
  (with-slots (points) region
    (make-polygon
     (mapcar (lambda (p)
               (multiple-value-bind (x* y*)
                   (transform-position transformation (point-x p) (point-y p))
                 (make-point x* y*)))
             points))))
