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
;;; 3.2.3 Lines

(in-package #:silex)

(defclass standard-line (line)
  ((x1 :type coordinate :initarg :x1)
   (y1 :type coordinate :initarg :y1)
   (x2 :type coordinate :initarg :x2)
   (y2 :type coordinate :initarg :y2)))

(defmethod slots-for-pprint-object append ((object standard-line))
  '(x1 y1 x2 y2))

(defmethod print-object ((region standard-line) sink)
  (maybe-print-readably (region sink)
    (print-unreadable-object (region sink :identity nil :type t)
         (with-slots (x1 y1 x2 y2) region
           (format sink "~D ~D ~D ~D" x1 y1 x2 y2)))))

(defun make-line (start-point end-point)
  (make-line* (point-x start-point) (point-y start-point)
              (point-x end-point) (point-y end-point)))

(defun make-line* (start-x start-y end-x end-y)
  (setf start-x (coordinate start-x)
        start-y (coordinate start-y)
        end-x (coordinate end-x)
        end-y (coordinate end-y))
  (if (and (coordinate= start-x end-x)
           (coordinate= start-y end-y))
      +nowhere+
      (make-instance 'standard-line :x1 start-x :y1 start-y :x2 end-x :y2 end-y)))

(defmethod line-start-point* ((line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (values x1 y1)))

(defmethod line-end-point* ((line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (values x2 y2)))

(defmethod line-start-point ((line line))
  (multiple-value-bind (x y) (line-start-point* line)
    (make-point x y)))

(defmethod line-end-point ((line line))
  (multiple-value-bind (x y) (line-end-point* line)
    (make-point x y)))

;;; Polyline protocol for standard-line's

(defmethod polygon-points ((line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (list (make-point x1 y1) (make-point x2 y2))))

(defmethod map-over-polygon-coordinates (fun (line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (funcall fun x1 y1)
    (funcall fun x2 y2)))

(defmethod map-over-polygon-segments (fun (line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (funcall fun x1 y1 x2 y2)))

(defmethod polyline-closed ((line standard-line))
  nil)

(defmethod region-contains-position-p ((region line) x y)
  (multiple-value-bind (x1 y1) (line-start-point* region)
    (multiple-value-bind (x2 y2) (line-end-point* region)
      (segment-contains-point-p x1 y1 x2 y2 x y))))

(defmethod bounding-rectangle* ((a standard-line))
  (with-slots (x1 y1 x2 y2) a
    (values (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2))))

(defmethod transform-region (transformation (line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (multiple-value-bind (x1* y1*) (transform-position transformation x1 y1)
      (multiple-value-bind (x2* y2*) (transform-position transformation x2 y2)
        (make-line* x1* y1* x2* y2*)))))
