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
;;; 3.2.1 Points

(in-package #:clim-internals)

(defclass standard-point (point)
  ((x :type coordinate :initarg :x)
   (y :type coordinate :initarg :y)))

(defun make-point (x y)
  (make-instance 'standard-point
    :x (coordinate x)
    :y (coordinate y)))

(defmethod slots-for-pprint-object append ((object standard-point))
  '(x y))

(defmethod print-object ((region standard-point) sink)
  (maybe-print-readably (region sink)
    (print-unreadable-object (region sink :identity nil :type t)
      (with-slots (x y) region
        (format sink "~S ~S" x y)))))

(defmethod point-position ((region standard-point))
  (with-slots (x y) region
    (values x y)))

(defmethod point-x ((region point))
  (nth-value 0 (point-position region)))

(defmethod point-y ((region point))
  (nth-value 1 (point-position region)))

(defmethod region-contains-position-p ((region point) x y)
  (multiple-value-bind (px py) (point-position region)
    (and (coordinate= px x)
         (coordinate= py y))))

(defmethod bounding-rectangle* ((region standard-point))
  (with-slots (x y) region
    (values x y x y)))

(defmethod transform-region (transformation (region standard-point))
  (with-slots (x y) region
    (multiple-value-bind (x* y*) (transform-position transformation x y)
      (make-point x* y*))))
