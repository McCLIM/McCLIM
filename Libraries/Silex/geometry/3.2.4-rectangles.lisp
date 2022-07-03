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
;;; 3.2.4 Rectangles

(in-package #:silex)

(defclass standard-rectangle (rectangle)
  ((coordinates :initform (make-array 4 :element-type 'coordinate))))

(defmethod make-load-form ((object standard-rectangle) &optional environment)
  (make-load-form-saving-slots object :slot-names '(coordinates)
                                      :environment environment))

(defmethod simple-pprint-object-args (stream (object standard-rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* object)
    (loop for (slot-name slot-value) in `((x1 ,x1)
                                          (y1 ,y1)
                                          (x2 ,x2)
                                          (y2 ,y2))
          do
             (write-char #\Space stream)
             (pprint-newline :fill stream)
             (write-char #\: stream)
             (princ slot-name stream)
             (write-char #\Space stream)
             (unless (atom slot-value)
               (princ "'" stream))
             (write slot-value :stream stream))))

(defmethod print-object ((region standard-rectangle) stream)
  (maybe-print-readably (region stream)
    (print-unreadable-object (region stream :type t :identity nil)
      (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* region)
        (format stream "X ~S:~S Y ~S:~S" x1 x2 y1 y2)))))

(defmethod initialize-instance :after ((obj standard-rectangle)
                                       &key (x1 0.0d0) (y1 0.0d0)
                                         (x2 0.0d0) (y2 0.0d0))
  (let ((coords (slot-value obj 'coordinates)))
    (declare (type standard-rectangle-coordinate-vector coords))
    (setf (aref coords 0) x1)
    (setf (aref coords 1) y1)
    (setf (aref coords 2) x2)
    (setf (aref coords 3) y2)))

;;; - VARIABLES before first keyword are positional and correspond to
;;;   X1, Y1, X2, Y2.
;;;   - Fewer than all four can be provided.
;;;   - Any of the positional variables can be `nil' indicating that
;;;     the binding should not be established.
;;; - The first keyword initiates the keyword part of the variable
;;;   list (can start after between zero and four positional
;;;   variables).
(defmacro with-standard-rectangle* ((&rest variables) rectangle &body body)
  (let* ((index      (position-if #'keywordp variables))
         (positional (subseq variables 0 index))
         (keyword    (when index
                       (subseq variables index))))
    (destructuring-bind (&key (x1 (nth 0 positional))
                              (y1 (nth 1 positional))
                              (x2 (nth 2 positional))
                              (y2 (nth 3 positional))
                              width height center-x center-y)
        keyword
      (declare (ignore width height center-x center-y))
      (with-gensyms (coords)
        `(let ((,coords (slot-value ,rectangle 'coordinates)))
           (declare (type standard-rectangle-coordinate-vector ,coords))
           ,(generate-rectangle-bindings
             (list* :x1 x1 :y1 y1 :x2 x2 :y2 y2 keyword)
             `((aref ,coords 0) (aref ,coords 1) (aref ,coords 2) (aref ,coords 3))
             body))))))

(defmacro with-grown-rectangle* (((out-x1 out-y1 out-x2 out-y2)
                                  (in-x1 in-y1 in-x2 in-y2)
                                  &key
                                  radius
                                  (radius-x radius)
                                  (radius-y radius)
                                  (radius-left  radius-x)
                                  (radius-right radius-x)
                                  (radius-top    radius-y)
                                  (radius-bottom radius-y))
                                  &body body)
  `(multiple-value-bind (,out-x1 ,out-y1 ,out-x2 ,out-y2)
    (values (- ,in-x1 ,radius-left)
     (- ,in-y1 ,radius-top)
     (+ ,in-x2 ,radius-right)
     (+ ,in-y2 ,radius-bottom))
    ,@body))

(defun make-rectangle (point1 point2)
  (make-rectangle* (point-x point1) (point-y point1)
                   (point-x point2) (point-y point2)))

(defun make-rectangle* (x1 y1 x2 y2)
  (let ((x1 (coordinate x1))
        (y1 (coordinate y1))
        (x2 (coordinate x2))
        (y2 (coordinate y2)))
    (multiple-value-bind (x1 x2)
        (cond ((= x1 x2) (return-from make-rectangle* +nowhere+))
              ((< x1 x2) (values x1 x2))
              (t         (values x2 x1)))
      (multiple-value-bind (y1 y2)
          (cond ((= y1 y2) (return-from make-rectangle* +nowhere+))
                ((< y1 y2) (values y1 y2))
                (t         (values y2 y1)))
        ;; XXX: This seems to not be right. -- jd 2019-09-30
        (make-instance 'standard-bounding-rectangle :x1 x1 :y1 y1 :x2 x2 :y2 y2)))))

(defmethod rectangle-edges* ((rect standard-rectangle))
  (with-standard-rectangle* (x1 y1 x2 y2) rect
    (values x1 y1 x2 y2)))

;;; standard-rectangles are immutable and all that, but we still need
;;; to set their positions and dimensions (in output recording)
(defgeneric* (setf rectangle-edges*) (x1 y1 x2 y2 rectangle))

(defmethod* (setf rectangle-edges*)
  (x1 y1 x2 y2 (rectangle standard-rectangle))
  (let ((coords (slot-value rectangle 'coordinates)))
    (declare (type standard-rectangle-coordinate-vector coords))
    (setf (aref coords 0) x1)
    (setf (aref coords 1) y1)
    (setf (aref coords 2) x2)
    (setf (aref coords 3) y2))
  (values x1 y1 x2 y2))

(macrolet
    ((def (name &rest parts)
       (destructuring-bind (first-part &optional second-part) parts
         (let ((result-form (if (not second-part)
                                (second first-part)
                                `(,(if (eq (first first-part) :width)
                                       'values
                                       'make-point)
                                  ,(second first-part)
                                  ,(second second-part)))))
           `(progn
              (defmethod ,name ((rect standard-rectangle))
                (with-standard-rectangle* (,@(apply #'append parts)) rect
                  ,result-form))

              (defmethod ,name ((rect rectangle))
                (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rect)
                  (declare (type coordinate x1 y1 x2 y2)
                           (ignorable x1 y1 x2 y2))
                  ,(generate-rectangle-bindings
                    (apply #'append parts)
                    '(x1 y1 x2 y2)
                    `(,result-form)))))))))
  (def rectangle-min-point (:x1 x1) (:y1 y1))
  (def rectangle-max-point (:x2 x2) (:y2 y2))
  (def rectangle-min-x     (:x1 x1))
  (def rectangle-min-y     (:y1 y1))
  (def rectangle-max-x     (:x2 x2))
  (def rectangle-max-y     (:y2 y2))
  (def rectangle-width     (:width  width))
  (def rectangle-height    (:height height))
  (def rectangle-size      (:width  width) (:height height)))

;;; Polyline/polygon protocol for STANDARD-RECTANGLEs

(defmethod polygon-points ((rect standard-rectangle))
  (with-standard-rectangle* (x1 y1 x2 y2) rect
    (list (make-point x1 y1)
          (make-point x1 y2)
          (make-point x2 y2)
          (make-point x2 y1))))

(defmethod map-over-polygon-coordinates (fun (rect standard-rectangle))
  (with-standard-rectangle* (x1 y1 x2 y2) rect
    (funcall fun x1 y1)
    (funcall fun x1 y2)
    (funcall fun x2 y2)
    (funcall fun x2 y1)))

(defmethod map-over-polygon-segments (fun (rect standard-rectangle))
  (with-standard-rectangle* (x1 y1 x2 y2) rect
    (funcall fun x1 y1 x1 y2)
    (funcall fun x1 y2 x2 y2)
    (funcall fun x2 y2 x2 y1)
    (funcall fun x2 y1 x1 y1)))

(defmethod region-contains-position-p ((region rectangle) x y)
  (multiple-value-bind (x1 y1 x2 y2)
      (rectangle-edges* region)
    (and (coordinate-between* x1 x x2)
         (coordinate-between* y1 y y2))))

(defmethod region-contains-position-p ((region standard-rectangle) x y)
  (with-standard-rectangle* (x1 y1 x2 y2) region
    (and (coordinate-between* x1 x x2)
         (coordinate-between* y1 y y2))))

(defmethod bounding-rectangle* ((region standard-rectangle))
  (with-standard-rectangle* (x1 y1 x2 y2) region
    (values x1 y1 x2 y2)))

(defmethod transform-region (transformation (rect standard-rectangle))
  (cond ((rectilinear-transformation-p transformation)
         (with-standard-rectangle* (x1 y1 x2 y2) rect
           (multiple-value-bind (x1* y1*)
               (transform-position transformation x1 y1)
             (multiple-value-bind (x2* y2*)
                 (transform-position transformation x2 y2)
               (make-rectangle* x1* y1* x2* y2*)))))
        (t
         (make-polygon (mapcar (lambda (p) (transform-region transformation p))
                               (polygon-points rect))))))
