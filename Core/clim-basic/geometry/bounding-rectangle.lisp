;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2000 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 1998 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2016 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Bounding rectangle class and protocol implementation.
;;;

(in-package #:climi)

;;; 4.1 Bounding rectangles

(defclass standard-bounding-rectangle (standard-rectangle) ())

(defun make-bounding-rectangle (x1 y1 x2 y2)
  (let ((x1 (coordinate x1))
        (y1 (coordinate y1))
        (x2 (coordinate x2))
        (y2 (coordinate y2)))
    (multiple-value-bind (x1 x2)
        (if (<= x1 x2)
            (values x1 x2)
            (values x2 x1))
      (multiple-value-bind (y1 y2)
          (if (<= y1 y2)
              (values y1 y2)
              (values y2 y1))
        (make-instance 'standard-bounding-rectangle :x1 x1 :y1 y1 :x2 x2 :y2 y2)))))

;;; 4.1.1 The Bounding Rectangle Protocol

(defmethod bounding-rectangle ((region rectangle))
  region)

(defmethod bounding-rectangle ((region region))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
    (make-bounding-rectangle x1 y1 x2 y2)))

;;; 4.1.2 Bounding Rectangle Convenience Functions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-rectangle-bindings (variables coordinate-expressions body)
    (destructuring-bind (&key x1 y1 x2 y2 width height center-x center-y)
        variables
      (let ((bindings '()))
        (macrolet ((bind (name expression)
                     `(let ((name (or ,name (gensym ,(string name)))))
                        (push (cons ',name name) bindings)
                        (unless (eq name ,expression)
                          (list (list name ,expression)))))
                   (ref (name)
                     `(assoc-value bindings ',name)))
          `(let* (,@(when (or x1 width  center-x)
                      (bind x1 (nth 0 coordinate-expressions)))
                  ,@(when (or y1 height center-y)
                      (bind y1 (nth 1 coordinate-expressions)))
                  ,@(when (or x2 width  center-x)
                      (bind x2 (nth 2 coordinate-expressions)))
                  ,@(when (or y2 height center-y)
                      (bind y2 (nth 3 coordinate-expressions)))
                  ,@(when (or width)
                      (bind width  `(- ,(ref x2) ,(ref x1))))
                  ,@(when (or height)
                      (bind height `(- ,(ref y2) ,(ref y1))))
                  ,@(when center-x
                      (bind center-x `(/ (+ ,(ref x1) ,(ref x2)) 2)))
                  ,@(when center-y
                      (bind center-y `(/ (+ ,(ref y1) ,(ref y2)) 2))))
             (declare (type coordinate ,@(map 'list #'cdr bindings)))
             ,@body))))))

;;; Compatible extension compared to standard.
;;; - Like in the standard macro, VARIABLES before first keyword are
;;;   positional and correspond to X1, Y1, X2, Y2. However:
;;;   - Fewer than all four can be provided.
;;;   - Any of the positional variables can be `nil' indicating that
;;;     the binding should not be established.
;;; - The first keyword initiates the keyword part of the variable
;;;   list (can start after between zero and four positional
;;;   variables).
(defmacro with-bounding-rectangle* ((&rest variables) region &body body)
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
      (alexandria:with-unique-names (x1* y1* x2* y2*)
        `(multiple-value-bind (,x1* ,y1* ,x2* ,y2*) (bounding-rectangle* ,region)
           (declare (type coordinate ,x1* ,y1* ,x2* ,y2*)
                    (ignorable ,x1* ,y1* ,x2* ,y2*))
           ,(generate-rectangle-bindings
             (list* :x1 x1 :y1 y1 :x2 x2 :y2 y2 keyword)
             `(,x1* ,y1* ,x2* ,y2*)
             body))))))

(macrolet ((def (name &rest parts)
             `(defmethod ,name (bounding-rectangle)
                (with-bounding-rectangle* (,@(apply #'append parts))
                    bounding-rectangle
                  (values ,@(map 'list #'second parts))))))
  (def bounding-rectangle-position (:x1 x1) (:y1 y1))
  (def bounding-rectangle-min-x    (:x1 x1))
  (def bounding-rectangle-min-y    (:y1 y1))
  (def bounding-rectangle-max-x    (:x2 x2))
  (def bounding-rectangle-max-y    (:y2 y2))
  (def bounding-rectangle-size     (:width width) (:height height))
  (def bounding-rectangle-width    (:width width))
  (def bounding-rectangle-height   (:height height)))
