;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 1998-2001 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) Copyright 2000,2014 by Robert Strandh <robert.strandh@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

(defgeneric cursor-height (cursor))
(defgeneric cursor-width (cursor))

;;; Standard-Text-Cursor class
(defclass standard-text-cursor (cursor)
  ((sheet :initarg :sheet :reader cursor-sheet)
   (x :initarg :x-position)
   (y :initarg :y-position)
   (width  :initarg :width  :accessor cursor-width)
   (height :initarg :height :accessor cursor-height)
   ;; XXX what does "cursor is active" mean?
   ;; It means that the sheet (stream) updates the cursor, though currently the
   ;; cursor appears to be always updated after stream text operations. -- moore
   (cursor-active :accessor cursor-active)
   (cursor-state  :accessor cursor-state))
  (:default-initargs :x-position 0
                     :y-position 0
                     :width 4
                     :height 16))

(defmethod initialize-instance :after ((object standard-text-cursor) &key visibility)
  (setf (cursor-visibility object) visibility))

(defmethod bounding-rectangle* ((cursor standard-text-cursor))
  (with-slots (x y width height) cursor
    (values x y (+ x width) (+ y height))))

(defmethod print-object ((cursor standard-text-cursor) stream)
  (with-slots (x y) cursor
    (print-unreadable-object (cursor stream :type t :identity t)
      (format stream "~D ~D " x y))))

(defmethod cursor-visibility ((cursor standard-text-cursor))
  (if (cursor-active cursor)
      (if (cursor-state cursor)
          :on
          :off)
      nil))

(defmethod (setf cursor-visibility) (nv (cursor standard-text-cursor))
  (multiple-value-bind (active state)
      (ecase nv
        ((:on t) (values t t))
        (:off    (values t nil))
        ((nil)   (values nil nil)))
    (setf (cursor-state cursor)  state
          (cursor-active cursor) active)))

(defmethod cursor-position ((cursor standard-text-cursor))
  (with-slots (x y) cursor
    (values x y)))

(defmethod* (setf cursor-position) (nx ny (cursor standard-text-cursor))
  (when (cursor-active cursor)
    (with-slots (x y) cursor
      (setf (values x y) (values nx ny)))))

(defmethod cursor-focus ((cursor standard-text-cursor))
  (when-let* ((sheet (cursor-sheet cursor))
              (port (port sheet)))
    (eq sheet (port-keyboard-input-focus port))))

(defmethod draw-design (sheet (cursor standard-text-cursor)
                        &rest args &key (ink +foreground-ink+)
                        &allow-other-keys)
  (when (cursor-state cursor)
    (with-bounding-rectangle* (x1 y1 x2 y2) cursor
      (let ((ink (if (and (cursor-focus cursor)
                          (cursor-active cursor))
                     ink +dark-grey+)))
        (apply #'draw-rectangle* sheet  x1 y1 x2 y2
               :ink ink :filled t args)))))
