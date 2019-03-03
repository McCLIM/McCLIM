;;; ---------------------------------------------------------------------------
;;;     Title: Page layout abstraction
;;;   Created: 2019-02-06 19:42
;;;    Author: Daniel Kochmański <daniel@turtleware.eu>
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2002 by Alexey Dejneka <adejneka@comail.ru>
;;;  (c) copyright 2019 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Page layout may have numerous properties which arrange things on a
;;; stream with having a broader picture in mind. Text on a page may
;;; have alignment and direction, margins, columns, paragraph settings
;;; and much more. This file is a beacon of the abstraction which may
;;; be used to specify these things.
;;;
(in-package :clim-internals)

(defclass standard-page-layout ()
  ((%page-region :reader stream-page-region :writer (setf %page-region))
   (margins :initarg :text-margins :accessor stream-text-margins))
  (:default-initargs :text-margins '(:left   (:absolute 0)
                                     :top    (:absolute 0)
                                     :right  (:relative 0)
                                     :bottom (:relative 0))))

(defmethod initialize-instance :after ((instance standard-page-layout)
                                       &key text-margins text-margin)
  (macrolet ((thunk (edge default)
               `(unless (getf text-margins ,edge)
                  (setf (getf text-margins ,edge) ,default))))
    (thunk :left   '(:absolute 0))
    (thunk :top    '(:absolute 0))
    (thunk :right  (or text-margin '(:relative 0)))
    (thunk :bottom '(:relative 0))))

(defgeneric page-cursor-initial-position (stream)
  (:documentation "Returns two values: x and y initial position for a cursor on page.")
  (:method ((stream standard-page-layout))
    (with-bounding-rectangle* (min-x min-y max-x max-y)
        (stream-page-region stream)
      (declare (ignore max-x max-y))
      (values min-x min-y))))

(defgeneric page-cursor-final-position (stream)
  (:documentation "Returns two values: x and y final position for a cursor on page.")
  (:method ((stream standard-page-layout))
    (with-bounding-rectangle* (min-x min-y max-x max-y)
        (stream-page-region stream)
      (declare (ignore min-x min-y))
      (values max-x max-y))))

(defmethod slot-unbound (class (stream standard-page-layout) (slot (eql '%page-region))
                         &aux (sheet-region (sheet-region (or (pane-viewport stream)
                                                              stream))))
  (when (eql sheet-region +everywhere+)
    (let ((x2 (* 80 (text-style-width  (stream-text-style stream) stream)))
          (y2 (* 43 (text-style-height (stream-text-style stream) stream))))
      (setf sheet-region (make-rectangle* 0 0 x2 y2))))
  (with-bounding-rectangle* (x1 y1 x2 y2) sheet-region
    (macrolet ((thunk (margin edge sign direction)
                 `(if (eql (first ,margin) :absolute)
                      (parse-space stream (second ,margin) ,direction)
                      (,sign ,edge (parse-space stream (second ,margin) ,direction)))))
      (destructuring-bind (&key left top right bottom) (stream-text-margins stream)
        (setf (%page-region stream)
              (make-rectangle* (thunk left   x1 + :horizontal)
                               (thunk top    y1 + :vertical)
                               (thunk right  x2 - :horizontal)
                               (thunk bottom y2 - :vertical)))))))

(defmethod (setf sheet-region) :before (sheet-region (stream standard-page-layout))
  (unless (region-equal sheet-region (sheet-region stream))
    (slot-makunbound stream '%page-region)))

(defun valid-margin-spec-p (margins)
  (ignore-errors ; destructuring-bind may error; that yields invalid spec
    (destructuring-bind (&key left top right bottom) margins
      (flet ((margin-spec-p (m)
               (and (member (first m) '(:absolute :relative))
                    (not (null (second m))))))
        (every #'margin-spec-p (list left top right bottom))))))

(deftype margin-spec ()
  `(satisfies valid-margin-spec-p))

(defmethod (setf stream-text-margins) :before
    (new-margins (stream standard-page-layout)
     &aux (old-margins (stream-text-margins stream)))
  (macrolet ((thunk (edge)
               `(unless (getf new-margins ,edge)
                  (setf (getf new-margins ,edge)
                        (getf old-margins ,edge)))))
    (thunk :left)
    (thunk :top)
    (thunk :right)
    (thunk :bottom))
  (check-type new-margins margin-spec)
  (unless (equal new-margins old-margins)
    (slot-makunbound stream '%page-region)))

(defgeneric invoke-with-temporary-page (stream continuation &key margins move-cursor)
  (:method ((stream standard-page-layout) continuation &key margins (move-cursor t))
    (letf (((stream-text-margins stream) margins))
      (multiple-value-bind (cx cy) (stream-cursor-position stream)
        (funcall continuation stream)
        (unless move-cursor
          (setf (stream-cursor-position stream) (values cx cy)))))))

(defmacro with-temporary-margins
    ((stream &rest args
             &key (move-cursor t) (left nil lp) (right nil rp) (top nil tp) (bottom nil bp))
     &body body)
  (declare (ignore move-cursor))
  (setq stream (stream-designator-symbol stream '*standard-output*))
  (with-keywords-removed (args (:left :right :top :bottom))
    (with-gensyms (continuation margins)
      `(flet ((,continuation (,stream) ,@body))
         (declare (dynamic-extent #',continuation))
         (let (,margins)
           ,@(collect (margin)
               (when lp (margin `(setf (getf ,margins :left) ,left)))
               (when rp (margin `(setf (getf ,margins :right) ,right)))
               (when tp (margin `(setf (getf ,margins :top) ,top)))
               (when bp (margin `(setf (getf ,margins :bottom) ,bottom)))
               (margin))
           (invoke-with-temporary-page ,stream #',continuation :margins ,margins ,@args))))))

