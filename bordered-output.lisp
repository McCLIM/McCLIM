;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Alexey Dejneka (adejneka@comail.ru)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

;;; TODO:
;;; - Use DRAWING-OPTIONS, MOVE-CURSOR in I-S-O-W-B
;;; - Gap computation

(in-package :clim-internals)

(defvar *border-types* (make-hash-table))

(defmacro surrounding-output-with-border ((&optional stream
                                           &rest drawing-options
                                           &key (shape :rectangle) (move-cursor t))
                                          &body body)
  (declare (ignore shape move-cursor))
  (orf stream '*standard-output*)
  (check-type stream symbol)
  (let ((continuation-name (gensym)))
    `(flet ((,continuation-name (,stream) ,@body))
       (invoke-surrounding-output-with-border ,stream
                                              #',continuation-name
                                              ,@drawing-options))))

(defun invoke-surrounding-output-with-border (stream cont
                                              &rest drawing-options
                                              &key (shape :rectangle) (move-cursor t))
  (with-sheet-medium (medium stream)
    (let ((record (with-new-output-record (stream)
                    (funcall cont stream))))
      (with-bounding-rectangle* (left top right bottom) record
        (letf (((medium-transformation medium) +identity-transformation+))
          (funcall (gethash shape *border-types*)
                   :stream stream
                   :record record
                   :left left :top top
                   :right right :bottom bottom
                   :allow-other-keys t))))))

(defmacro define-border-type (shape arglist &body body)
  (check-type arglist list)
  (loop for arg in arglist
     do (check-type arg symbol)
        (assert (member arg '(&key stream record left top right bottom)
                        :test #'string-equal)))
  `(setf (gethash ,shape *border-types*)
         (lambda ,arglist ,@body)))


;;;; Standard border types

(define-border-type :rectangle (&key stream left top right bottom)
  (let ((gap 3)) ; FIXME
    (draw-rectangle* stream
                     (- left gap) (- top gap)
                     (+ right gap) (+ bottom gap)
                     :filled nil)))

(define-border-type :oval (&key stream left top right bottom)
  (let ((gap 3)) ; FIXME
    (draw-oval* stream
                (/ (+ left right) 2) (/ (+ top bottom) 2)
                (+ (/ (- right left) 2) gap) (+ (/ (- bottom top) 2) gap)
                :filled nil)))

(define-border-type :drop-shadow (&key stream left top right bottom)
  (let* ((gap 3) ; FIXME?
	 (offset 4)
	 (left-edge (- left gap))
	 (bottom-edge (+ bottom gap))
	 (top-edge (- top gap))
	 (right-edge (+ right gap)))
    (draw-rectangle* stream
		     left-edge top-edge
		     right-edge bottom-edge
		     :filled nil)
    (draw-rectangle* stream
		     right-edge (+ top-edge offset)
		     (+ right-edge offset) bottom-edge :filled T)
    (draw-rectangle* stream
		     (+ left-edge offset) bottom-edge
		     (+ right-edge offset) (+ bottom-edge offset)
		     :filled T)))

(define-border-type :underline (&key stream record)
  (let ((children (output-record-children record)))
    (loop for child across children do
      (when (text-displayed-output-record-p child)
	(with-bounding-rectangle* (left top right bottom) child
	  (declare (ignore top))
	  (draw-line* stream left bottom right bottom))))))
