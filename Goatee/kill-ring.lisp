;;; -*- Mode: Lisp; Package: GOATEE -*-

;;;  (c) copyright 2002 by Andreas Fuchs <asf@void.at>
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

(in-package :goatee)

;; Kill Ring is the place where killed regions go. We probably need to
;; have better semantics for killing after/before motion (like emacs
;; does - have two consecutive `kill-line's append the second killed
;; line to the first and open another element in the kill ring if
;; there was motion or other activity between the two kills). I have
;; tried to emulate this behaviour with *kill-ring-open-new*.
	    
(defclass ring (dbl-list-head)
   ((last-access :accessor last-access
		 :documentation "Pointer to the element that was
		 accessed last."
		 :initform nil)
    (end         :accessor end
		 :initform nil
		 :documentation "Last element in the list."))
  (:documentation "A LIFO double-list whose last element's next-value is the
  ring's first element."))

(defclass ring-cell (dbl-list-cell)
  ((head :accessor head :initarg :head :initform nil)))

(defun make-ring ()
  (make-instance 'ring))

(defmethod forward ((r ring))
  "Move forward in a ring."
  (let ((next-element (next (or (last-access r)
				r))))
    (setf (last-access r) (or next-element
			      (dbl-head r)))))

(defmethod backward ((r ring))
  "Move backward in a ring."
  (let ((prev-element (if (last-access r)
			  (prev (last-access r))
			(end r))))
    (setf (last-access r) (if (eq prev-element r)
			      (end r)
			    prev-element))))

(defun ring-obj-insert (obj ring)
  "Insert an object into a Ring."
  (let ((cell (make-instance 'ring-cell :head ring :contents obj)))
    (dbl-insert-after cell ring)))

(defmethod dbl-kill-after :after ((element ring-cell))
  (setf (end (head element)) element)
  (setf (last-access (head element)) nil))

(defmethod dbl-remove :after ((element ring-cell))
  (when (eq (end (head element)) element)
    (setf (end (head element)) element))
  (setf (last-access (head element)) nil))

(defmethod dbl-insert-after :after ((element ring-cell) (ring ring))
  (when (eq (end ring) nil)
    (setf (end ring) element)))

(defun kill-region (ring buf start end)
  (copy-or-kill-region ring buf start end :copy nil))
 
(defun copy-region (ring buf start end)
  (copy-or-kill-region ring buf start end :copy t))

(defun copy-or-kill-region (ring buf start end &key copy)
  (let ((copy-buffer (make-instance 'editable-buffer)))
    (map-over-region #'(lambda (line pos)
			 (insert copy-buffer (char-ref* buf line pos)))
		       buf start end)
    (unless copy
      (delete-region buf start end))
    (ring-obj-insert copy-buffer ring)))

(defun yank (ring buffer yank-extent)
  (yank-1 ring buffer #'dbl-head yank-extent))

(defun yank-next (ring buffer yank-extent)
  (delete-region buffer (bp-start yank-extent) (bp-end yank-extent))
  (yank-1 ring buffer #'forward yank-extent))

(defun yank-prev (ring buffer yank-extent)
  (delete-region buffer (bp-start yank-extent) (bp-end yank-extent))
  (yank-1 ring buffer #'backward yank-extent))

(defun yank-1 (ring buffer operation yank-extent)
  (let ((to-insert (buffer-string (contents (funcall operation ring)))))
    (insert buffer to-insert :position (bp-end yank-extent))))

(defmacro with-object-on-ring ((object ring)  &body body)
  (climi::with-gensyms  (ring-var cell-var)
    `(let* ((,ring-var ,ring)
	    (,cell-var (ring-obj-insert ,object ,ring-var)))
       (unwind-protect
	    (progn
	      ,@body)
	 (goatee::dbl-remove ,cell-var)))))

