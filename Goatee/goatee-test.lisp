;;; -*- Mode: Lisp; Package: GOATEE -*-

;;;  (c) copyright 2002 by Tim Moore (moore@bricoworks.com)
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

;;; Puts up a single line of text for editing.  These basic commands work:
;;; self insertion of characters
;;; C-f, C-b for moving forwards and backwards
;;; C-a, C-e for beginning of line, end of line
;;; C-d for "forward deletion"
;;; #\delete or #\rubout for "backwards deletion"
;;; M-C-b drops into a break loop.
;;;
;;; Run with (clim-demo::run-test 'goatee-test)

(define-application-frame goatee-test ()
  ((goatee-area :accessor goatee-area :initarg :goatee-area))
  (:panes
   (tester :interactor :width 640))
  (:layouts
   (default (vertically () tester)))
  (:top-level (goatee-test-top-level)))

(defun goatee-test-top-level (frame &key
			      command-parser command-unparser 
			      partial-command-parser prompt)
  (declare (ignore command-parser command-unparser partial-command-parser))
  (let ((*standard-output* (frame-standard-output frame))
	(*standard-input* (frame-standard-input frame))
	(*print-pretty* nil)
	)
    (multiple-value-bind (cx cy)
	(stream-cursor-position *standard-output*)
      (setf (cursor-visibility (stream-text-cursor *standard-input*)) nil)
      (stream-add-output-record
       *standard-output*
       (setf (goatee-area frame)
	     (make-instance
	      'simple-screen-area
	      :area-stream *standard-output*
	      :buffer (make-instance
		       'editable-buffer
		       :initial-contents "The fox jumped over the goatee.")
	      :x-position cx
	      :y-position cy))))
    

    
    (loop
     (let ((gesture (read-gesture :stream *standard-input*)))
       (execute-gesture-command gesture
				(goatee-area frame)
				*simple-area-gesture-table*)))))
