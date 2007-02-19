;; -*- Mode: Lisp; Package: DREI-FUNDAMENTAL-SYNTAX -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;
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

;;; Syntax for unknown buffer contents.  Parse contents into lines.

(in-package :drei-fundamental-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Every syntax must have a command table.

(define-syntax-command-table fundamental-table
    :errorp nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The syntax object and misc stuff.

(define-syntax fundamental-syntax (syntax)
  ((lines :initform (make-instance 'standard-flexichain))
   (scan :accessor scan))
  (:command-table fundamental-table)
  (:name "Fundamental"))

(defmethod initialize-instance :after ((syntax fundamental-syntax) &rest args)
  (declare (ignore args))
  (with-slots (buffer scan) syntax
     (setf scan (clone-mark (low-mark buffer) :left))))

(setf *default-syntax* 'fundamental-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; update syntax

(defclass line-object ()
  ((start-mark :initarg :start-mark :reader start-mark)))

(defmethod update-syntax-for-display (buffer (syntax fundamental-syntax) top bot)
  nil)

(defmethod update-syntax (buffer (syntax fundamental-syntax))
  (let* ((low-mark (low-mark buffer))
	 (high-mark (high-mark buffer)))
    (when (mark<= low-mark high-mark)
      (beginning-of-line low-mark)
      (end-of-line high-mark)
      (with-slots (lines scan) syntax
	(let ((low-index 0)
	      (high-index (nb-elements lines)))
	  (loop while (< low-index high-index)
		do (let* ((middle (floor (+ low-index high-index) 2))
			  (line-start (start-mark (element* lines middle))))
		     (cond ((mark> low-mark line-start)
			    (setf low-index (1+ middle)))
			   (t
			    (setf high-index middle)))))
	  ;; discard lines that have to be re-analyzed
	  (loop while (and (< low-index (nb-elements lines))
			   (mark<= (start-mark (element* lines low-index))
				   high-mark))
		do (delete* lines low-index))
	  ;; analyze new lines
	  (setf (offset scan) (offset low-mark))
	  (loop while (and (mark<= scan high-mark)
			   (not (end-of-buffer-p scan)))
		for i from low-index
		do (progn (insert* lines i (make-instance
					    'line-object
					    :start-mark (clone-mark scan)))
			  (end-of-line scan)
			  (unless (end-of-buffer-p scan)
			    ;; skip newline
			    (forward-object scan)))))))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; display

(defvar *white-space-start* nil)

(defvar *current-line* 0)

(defun handle-whitespace (pane buffer start end)
  (let ((space-width (space-width pane))
        (tab-width (tab-width pane)))
    (with-sheet-medium (medium pane)
      (with-accessors ((cursor-positions cursor-positions)) (syntax buffer)
        (loop while (< start end)
           do (case (buffer-object buffer start)
                (#\Newline (record-line-vertical-offset pane (syntax buffer) (incf *current-line*))
                           (terpri pane)
                           (stream-increment-cursor-position
                            pane (first (aref cursor-positions 0)) 0))
                ((#\Page #\Return #\Space) (stream-increment-cursor-position
                                            pane space-width 0))
                (#\Tab (when (plusp tab-width)
                         (let ((x (stream-cursor-position pane)))
                           (stream-increment-cursor-position
                            pane (- tab-width (mod x tab-width)) 0)))))
           (incf start))))))

(defmethod display-line ((stream clim-stream-pane) (drei drei) mark)
  (let ((mark (clone-mark mark)))
    (with-accessors ((space-width space-width) (tab-width tab-width)) stream
      (let ((saved-offset nil)
            (id 0))
        (flet ((output-word ()
                 (unless (null saved-offset)
                   (let ((contents (coerce (region-to-sequence
                                            saved-offset
                                            mark)
                                           'string)))
                     (updating-output (stream :unique-id (cons drei (incf id))
                                              :id-test #'equal
                                              :cache-value contents
                                              :cache-test #'equal)
                       (unless (null contents)
                         (present contents 'string :stream stream))))
                   (setf saved-offset nil))))
          (with-slots (bot scan cursor-x cursor-y) drei
            (loop
               until (end-of-line-p mark)
               do (let ((obj (object-after mark)))
                    (cond ((eql obj #\Space)
                           (output-word)
                           (stream-increment-cursor-position stream space-width 0))
                          ((eql obj #\Tab)
                           (output-word)
                           (let ((x (stream-cursor-position drei)))
                             (stream-increment-cursor-position
                              stream (- tab-width (mod x tab-width)) 0)))
                          ((constituentp obj)
                           (when (null saved-offset)
                             (setf saved-offset (offset mark))))
                          ((characterp obj)
                           (output-word)
                           (updating-output (stream :unique-id (cons stream (incf id))
                                                    :id-test #'equal
                                                    :cache-value obj)
                             (present obj 'character :stream stream)))
                          (t
                           (output-word)
                           (updating-output (stream :unique-id (cons stream (incf id))
                                                    :id-test #'equal
                                                    :cache-value obj
                                                    :cache-test #'eq)
                             (present obj (presentation-type-of obj)
                                      :stream stream)))))
               do (forward-object mark)
               finally
               (output-word)
               (unless (end-of-buffer-p mark)
                 (terpri stream)))))))))

(defmethod display-drei-contents ((stream clim-stream-pane) (drei drei) (syntax fundamental-syntax))
  (with-slots (top bot) drei
    (with-accessors ((cursor-positions cursor-positions)) syntax
      (setf cursor-positions (make-array (1+ (number-of-lines-in-region top bot))
                                         :initial-element nil
                                         :fill-pointer 1
                                         :adjustable t)
            *current-line* 0
            (aref cursor-positions 0) (multiple-value-list (stream-cursor-position stream))))
    (setf *white-space-start* (offset top))
    (with-slots (lines scan) syntax
      (let ((low-index 0)
            (high-index (nb-elements lines)))
        (loop while (< low-index high-index)
           do (let* ((middle (floor (+ low-index high-index) 2))
                     (line-start (start-mark (element* lines middle))))
                (cond ((mark> top line-start)
                       (setf low-index (1+ middle)))
                      ((mark< top line-start)
                       (setf high-index middle))
                      (t
                       (setf low-index middle
                             high-index middle)))))
        (loop for i from low-index
           while (and (< i (nb-elements lines))
                      (mark< (start-mark (element* lines i))
                             bot))
           do (let ((line (element* lines i)))
                (updating-output (stream :unique-id (cons drei i)
                                         :id-test #'equal
                                         :cache-value line
                                         :cache-test #'equal)
                  (display-line stream drei (start-mark (element* lines i))))))))))

(defmethod display-drei-cursor ((stream extended-output-stream) (drei drei)
                                (cursor drei-cursor) (syntax fundamental-syntax))
  (let ((mark (mark cursor))
        (drei (drei-instance cursor)))
    (multiple-value-bind (cursor-x cursor-y line-height)
	(offset-to-screen-position stream drei (offset mark))
      (updating-output (stream :unique-id (list stream :cursor)
                               :cache-value (list* cursor-x cursor-y line-height))
	(draw-rectangle* stream
			 (1- cursor-x) cursor-y
			 (+ cursor-x 2) (+ cursor-y line-height)
			 :ink (ink cursor))))))

(defmethod display-region ((pane drei-pane) (syntax fundamental-syntax))
  (highlight-region pane (point pane) (mark pane)))

(defgeneric highlight-region (pane mark1 offset2 &optional ink))

(defmethod highlight-region ((pane drei-pane) (offset1 integer) (offset2 integer)
			     &optional (ink (compose-in +green+ (make-opacity .1))))
  ;; FIXME stream-vertical-spacing between lines
  ;; FIXME note sure updating output is working properly...
  ;; we'll call offset1 CURSOR and offset2 MARK
  (multiple-value-bind (cursor-x cursor-y line-height)
      (offset-to-screen-position offset1 pane pane)
    (multiple-value-bind (mark-x mark-y)
	(offset-to-screen-position offset2 pane pane)
      (cond
	;; mark and point are above the screen
	((and (null cursor-y) (null mark-y)
	      (null cursor-x) (null mark-x))
	 nil)
	;; mark and point are below the screen
	((and (null cursor-y) (null mark-y)
	      cursor-x mark-x)
	 nil)
	;; mark or point is above the screen, and point or mark below it
	((and (null cursor-y) (null mark-y)
	      (or (and cursor-x (null mark-x))
		  (and (null cursor-x) mark-x)))
	 (let ((width (stream-text-margin pane))
	       (height (bounding-rectangle-height
			(window-viewport pane))))
	   (updating-output (pane :unique-id -3
				  :cache-value (list cursor-y mark-y cursor-x mark-x
						     height width ink))
	     (draw-rectangle* pane
			      0 0
			      width height
			      :ink ink))))
	;; mark is above the top of the screen
	((and (null mark-y) (null mark-x))
	 (let ((width (stream-text-margin pane)))
	   (updating-output (pane :unique-id -3
				  :cache-value ink)
	     (updating-output (pane :cache-value (list mark-y mark-x cursor-y width))
	       (draw-rectangle* pane
				0 0
				width cursor-y
				:ink ink))
	     (updating-output (pane :cache-value (list cursor-y cursor-x))
	       (draw-rectangle* pane
				0 cursor-y 
				cursor-x (+ cursor-y line-height)
				:ink ink)))))
	;; mark is below the bottom of the screen
	((and (null mark-y) mark-x)
	 (let ((width (stream-text-margin pane))
	       (height (bounding-rectangle-height
			(window-viewport pane))))
	   (updating-output (pane :unique-id -3
				  :cache-value ink)
	     (updating-output (pane :cache-value (list cursor-y width height))
	       (draw-rectangle* pane
				0 (+ cursor-y line-height)
				width height
				:ink ink))
	     (updating-output (pane :cache-value (list cursor-x cursor-y width))
	       (draw-rectangle* pane
				cursor-x cursor-y
				width (+ cursor-y line-height)
				:ink ink)))))
	;; mark is at point
	((and (= mark-x cursor-x) (= mark-y cursor-y))
	 nil)
	;; mark and point are on the same line
	((= mark-y cursor-y)
	 (updating-output (pane :unique-id -3
				:cache-value (list offset1 offset2 ink))
	   (draw-rectangle* pane
			    mark-x mark-y
			    cursor-x (+ cursor-y line-height)
			    :ink ink)))
	;; mark and point are both visible, mark above point
	((< mark-y cursor-y)
	 (let ((width (stream-text-margin pane)))
	   (updating-output (pane :unique-id -3
				  :cache-value ink)
	     (updating-output (pane :cache-value (list mark-x mark-y width))
	       (draw-rectangle* pane
				mark-x mark-y
				width (+ mark-y line-height)
				:ink ink))
	     (updating-output (pane :cache-value (list cursor-x cursor-y))
	       (draw-rectangle* pane
				0 cursor-y
				cursor-x (+ cursor-y line-height)
				:ink ink))
	     (updating-output (pane :cache-value (list mark-y cursor-y width))
	       (draw-rectangle* pane
				0 (+ mark-y line-height)
				width cursor-y
				:ink ink)))))
	;; mark and point are both visible, point above mark
	(t
	 (let ((width (stream-text-margin pane)))
	   (updating-output (pane :unique-id -3
				  :cache-value ink)
	     (updating-output (pane :cache-value (list cursor-x cursor-y width))
	       (draw-rectangle* pane
				cursor-x cursor-y
				width (+ cursor-y line-height)
				:ink ink))
	     (updating-output (pane :cache-value (list mark-x mark-y))
	       (draw-rectangle* pane
				0 mark-y
				mark-x (+ mark-y line-height)
				:ink ink))
	     (updating-output (pane :cache-value (list cursor-y mark-y width))
	       (draw-rectangle* pane
				0 (+ cursor-y line-height)
				width mark-y
				:ink ink)))))))))

(defmethod highlight-region ((pane drei-pane) (mark1 mark) (mark2 mark)
			     &optional (ink (compose-in +green+ (make-opacity .1))))
  (highlight-region pane (offset mark1) (offset mark2) ink))

(defmethod highlight-region ((pane drei-pane) (mark1 mark) (offset2 integer)
			     &optional (ink (compose-in +green+ (make-opacity .1))))
  (highlight-region pane (offset mark1) offset2 ink))

(defmethod highlight-region ((pane drei-pane) (offset1 integer) (mark2 mark)
			     &optional (ink (compose-in +green+ (make-opacity .1))))
  (highlight-region pane offset1 (offset mark2) ink))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exploit the parse 

;; do this better
(defmethod syntax-line-indentation (mark tab-width (syntax fundamental-syntax))
  0)
