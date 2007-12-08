;;; -*- Mode: Lisp; Package: DREI-FUNDAMENTAL-SYNTAX -*-

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
  (with-accessors ((buffer buffer) (scan scan)) syntax
    (setf scan (make-buffer-mark buffer 0 :left))))

(setf *default-syntax* 'fundamental-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; update syntax

(defclass line-object ()
  ((start-mark :initarg :start-mark :reader start-mark)))

(defmethod update-syntax-for-display (buffer (syntax fundamental-syntax) top bot)
  nil)

(defmethod update-syntax ((syntax fundamental-syntax) prefix-size suffix-size
                          &optional begin end)
  (declare (ignore begin end))
  (let ((low-mark (clone-mark (scan syntax) :left))
        (high-mark (clone-mark (scan syntax) :left)))
    (setf (offset low-mark) prefix-size
          (offset high-mark) (- (size (buffer syntax)) suffix-size))
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

(defun handle-whitespace (pane view buffer start end)
  (let ((space-width (space-width pane view))
        (tab-width (tab-width pane view)))
    (with-sheet-medium (medium pane)
      (with-accessors ((cursor-positions cursor-positions)) view
        (loop while (< start end)
           do (case (buffer-object buffer start)
                (#\Newline (record-line-vertical-offset pane view (incf *current-line*))
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

(defmethod display-line ((stream clim-stream-pane) (view textual-drei-syntax-view) mark)
  (let ((mark (clone-mark mark)))
    (let ((saved-offset nil)
          (id 0)
          (space-width (space-width stream view))
          (tab-width (tab-width stream view)))
      (flet ((output-word ()
               (unless (null saved-offset)
                 (let ((contents (coerce (region-to-sequence
                                          saved-offset
                                          mark)
                                         'string)))
                   (updating-output (stream :unique-id (cons view (incf id))
                                            :id-test #'equal
                                            :cache-value contents
                                            :cache-test #'equal)
                     (unless (null contents)
                       (present contents 'string :stream stream))))
                 (setf saved-offset nil))))
        (loop
           until (end-of-line-p mark)
           do (let ((obj (object-after mark)))
                (cond ((eql obj #\Space)
                       (output-word)
                       (stream-increment-cursor-position stream space-width 0))
                      ((eql obj #\Tab)
                       (output-word)
                       (let ((x (stream-cursor-position stream)))
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
             (terpri stream)))))))

(defmethod display-syntax-view ((stream clim-stream-pane) (view textual-drei-syntax-view)
                                (syntax fundamental-syntax))
  (update-parse syntax)
  (with-accessors ((top top) (bot bot)) view
    (with-accessors ((cursor-positions cursor-positions)) view
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
                (updating-output (stream :unique-id (cons view i)
                                         :id-test #'equal
                                         :cache-value line
                                         :cache-test #'equal)
                  (display-line stream view (start-mark (element* lines i))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exploit the parse 

;; do this better
(defmethod syntax-line-indentation ((syntax fundamental-syntax) mark tab-width)
  0)
