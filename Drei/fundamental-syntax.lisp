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
  ()
  (:command-table fundamental-table)
  (:name "Fundamental"))

(setf *default-syntax* 'fundamental-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; update syntax

(defmethod update-syntax values-max-min ((syntax fundamental-syntax) prefix-size suffix-size
                                         &optional begin end)
  (declare (ignore begin end))
  ;; We do nothing. Technically, Fundamental syntax always parses the
  ;; entire buffer, though.
  (values 0 (size (buffer syntax))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; display

(defstruct (pump-state
             (:constructor make-pump-state
                           (line-index offset chunk-index))) 
  "A pump state object used in the fundamental syntax. `Line' is
the line object `offset' is in, and `line-index' is the index of
`line' in the list of lines maintained by the syntax that created
this pump state."
  line-index
  offset
  chunk-index)

(defmethod pump-state-for-offset-with-syntax ((view textual-drei-syntax-view)
                                              (syntax fundamental-syntax) (offset integer))
  ;; Perform binary search looking for line starting with `offset'.
  (with-accessors ((lines lines)) view
    (loop with low-index = 0
          with high-index = (nb-elements lines)
          for middle = (floor (+ low-index high-index) 2)
          for line-start = (start-mark (element* lines middle))
          do (cond ((mark> offset line-start)
                    (setf low-index (1+ middle)))
                   ((mark< offset line-start)
                    (setf high-index middle))
                   ((mark= offset line-start)
                    (loop-finish)))
          finally (return (make-pump-state middle offset 0)))))

(defun fetch-chunk (line chunk-index)
  "Retrieve the `chunk-index'th chunk from `line'. The return
value is either an integer, in which case it specifies the
end-offset of a string chunk relative to the start of the line,
or a function, in which case it is the drawing function for a
single-object non-character chunk."
  (destructuring-bind (relative-chunk-end-offset . objectp)
      (aref (chunks line) chunk-index)
    (if objectp (object-drawer) (+ relative-chunk-end-offset
                                   (offset (start-mark line))))))

(defmethod stroke-pump-with-syntax ((view textual-drei-syntax-view)
                                    (syntax fundamental-syntax) stroke
                                    (pump-state pump-state))
  ;; `Pump-state' will be destructively modified.
  (prog1 pump-state
    (with-accessors ((line-index pump-state-line-index)
                     (offset pump-state-offset)
                     (chunk-index pump-state-chunk-index)) pump-state
      (let* ((chunk (fetch-chunk
                     (element* (lines view) line-index) chunk-index))
             (drawing-options (if (functionp chunk)
                                  (make-drawing-options :function chunk)
                                  +default-drawing-options+))
             (end-offset (if (functionp chunk)
                             (1+ offset)
                             chunk)))
        (setf (stroke-start-offset stroke) offset
              (stroke-end-offset stroke) end-offset
              (stroke-drawing-options stroke) drawing-options)
        (if (offset-end-of-line-p (buffer view) end-offset)
            (setf line-index (1+ line-index)
                  chunk-index 0
                  offset (1+ end-offset))
            (setf chunk-index (1+ chunk-index)
                  offset end-offset))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exploit the parse

;; do this better
(defmethod syntax-line-indentation ((syntax fundamental-syntax) mark tab-width)
  0)
