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
  ((lines :initform (make-instance 'standard-flexichain)
          :reader lines)
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
  ((%start-mark :reader start-mark
                :initarg :start-mark)
   (%line-length :reader line-length
                 :initarg :line-length)
   (%chunks :accessor chunks
            :initform (make-array 5
                       :adjustable t
                       :fill-pointer 0)
            :documentation "A list of cons-cells, with the car
being a buffer offset relative to the `start-mark' of the line,
and the cdr being T if the chunk covers a non-character, and NIL
if it covers a character sequence.")))

(defun line-end-offset (line)
  "Return the end buffer offset of `line'."
  (+ (offset (start-mark line)) (line-length line)))

(defun get-chunk (buffer line-start-offset chunk-start-offset line-end-offset)
  "Return a chunk in the form of a cons cell. The chunk will
start at `chunk-start-offset' and extend no further than
`line-end-offset'."
  (let* ((chunk-end-offset (buffer-find-nonchar
                            buffer chunk-start-offset
                            (min (+ *maximum-chunk-size*
                                    chunk-start-offset)
                                 line-end-offset))))
    (cond ((= chunk-start-offset line-end-offset)
           (cons (- chunk-end-offset
                    line-start-offset) nil))
          ((or (not (= chunk-end-offset chunk-start-offset))
               (and (offset-beginning-of-line-p buffer chunk-start-offset)
                    (offset-end-of-line-p buffer chunk-end-offset)))
           (cons (- chunk-end-offset
                    line-start-offset) nil))
          ((not (characterp (buffer-object buffer chunk-end-offset)))
           (cons (- (1+ chunk-end-offset)
                    line-start-offset) t)))))

(defmethod update-syntax values-max-min ((syntax fundamental-syntax) prefix-size suffix-size
                                         &optional begin end)
  (declare (ignore begin end))
  (let ((low-mark (make-buffer-mark (buffer syntax) prefix-size :left))
        (high-mark (make-buffer-mark
                    (buffer syntax) (- (size (buffer syntax)) suffix-size) :left)))
    (when (mark<= low-mark high-mark)
      (beginning-of-line low-mark)
      (end-of-line high-mark)
      (with-slots (lines scan) syntax
        (let ((low-index 0)
              (high-index (nb-elements lines)))
          ;; Binary search for the start of changed lines.
          (loop while (< low-index high-index)
             do (let* ((middle (floor (+ low-index high-index) 2))
                       (line-start (start-mark (element* lines middle))))
                  (cond ((mark> low-mark line-start)
                         (setf low-index (1+ middle)))
                        (t
                         (setf high-index middle)))))
          ;; Discard lines that have to be re-analyzed.
          (loop while (and (< low-index (nb-elements lines))
                           (mark<= (start-mark (element* lines low-index))
                                   high-mark))
             do (delete* lines low-index))
          ;; Analyze new lines.
          (setf (offset scan) (offset low-mark))
          (loop while (mark<= scan high-mark)
             for i from low-index
             do (progn (let ((line-start-mark (clone-mark scan)))
                         (insert* lines i (make-instance
                                           'line-object
                                           :start-mark line-start-mark
                                           :line-length (- (offset (end-of-line scan))
                                                           (offset line-start-mark))))
                         (if (end-of-buffer-p scan)
                             (loop-finish)
                             ;; skip newline
                             (forward-object scan))))))))
    ;; Fundamental syntax always parses the entire buffer.
    (values 0 (size (buffer syntax)))))

(defmethod initialize-instance :after ((line line-object)
                                       &rest initargs)
  (declare (ignore initargs))
  (loop with buffer = (buffer (start-mark line))
     with line-start-offset = (offset (start-mark line))
     with line-end-offset = (+ line-start-offset (line-length line))
     with chunk-start-offset = line-start-offset
     for chunk-info = (get-chunk buffer
                                 line-start-offset
                                 chunk-start-offset line-end-offset)
     do (vector-push-extend chunk-info (chunks line))
     (setf chunk-start-offset (+ (car chunk-info)
                                 line-start-offset))
     when (= chunk-start-offset line-end-offset)
     do (loop-finish)))
		
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
  (update-parse syntax 0 offset)
  ;; Perform binary search looking for line starting with `offset'.
  (with-accessors ((lines lines)) syntax
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
      (update-parse syntax 0 offset)
      (let* ((chunk (fetch-chunk
                     (element* (lines syntax) line-index) chunk-index))
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

(defun offset-in-line-p (line offset)
  "Return true if `offset' is in the buffer region delimited by
`line'."
  (<= (offset (start-mark line)) offset
      (line-end-offset line)))

(defun line-containing-offset (syntax mark-or-offset)
  "Return the line `mark-or-offset' is in for `syntax'. `Syntax'
must be a `fundamental-syntax' object."
  ;; Perform binary search looking for line containing `offset1'.
  (as-offsets ((offset mark-or-offset))
    (with-accessors ((lines lines)) syntax
      (loop with low-index = 0
         with high-index = (nb-elements lines)
         for middle = (floor (+ low-index high-index) 2)
         for this-line = (element* lines middle)
         for line-start = (start-mark this-line)
         do (cond ((offset-in-line-p this-line offset)
                   (loop-finish))
                  ((mark> offset line-start)
                   (setf low-index (1+ middle)))
                  ((mark< offset line-start)
                   (setf high-index middle)))
         finally (return this-line)))))

;; do this better
(defmethod syntax-line-indentation ((syntax fundamental-syntax) mark tab-width)
  0)
