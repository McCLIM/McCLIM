;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clouseau)

;;; Places

(defclass car-place (basic-place)
  ())

(defmethod value ((place car-place))
  (car (cell place)))

(defmethod (setf value) (new-value (place car-place))
  (setf (car (cell place)) new-value))

(defclass cdr-place (basic-place)
  ())

(defmethod value ((place cdr-place))
  (cdr (cell place)))

(defmethod (setf value) (new-value (place cdr-place))
  (setf (cdr (cell place)) new-value))

(defclass list-element-place (car-place
                              sequence-element-place)
  ())

(defmethod supportsp ((place list-element-place) (operation (eql 'remove-value)))
  t)

(defmethod remove-value ((place list-element-place))
  (loop :for predecessor :on (container place)
        :for middle = (rest predecessor)
        :for successor = (rest middle)
        :when (eq middle (cell place))
        :do (setf (cdr predecessor) successor)
            (return)))

(defclass alist-element-place (list-element-place)
  ())

(defmethod remove-value ((place alist-element-place))
  (delete (cell place) (container place)))

(defclass alist-key-place (key-place
                           alist-element-place)
  ())

(defclass alist-value-place (value-place
                             cdr-place
                             alist-element-place)
  ())

;;; Object states

(defclass inspected-list (inspected-sequence)
  ((%cell-style :initarg  :cell-style
                :type     (member :element-list :graph)
                :accessor cell-style
                :initform :element-list)))

(defclass inspected-improper-list (inspected-list)
  ())

(defclass inspected-proper-list (inspected-list)
  ())

(defclass inspected-alist (inspected-proper-list)
  ())

(defmethod object-state-class ((object cons) (place t))
  (cond ((not (proper-list-p object))
         'inspected-improper-list)
        ((and (not (length= 1 object))
              (every (of-type '(cons (not cons) t)) object))
         'inspected-alist)
        (t
         'inspected-proper-list)))

;;; Object inspection methods

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-proper-list)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (write-string "Proper " stream)
  (inspect-class-as-name (find-class 'list) stream)
  (write-char #\Space stream)
  (with-output-as-presentation (stream state 'sequence-range)
    (print-sequence-header stream (length object) (start state) (end state))))

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-alist)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (write-string "Alist-shaped " stream)
  (inspect-class-as-name (find-class 'list) stream)
  (write-char #\Space stream)
  (print-sequence-header
   stream (length object) (start state) (end state)))

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-improper-list)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (cond ((listp (cdr object))
         (write-string "Improper " stream)
         (inspect-class-as-name (find-class 'list) stream))
        (t
         (inspect-class-as-name (class-of object) stream))))

(defmethod inspect-object-using-state :after ((object cons)
                                              (state  inspected-improper-list)
                                              (style  (eql :badges))
                                              (stream t))
  (when (circular-tree-p object)
    (write-char #\Space stream)
    (badge stream "circular")))

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-list)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (let ((style (cell-style state)))
    (inspect-object-using-state object state style stream)))

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-proper-list)
                                       (style  (eql :element-list))
                                       (stream t))
  (let ((length (length object)))
    (multiple-value-bind (start end truncated?) (effective-bounds state length)
      (with-preserved-cursor-x (stream)
        (formatting-table (stream)
          (loop :for i :below (- end start)
                :for cell :on (nthcdr start object)
                :do (format-place-row stream object 'list-element-place cell))))
      (when truncated?
        (note-truncated stream length (- end start))))))

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-alist)
                                       (style  (eql :element-list))
                                       (stream t))
  (let ((length (length object)))
    (multiple-value-bind (start end truncated?)
        (effective-bounds state length)
      (with-preserved-cursor-x (stream)
        (formatting-table (stream)
          (loop :for i :from start :below end
                :for cell :in (nthcdr start object)
                :for (key . value) = cell
                :do (formatting-row (stream)
                      (format-place-cells stream object 'alist-key-place cell)
                      (format-place-cells stream object 'alist-value-place cell)))))
      (when truncated?
        (note-truncated stream length (- end start))))))

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-improper-list)
                                       (style  (eql :element-list))
                                       (stream t))
  (formatting-table (stream :background +red+)
    (formatting-row (stream)
      (formatting-cell (stream)
        (princ "(" stream))
      (loop :with limit = (or *print-length* 10)
            :for cell :on object
            :for (car . cdr) = (when (listp cell) cell)
            :for i :from 1 :to limit
            :while (consp (cdr cell))
            :do (formatting-cell (stream)
                  (formatting-place (object 'list-element-place cell present inspect)
                    (present stream) (inspect stream)))
                (formatting-cell (stream) (write-string " " stream))
            :finally (cond ((null cdr)
                            (formatting-cell (stream)
                              (formatting-place (object 'list-element-place cell nil inspect)
                                (inspect stream)))
                            (formatting-cell (stream) (princ ")" stream))
                            t)
                           ((atom cdr)
                            (formatting-cell (stream)
                              (formatting-place (object 'list-element-place cell present inspect)
                                (present stream) (inspect stream)))
                            (formatting-cell (stream)
                              (with-drawing-options (stream :text-face :bold :ink +forest-green+)
                                (princ "." stream)))
                            (formatting-cell (stream)
                              (formatting-place (object 'cdr-place cell present inspect)
                                (present stream) (inspect stream)))
                            (formatting-cell (stream) (princ ")" stream))
                            t)
                           ((>= i limit)
                            (with-output-as-presentation (stream cell 'long-list-tail)
                              (formatting-cell (stream) (princ "...)" stream)))
                            t)
                           (t nil))))))

(defmethod inspect-object-using-state ((object cons)
                                       (state  inspected-list)
                                       (style  (eql :graph))
                                       (stream t))
  (flet ((format-node (object stream)
           (destructuring-bind (car . cdr) object
             (surrounding-output-with-border (stream :padding 1)
               (formatting-table (stream :y-spacing 0)
                 (formatting-row (stream)
                   (formatting-cell (stream)
                     (formatting-place (nil 'car-place object present inspect)
                       (with-style (stream :slot-like)
                         (write-string "car" stream)
                         (present stream))
                       (unless (consp car)
                         (inspect stream)))))
                 (formatting-row (stream)
                   (formatting-cell (stream)
                     (formatting-place (nil 'cdr-place object present inspect)
                       (with-style (stream :slot-like)
                         (write-string "cdr" stream)
                         (present stream))
                       (unless (consp cdr)
                         (inspect stream)))))))))
         (node-children (object)
           (destructuring-bind (car . cdr) object
             (cond ((and (consp car) (consp cdr)) (list car cdr))
                   ((consp car)                   (list car))
                   ((consp cdr)                   (list cdr))
                   (t                             '()))))
         (draw-arc (stream node1 node2 x1 y1 x2 y2)
           (let* ((backp      (< x2 x1))
                  (cdrp       (eq (graph-node-object node2)
                                  (cdr (graph-node-object node1))))
                  ;; This X1 offset attaches the start of back-arcs to
                  ;; right side of NODE1.
                  (x1         (+ x1 (if backp
                                        (bounding-rectangle-width node2)
                                        0)))
                  ;; This Y1 offset attaches the start of the arc to
                  ;; either the upper (car) or the lower (cdr) box.
                  (y1         (+ y1 (* (if cdrp 1/4 -1/4)
                                       (bounding-rectangle-height node1))))
                  ;; Elevate the middle of back-arcs to reduce the
                  ;; probability of overlapping nodes and arcs.
                  (arc-height (if backp
                                  (* 2/10 (- x2 x1))
                                  0))
                  (design     (mcclim-bezier:make-bezier-curve*
                               (list x1              y1
                                     (lerp .3 x1 x2) (+ (lerp .3 y1 y2) arc-height)
                                     (lerp .7 x1 x2) (+ (lerp .7 y1 y2) arc-height)
                                     x2              y2))))
             (draw-design stream design)
             (draw-arrow* stream
                          (lerp .99 x1 x2) (lerp .99 y1 y2)
                          x2               y2))))
    (format-graph-from-root
     object #'format-node #'node-children
     :stream stream :graph-type :digraph :merge-duplicates t
     :maximize-generations t :arc-drawer #'draw-arc)))

;;; Commands

(define-command (com-cons-style-as-list :command-table inspector-command-table
                                        :name          t)
    ((object inspected-list))
  (setf (cell-style object) :element-list))

(define-presentation-to-command-translator
    inspected-list->com-cons-style-as-list
    (inspected-list com-cons-style-as-list inspector-command-table
     :tester ((object)
              (and (not (eq (style object) :collapsed))
                   (not (eq (cell-style object) :element-list))))
     :priority -1
     :documentation "Show cells as list elements"
     :pointer-documentation ((object stream)
                             (with-print-error-handling (stream)
                               (with-safe-and-terse-printing (stream)
                                 (format stream "~@<Show cells of ~A ~
                                                 as a list elements.~@:>"
                                         (object object))))))
    (object)
  (list object))

(define-command (com-cons-style-as-graph :command-table inspector-command-table
                                         :name          t)
    ((object inspected-list))
  (setf (cell-style object) :graph))

(define-presentation-to-command-translator
    inspected-list->com-cons-style-as-graph
    (inspected-list com-cons-style-as-graph inspector-command-table
     :tester ((object)
              (and (not (eq (style object) :collapsed))
                   (not (eq (cell-style object) :graph))))
     :priority -1
     :documentation "Show cells as graph"
     :pointer-documentation ((object stream)
                             (with-print-error-handling (stream)
                               (with-safe-and-terse-printing (stream)
                                (format stream "~@<Show cells of ~A as ~
                                                a graph.~@:>"
                                        (object object))))))
    (object)
  (list object))
