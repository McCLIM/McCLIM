;;;; Copyright (C) 2019 Jan Moringen
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

(defclass extended-sequence-element-place (sequence-element-place)
  ())

(define-presentation-method present ((object extended-sequence-element-place)
                                     (type   place)
                                     (stream t)
                                     (view   inspector-view)
                                     &key)
  (prin1 (cell object) stream))

(defclass inspected-extended-sequence (inspected-instance
                                       inspected-sequence)
  ())

(defmethod object-state-class :around ((object standard-object) (place t))
  (if (typep object 'sequence)
      'inspected-extended-sequence
      (call-next-method)))

(defmethod inspect-object-using-state ((object sequence)
                                       (state  inspected-extended-sequence)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (inspect-class-as-name (class-of object) stream)
  (write-char #\Space stream)
  (with-output-as-presentation (stream state 'sequence-range)
    (print-sequence-header stream (length object) (start state) (end state))))

(defmethod inspect-object-using-state ((object sequence)
                                       (state  inspected-extended-sequence)
                                       (style  (eql :expanded-body))
                                       (stream extended-output-stream))
  (let ((length (length object)))
    (with-section (stream) "Elements"
      (with-placeholder-if-empty (stream)
        ((zerop length)
         "no elements")
        (t
         (multiple-value-bind (start end truncated?)
             (effective-bounds state length)
           (with-preserved-cursor-x (stream)
             (formatting-table (stream)
               (loop :for i :from start :below end
                     :do (formatting-row (stream)
                           (formatting-place
                               (object 'extended-sequence-element-place i present inspect)
                             (formatting-cell (stream :align-x :right)
                               (present stream))
                             (formatting-cell (stream)
                               (inspect stream)))))))
           (when truncated?
             (note-truncated stream length (- end start)))))))))
