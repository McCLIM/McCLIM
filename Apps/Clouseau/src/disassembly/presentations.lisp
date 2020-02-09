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

;;;; The code in this file parses the output of SBCL's disassembler
;;;; and presents it in an enhanced fashion.

(cl:in-package #:clouseau)

;;; Basic blocks

(defun starters (basic-blocks)
  (flet ((has-predecessors-p (basic-block)
           (some (lambda (other)
                   (find basic-block (successors other) :key #'basic-block))
                 basic-blocks)))
    (remove-if #'has-predecessors-p basic-blocks)))

;;; `basic-block'

(defclass basic-block ()
  ((%label        :initarg :label
                  :reader  label)
   (%instructions :initarg  :instructions
                  :accessor instructions
                  :initform '())
   (%successors   :initarg  :successors
                  :accessor successors
                  :initform '())))

(defmethod offset ((block basic-block))
  (when (null (first (instructions block))) ; TODO why can this happen
    (return-from offset 0))
  (offset (first (instructions block))))

(defclass successor ()
  ((%basic-block :initarg :basic-block
                 :reader  basic-block)))

(defclass fall-through-successor (successor)
  ())

(defclass false-successor (successor)
  ())

(defclass true-successor (successor)
  ())

(defclass instruction ()
  ((%offset  :initarg :offset
             :reader  offset)
   (%label   :initarg :label
             :reader  label)
   (%bytes   :initarg :bytes
             :reader  bytes)
   (%decoded :initarg :decoded
             :reader  decoded)
   (%comment :initarg :comment
             :reader  comment)))

(defclass basic-block-graph-view () ())

(defclass disassembly-text-view () ())

;;; `basic-blocks'

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-presentation-type basic-blocks ()))

(define-presentation-method present ((object sequence)
                                     (type   basic-blocks)
                                     (stream extended-output-stream)
                                     (view   disassembly-text-view)
                                     &key)
  (with-drawing-options (stream :text-size :tiny)
    (formatting-table (stream :y-spacing 0)
      (map nil (lambda (basic-block)
                 (present basic-block 'basic-block :stream stream :view view))
           (sort object #'< :key #'offset)))))

(defun maybe-extract (thing)
  (etypecase thing
    (basic-block thing)
    (successor   (basic-block thing))))

(define-presentation-method present ((object sequence)
                                     (type   basic-blocks)
                                     (stream extended-output-stream)
                                     (view   basic-block-graph-view)
                                     &key)
  (loop :for i :from 0 :to 0.5 :by .01
        :do (draw-rectangle* stream (* i 100) 10 (* (+ i .01) 100) 20
                             :ink (make-ihs-color (+ 0.5 i) (mod (+ i 0.6) 1) 1)))
  (let ((roots (starters object)))
    (format-graph-from-roots
     roots
     (lambda (object stream)
       (present (maybe-extract object) 'basic-block
                :stream stream :view view))
     (compose #'successors #'maybe-extract)
     :stream stream
     :graph-type :directed-graph :merge-duplicates t :duplicate-key #'maybe-extract
     :orientation :vertical :maximize-generations t
     :arc-drawer (lambda (stream from-node to-node x1 y1 x2 y2
                          &rest with-drawing-options)
                   (let ((ink (typecase (climi::graph-node-object to-node)
                                (fall-through-successor +black+)
                                (false-successor        +dark-red+)
                                (true-successor         +forest-green+))))
                     (apply #'climi::arrow-arc-drawer #+later #'climi::bezier-arc-drawer
                                                      stream from-node to-node x1 y1 x2 y2
                                                      :ink ink with-drawing-options))))))

;;; `basic-block'

(define-presentation-method present ((object basic-block)
                                     (type   basic-block)
                                     (stream extended-output-stream)
                                     (view   t)
                                     &key)
  (surrounding-output-with-border (stream :shape :rectangle)
    (with-drawing-options (stream :text-size :tiny)
      (when-let ((label (label object)))
        (with-drawing-options (stream :text-face :bold :ink +dark-slate-blue+)
          (write-string label stream)))
      (formatting-table (stream :y-spacing 0)
        (map nil (lambda (instruction)
                   (formatting-row (stream)
                     (present instruction 'instruction :stream stream :view view)))
             (instructions object))))))

(define-presentation-method present ((object basic-block)
                                     (type   basic-block)
                                     (stream extended-output-stream)
                                     (view   disassembly-text-view)
                                     &key)
  (map nil (lambda (instruction)
             (formatting-row (stream)
               (present instruction '(instruction :show-label t)
                        :stream stream :view view)))
       (instructions object)))

;;; `instruction'

(define-presentation-type instruction (&key (show-label nil))) ; TODO is show-label unused?

(defun heat->color (heat)
  (let ((intensity  (lerp heat 0.8 1.0))
        (hue        (mod (+ (/ heat 2.0) 0.6) 1.0))
        (saturation (lerp heat .2 1.0)))
    (make-ihs-color intensity hue saturation)))

(defun instruction-color (instruction)
  (when-let ((comment (comment instruction)))
    (when (ends-with-subseq "samples" comment)
      (let* ((index1 (position #\/ comment))
             (index2 (position #\Space comment :start index1))
             (ratio  (/ (parse-integer (subseq comment 0 index1))
                        (parse-integer (subseq comment (1+ index1) index2))
                        )))
        (heat->color (* 1.5 ratio))))))

(define-presentation-method present ((object instruction)
                                     (type   instruction)
                                     (stream extended-output-stream)
                                     (view   t)
                                     &key)
  (let ((offset  (offset object))
        (label   (label object))
        (bytes   (bytes object))
        (decoded (decoded object))
        (comment (comment object)))
    (formatting-cell (stream :align-x :right)
      (when offset
        (with-drawing-options (stream :ink +darkgray+ :text-size :smaller)
          (format stream "~X" offset))))
    (formatting-cell (stream)
      (when (and show-label label)
        (with-drawing-options (stream :ink +dark-slate-blue+ :text-family :fix :text-face :bold)
          (write-string label stream))))
    (formatting-cell (stream)
      (when bytes
        (with-drawing-options (stream :ink +darkgray+ :text-family :fix :text-size :smaller)
          (write-string bytes stream))))
    (formatting-cell (stream)
      (when decoded
        (with-drawing-options (stream :text-family :fix :text-size :smaller)
          (write-string decoded stream))))
    (formatting-cell (stream)
      (when comment
        (with-drawing-options (stream :ink (or (instruction-color object) +firebrick+) :text-family :fix)
          (write-string comment stream))))))
