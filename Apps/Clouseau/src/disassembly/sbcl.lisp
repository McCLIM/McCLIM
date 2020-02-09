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

;;;; Model

(defmethod unconditional-jump-p ((instruction instruction))
  (starts-with-subseq "JMP" (decoded instruction)))

(defmethod conditional-jump-p ((instruction instruction))
  (and (starts-with-subseq "J" (decoded instruction))
       (not (starts-with-subseq "JMP" (decoded instruction)))))

(defmethod returnp ((instruction instruction))
  (or (starts-with-subseq "RET" (decoded instruction))
      (and (starts-with-subseq "BREAK" (decoded instruction))
           (not (starts-with-subseq "BREAK 9" (decoded instruction))))
      (starts-with-subseq "BYTE" (decoded instruction))))

;;;; Parsing

(defun parse-line (line)
  ;; We want to cut out the first two characters of every line, which
  ;; are always going to be "; ", and if anything is left print it.
  (prog (offset label bytes decoded comment
         (index     0)
         (semicolon (position #\; line)))
   :offset
     (let ((end (position #\: line)))
       (if (and end (every (rcurry #'digit-char-p 16)
                           (subseq line 0 end))
                (or (not semicolon) (< end semicolon)))
           (setf offset (parse-integer (subseq line 0 end) :radix 16)
                 index  (+ end 2))
           (go :comment)))
   :label
     (when-let ((colon (position #\: line :start index :end semicolon)))
       (setf label (subseq line index colon)
             index colon))
   :bytes
     (let* ((skip  (search ".SKIP" line :start2 index))
            (start (position-if (alexandria:rcurry #'digit-char-p 16)
                                line :start index))
            (end   (position-if-not (alexandria:rcurry #'digit-char-p 16)
                                    line :start start)))
       (cond ((and skip (or (not start) (< skip start)))
              (go :end))
             ((and start (or (not skip) (< start skip)))
              (setf bytes (subseq line start end)
                    index end))))
   :decoded
     (let* ((start (position #\Space line :start index :test-not #'char=))
            (end   (1+ (position #\Space line :end semicolon :from-end t :test-not #'char=))))
       (setf decoded (subseq line start end)))
   :comment
     (setf comment (cond (semicolon
                          (subseq line (+ semicolon 2)))
                         ((not offset)
                          line)))
   :end
     (return (values offset label bytes decoded comment))))

(defun parse-disassembly (disassembly-string)
  (with-input-from-string (stream disassembly-string)
    (loop with block = (make-instance 'basic-block :label nil)

          for line = (read-line stream nil nil)
          while line   ; (and line (not (parse-line (subseq line 2))))
          for (offset label bytes decoded comment) =
             (multiple-value-list (parse-line (subseq line 2)))
          for instruction = (when offset ; TODO necessary?
                              (make-instance 'instruction :offset  offset
                                                          :label   label
                                                          :bytes   bytes
                                                          :decoded decoded
                                                          :comment comment))

          when (and label (instructions block))
          collect block :into blocks
          and do (let* ((new-block (make-instance 'basic-block :label label))
                        (successor (make-instance 'fall-through-successor
                                                  :basic-block new-block)))
                   (push successor (successors block))
                   (setf block new-block))
          when (and label (not (instructions block)))
          do (reinitialize-instance block :label label)

          when offset
          do (appendf (instructions block) (list instruction))

          when (and instruction (or (unconditional-jump-p instruction)
                                    (conditional-jump-p instruction)
                                    (returnp instruction)))
          collect block :into blocks
          and do (let ((new-block (make-instance 'basic-block :label label)))
                   (when (conditional-jump-p instruction)
                     (let ((successor (make-instance 'false-successor
                                                     :basic-block new-block)))
                      (push successor (successors block))))
                   (setf block new-block))

          finally (return (list* block blocks)))))

(defun link-blocks (blocks)
  (let ((label->block (make-hash-table :test #'equal)))
    (map nil (lambda (block)
               (setf (gethash (label block) label->block) block))
         blocks)
    (map nil (lambda (block)
               (when (instructions block) ; HACK
                 (let* ((jump    (last-elt (instructions block)))
                        (decoded (decoded jump)))
                   (cond ((or (unconditional-jump-p jump)
                              (conditional-jump-p jump))
                          ;; TODO there should definitely be a successor
                          (when-let* ((successor (gethash (subseq decoded (1+ (position #\Space decoded)))
                                                          label->block))
                                      (successor (make-instance 'true-successor
                                                                :basic-block successor)))
                            (push successor (successors block))))))))
         blocks)
    blocks))

(defun display-disassembly (object stream)
  (terpri stream)
  (with-error-handling (stream "Error disassembling function")
    (let* ((disassembly-string (with-output-to-string (*standard-output*)
                                 (sb-disassem:disassemble-code-component object)))
           (blocks             (parse-disassembly disassembly-string))
           (graph              (link-blocks blocks)))
      (present graph 'basic-blocks :stream stream :view (make-instance 'disassembly-text-view
                                                                       #+no 'basic-block-graph-view)))))
