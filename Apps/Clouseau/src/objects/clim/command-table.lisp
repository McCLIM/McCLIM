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

;;; Places

(defclass related-command-table-place (pseudo-place)
  ())

(defmethod make-object-state ((object t) (place related-command-table-place))
  (make-instance (object-state-class object place) :place place
                                                   :style :name-only))

;;; Object states

(defclass inspected-command-table (inspected-instance
                                   remembered-collapsed-style-mixin)
  ())

(defmethod object-state-class ((object command-table) (place t))
  'inspected-command-table)

;;;

(defmethod inspect-object-using-state ((object command-table)
                                       (state inspected-command-table)
                                       (style (eql :name-only))
                                       (stream extended-output-stream))
  (princ (command-table-name object) stream))

(defmethod inspect-object-using-state ((object command-table)
                                       (state inspected-command-table)
                                       (style (eql :expanded-header))
                                       (stream extended-output-stream))
  (call-next-method)
  (write-char #\Space stream)
  (princ (command-table-name object) stream))

(defmethod inspect-commands ((table  command-table)
                             (style  (eql :flat))
                             (stream t))
  (let ((menu-items (make-hash-table :test #'eq)))
    (map-over-command-table-keystrokes
     (lambda (menu-name keystroke item)
       (setf (gethash (command-menu-item-value item) menu-items)
             (list menu-name keystroke)))
     table)
    (formatting-table (stream)
      (with-style (stream :header)
        (formatting-row (stream) ; TODO do we have a macro for table headers?
          (formatting-cell (stream) (write-string "Command line name" stream))
          (formatting-cell (stream) (write-string "Name" stream))
          (formatting-cell (stream) (write-string "Menu?" stream))
          (formatting-cell (stream) (write-string "Gesture?" stream))
          (formatting-cell (stream) (write-string "Where" stream))))
      (map-over-command-table-commands
       (lambda (name)
         (formatting-row (stream)
           (formatting-cell (stream)
             (let ((name (command-line-name-for-command name table :errorp nil)))
               (with-placeholder-if-empty (stream)
                 ((not name)
                  "no name")
                 (t
                  (princ name stream))))
             ;; TODO indicate disabled commands
             )
           (formatting-cell (stream)
             (princ name stream))
           (formatting-cell (stream)
             (princ (first (gethash name menu-items)) stream))
           (formatting-cell (stream)
             (let ((gesture (second (gethash name menu-items))))
               (typecase gesture
                 ((cons (eql :keyboard))
                  (flet ((as-key (thunk)
                           (with-preserved-cursor-y (stream)
                             (surrounding-output-with-border (stream :shape :rounded :background +light-gray+ :outline-ink +dark-gray+ :radius 2  :padding-y 1 :padding-x 6)
                               (with-text-size (stream :small)
                                 (funcall thunk stream))))))
                    (let ((modifiers (third gesture))) ; TODO we already have something like this
                      (when (plusp modifiers)
                        (as-key (lambda (stream)
                                  (when (logtest climi::+meta-key+ modifiers)
                                    (write-string "Alt" stream))
                                  (when (logtest climi::+control-key+ modifiers)
                                    (write-string "Control" stream))))
                        (write-string " + " stream)))
                    (as-key (lambda (stream)
                              (princ (second gesture) stream)))))
                 (t
                  (princ gesture stream)))))
           #+no (formatting-cell (stream)
                  (princ command stream))
           (formatting-cell (stream)
             (unless (command-present-in-command-table-p name table)
               (with-style (stream :note)
                 (write-string "inherited" stream))))))
       table :inherited t))))

(defmethod inspect-commands ((table  command-table)
                             (style  (eql :by-command-table))
                             (stream t))
  ;; TODO avoid duplicated
  (climi::apply-with-command-table-inheritance
   (lambda (ancestor)
     (with-section (stream)
         (with-drawing-options (stream :text-size :smaller)
           (cond ((eq ancestor table)
                  (write-string "Direct commands" stream))
                 (t
                  (write-string "Inherited from " stream)
                  (formatting-place (table 'related-command-table-place ancestor nil present)
                    (present stream)))))
       (formatting-table (stream)
         (map-over-command-table-commands
          (lambda (name)
            (formatting-row (stream)
              (formatting-cell (stream)
                (let ((name (command-line-name-for-command name table :errorp nil)))
                  (with-placeholder-if-empty (stream)
                    ((not name)
                     "no name")
                    (t
                     (princ name stream)))))
              (formatting-cell (stream)
                (princ name stream))
              #+no (formatting-cell (stream)
                     (princ command stream))))
          ancestor :inherited nil))))
   table))

(defmethod inspect-object-using-state ((object command-table)
                                       (state inspected-command-table)
                                       (style (eql :expanded-body))
                                       (stream extended-output-stream))
  (with-section (stream) "Inheritance"
    (format-graph-from-root
     object
     (lambda (table stream)
       (formatting-place (nil 'related-command-table-place table nil inspect)
         (inspect stream))
                                        ; (princ (command-table-name table) stream)
       )
     (lambda (node)
       (alexandria:mappend
        (lambda (parent-name)
          (when-let ((parent (find-command-table parent-name)))
            (list parent)))
        (command-table-inherit-from node)))
     :stream stream :merge-duplicates t :maximize-generations t))
  (fresh-line stream)

  ;; TODO placeholder if no commands
  (with-section (stream) "Commands"
    (inspect-commands object :flat stream))

  (call-next-method))
