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

;;; Model

(defclass history ()
  ((%elements :initarg  :elements
              :accessor elements
              :initform '())))

(defmethod push-element ((element t) (history history))
  (unless (eq element (first (elements history)))
    (push element (elements history))))

(defmethod pop-element ((history history))
  (pop (elements history))
  (first (elements history)))

(defmethod clear ((history history))
  (setf (elements history) '()))

;;; Presentations

(define-presentation-type element (&key (selectedp nil)))
(define-presentation-type history (&key selected))

(define-presentation-method present ((object t)
                                     (type   element)
                                     (stream t)
                                     (view   t)
                                     &key)
  (flet ((print-it (stream)
           (with-print-error-handling (stream)
             (with-safe-and-terse-printing (stream)
               (let ((string (prin1-to-string (value object))))
                 (print-string-compactly string stream :delimitersp nil))))))
    (if selectedp
        (with-drawing-options (stream :text-face :bold)
          (print-it stream))
        (print-it stream))))

(define-presentation-method present ((object t)
                                     (type   history)
                                     (stream t)
                                     (view   t)
                                     &key)
  (loop :with firstp = t
        :for element :in (reverse (elements object))
        :for selectedp = (eq element selected)
        :do (if firstp
                (setf firstp nil)
                (write-string " » " stream))
            (present element `(element :selectedp ,selectedp)
                     :stream     stream
                     :single-box t)))

;;; Pane

(defclass history-pane (application-pane)
  (;; The inspector state this history should observe and manipulate.
   (%state   :initarg  :state
             :reader   state)
   (%history :initarg  :history
             :reader   history
             :initform (make-instance 'history)))
  (:default-initargs
   :end-of-line-action :allow
   :state              (error "Missing required initarg :state")))

(defmethod initialize-instance :after ((instance history-pane) &key)
  ;; Observe the inspector state, extending or at least redisplaying
  ;; the history when the root object changes.
  (let ((history (history instance)))
    (push (lambda (old-root-place new-root-place)
            (unless (or (eq old-root-place new-root-place)
                        (find old-root-place (elements history) :test #'eq))
              (push-element old-root-place history)
              (redisplay-frame-pane (pane-frame instance) instance)))
          (change-hook (state instance)))))

(defmethod redisplay-frame-pane ((frame application-frame)
                                 (pane  history-pane)
                                 &key force-p)
  (declare (ignore force-p))
  (stream-increment-cursor-position pane 4 4)
  (let* ((history      (history pane))
         (selected     (root-place (state pane)))
         (presentation (present history `(history :selected ,selected)
                                :stream pane :single-box t)))
    (when-let ((viewport (pane-viewport pane))
               (record   (find-if (lambda (record)
                                    (and (presentationp record)
                                         (eq (presentation-object record)
                                             selected)))
                                  (output-record-children presentation))))
      (scroll-extent pane (bounding-rectangle-min-x record) 0))))

;;; Commands

(define-command-table navigation-command-table)

(defun history-pane ()
  (map-over-sheets (lambda (sheet)
                     (when (typep sheet 'history-pane)
                       (return-from history-pane sheet)))
                   (frame-panes *application-frame*)))

(macrolet ((with-history ((state-var &optional history-var) &body body)
             (with-unique-names (pane)
               `(let* ((,pane (history-pane))
                       ,@(when state-var
                           `((,state-var (state ,pane))))
                       ,@(when history-var
                           `((,history-var (history ,pane)))))
                  ,@body))))

  (define-command (com-select :command-table navigation-command-table
                              :name          "Inspect object")
      ((object inspected-object :gesture :describe))
    (with-history (state)
      (setf (root-object state :run-hook-p t) (object object)))) ; TODO getting the object can fail

  (define-command (com-visit :command-table navigation-command-table
                             :name          t)
      ((object element :gesture :select))
    (with-history (state)
      (setf (root-place state :run-hook-p t) object)))

  (define-command (com-back :command-table navigation-command-table
                            :name          t
                            :keystroke     (#\l :meta))
      ()
    (with-history (state history)
      (setf (root-place state :run-hook-p t) (pop-element history))))

  (define-command (com-clear :command-table navigation-command-table
                             :name          t)
      ()
    (with-history (nil history)
      (clear history))))
