;;;; (C) Copyright 2019, 2020 Jan Moringen
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

(in-package #:clim-internals)

;;; Frame class redefinition notification protocol
;;;
;;; This is used for notifying application frame classes after aspects
;;; such as the class itself, the `generate-panes' method or the
;;; command table have been redefined. The notification sequence
;;; begins with a call to `note-frame-class-redefined' which calls
;;; `handle-frame-class-redefinition'.
;;;
;;; This protocol is probably most commonly initiated by re-evaluating
;;; the `define-application-frame' form that defined the frame class
;;; in question, but `note-frame-class-redefined' may be called by a
;;; programmer at any time.

;;; `frame-class-redefined-event'
;;;
;;; Sent to frame instances whose class has been redefined. Causes the
;;; following actions in the frame receiving the event:
;;;
;;; 1) A new command table is installed in the frame.
;;;
;;; 2) The current layout of the frame is reset to the first layout in
;;;    the layout list of the frame.
;;;
;;; 3) Panes are regenerated (actually reinitialized where possible)
;;;    for the potentially changed layout.

(define-event-class frame-class-redefined-event (standard-event)
  ((%new-layouts       :initarg :new-layouts
                       :reader  new-layouts)
   (%new-command-table :initarg :new-command-table
                       :reader  new-command-table)))

(defmethod handle-event ((client application-frame)
                         (event  frame-class-redefined-event))
  ;; We try to preserve the frame size. Store the current size before
  ;; we make any changes.
  (multiple-value-bind (width height)
      (bounding-rectangle-size (frame-top-level-sheet client))
    (setf (frame-command-table client) (new-command-table event)
          (%frame-layouts client)      (new-layouts event))
    (let ((new-layout (first (frame-all-layouts client))))
      ;; Changing the layout involves signaling FRAME-LAYOUT-CHANGED
      ;; which performs a non-local exit out of this method.
      (if (eq (frame-current-layout client) new-layout)
          (when-let ((frame-manager (frame-manager client)))
            (generate-panes frame-manager client)
            (layout-frame client width height)
            (signal 'frame-layout-changed :frame client))
          (unwind-protect ; Call LAYOUT-FRAME despite non-local exit.
               (setf (frame-current-layout client) new-layout)
            (layout-frame client width height))))))

(defun note-frame-class-redefined (class)
  (handle-frame-class-redefinition class))

(defun handle-frame-class-redefinition
    (class &aux (class-name (class-name class)))
  (if-let ((instances (member-if #'tg:weak-pointer-value
                                 (get class-name 'instances))))
    (let ((remaining         '())
          (new-layouts       nil)
          (new-command-table nil))
      (when-let ((initargs (c2mop:class-default-initargs class)))
        (flet ((initarg-value (name)
                 ;; Third element of the canonicalized default initarg is a
                 ;; thunk which when called evaluates a default value form
                 ;; in its proper lexical context and returns the result.
                 (when-let ((initarg (find name initargs :key #'first)))
                   (funcall (third initarg)))))
          (setf new-layouts       (initarg-value :layouts)
                new-command-table (initarg-value :command-table))))
      (loop for weak-instance in instances
            do
               ;; Update instances that haven't become garbage.
               (when-let ((live-instance (tg:weak-pointer-value weak-instance)))
                 (when (typep live-instance class)
                   (push weak-instance remaining)
                   (with-simple-restart
                       (continue "~@<Skip updating frame ~A~@:>" live-instance)
                     (event-queue-append
                      (frame-event-queue live-instance)
                      (make-instance 'frame-class-redefined-event
                                     :sheet             live-instance
                                     :new-layouts       new-layouts
                                     :new-command-table new-command-table)))))
            finally
               (setf (get class-name 'instances) remaining)))
    (setf (get class-name 'instances) nil)))
