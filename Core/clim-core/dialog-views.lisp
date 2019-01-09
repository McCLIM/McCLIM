;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2005 by Tim Moore (moore@bricoworks.com)
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

(in-package :clim-internals)

;;; Classes for the gadget dialog views. Eventually.

;;; Views described in the Franz User manual (CLIM 2.2). Sec. 8.6.13

(macrolet
    ((define-gadget-view (name &optional new-slots)
       (let ((class-name (alexandria:symbolicate name '-view))
             (variable-name (alexandria:symbolicate '+ name '-view+))
             (slots (c2mop:class-slots (c2mop:ensure-finalized (find-class name)))))
         `(progn
            (defclass ,class-name (gadget-view)
              ((gadget-name :allocation :class :reader view-gadget-name :initform ',name)
               (gadget-slots :allocation :class :reader view-gadget-slots
                             :initform
                             ',(loop for slot in slots
                                  when (c2mop:slot-definition-initargs slot)
                                  collect (list (c2mop:slot-definition-name slot) (car (c2mop:slot-definition-initargs slot)))))
               ,@new-slots
               ,@(loop for slot in slots
                    when (c2mop:slot-definition-initargs slot)
                    collect
                      (list (c2mop:slot-definition-name slot)
                            :initarg (car (c2mop:slot-definition-initargs slot))
                            :type (c2mop:slot-definition-type slot)))))
            (defvar ,variable-name (make-instance ',class-name))
            ',name))))

  (define-gadget-view toggle-button)
  (define-gadget-view push-button)
  (define-gadget-view radio-box)
  (define-gadget-view check-box)
  (define-gadget-view slider)
  (define-gadget-view text-field
      ((width :accessor width :initarg :width :initform nil)))
  (define-gadget-view text-editor)
  (define-gadget-view list-pane)
  (define-gadget-view option-pane))

(defmethod make-gadget-pane-from-view ((view gadget-view) stream &rest initargs)
  (let ((initargs (append initargs
                          (loop for slot in (view-gadget-slots view)
                             when (slot-boundp view (first slot))
                             append (list (second slot) (slot-value view (first slot)))))))
    (let ((gadget (with-look-and-feel-realization
                      ((frame-manager *application-frame*) *application-frame*)
                    (apply #'make-pane (view-gadget-name view) initargs))))
      gadget)))

(defmethod make-output-record-from-view ((view gadget-view) stream query-identifier &rest initargs)
  (updating-output (stream
                    :cache-value t      ; don't redisplay
                    :unique-id query-identifier
                    :record-type 'accepting-values-record)
                   (let ((gadget (apply #'make-gadget-pane-from-view view stream initargs)))
                     (with-output-as-gadget (stream)
                       gadget))))

(defun %standard-value-changed-callback (query-identifier)
  (lambda (pane item)
    (declare (ignore pane))
    (throw-object-ptype `(com-change-query ,query-identifier ,item)
                        '(command :command-table accept-values))))

;;; Use textual-dialog-view as default for Views not implemented

(define-default-presentation-method accept-present-default
    (type stream (view gadget-view) default default-supplied-p
          present-p query-identifier)
  (funcall-presentation-generic-function accept-present-default
                                         type
                                         stream
                                         +textual-dialog-view+
                                         default default-supplied-p
                                         present-p
                                         query-identifier))

(define-default-presentation-method accept
    (type stream (view gadget-view) &key default default-type)
  (funcall-presentation-generic-function accept
                                         type
                                         stream
                                         +textual-dialog-view+
                                         :default default
                                         :default-type default-type))

;;; toggle-button-view

(define-presentation-method accept-present-default
    ((type boolean) stream (view toggle-button-view) default default-supplied-p
     present-p query-identifier)
  (unless default-supplied-p
    (setq default nil))
  (make-output-record-from-view view stream query-identifier
                                :value default
                                :value-changed-callback
                                (%standard-value-changed-callback query-identifier)))

;;; radio-box-view

(define-presentation-method accept-present-default
    ((type completion) stream (view radio-box-view) default default-supplied-p
     present-p query-identifier)
  (let ((buttons (loop for choice in sequence collect
                      (make-pane 'toggle-button
                                 :indicator-type :one-of
                                 :id (funcall value-key choice)
                                 :label (funcall name-key choice)))))
    (make-output-record-from-view view stream query-identifier
                                  :choices buttons
                                  :current-selection (find default buttons :test test :key #'gadget-id)
                                  :value-changed-callback
                                  (lambda (pane item)
                                    (let ((value (gadget-id item)))
                                      (throw-object-ptype `(com-change-query ,query-identifier ,value)
                                                          '(command :command-table accept-values)))))))

;;; check-box-view

(define-presentation-method accept-present-default
    ((type subset-completion) stream (view check-box-view) default default-supplied-p
     present-p query-identifier)
  (let ((buttons (loop for choice in sequence collect
                      (make-pane 'toggle-button
                                 :indicator-type :some-of
                                 :id (funcall value-key choice)
                                 :label (funcall name-key choice)))))
    (make-output-record-from-view view stream query-identifier
                                  :choices buttons
                                  :current-selection
                                  (remove-if-not (lambda (x) (find (gadget-id x) default :test test)) buttons)
                                  :value-changed-callback
                                  (lambda (pane item)
                                    (let ((value (map 'list #'gadget-id item)))
                                      (throw-object-ptype `(com-change-query ,query-identifier ,value)
                                                          '(command :command-table accept-values)))))))

;;; option-pane-view

(define-presentation-method accept-present-default
    ((type completion) stream (view option-pane-view) default default-supplied-p
     present-p query-identifier)
  (flet ((generate-callback (query-identifier)
           (lambda (pane item)
             (declare (ignore pane))
             (when *accepting-values-stream*
               (when-let ((query (find query-identifier
                                       (queries *accepting-values-stream*)
                                       :key #'query-identifier :test #'equal)))
                 (setf (value query) item)
                 (setf (changedp query) t))))))
    (make-output-record-from-view view stream query-identifier
                                  :mode :exclusive
                                  :test test
                                  :value default
                                  :name-key name-key
                                  :value-key value-key
                                  :items sequence
                                  :value-changed-callback (generate-callback query-identifier))))

;;; A gadget that's not in the spec but which would  be useful.
(defclass pop-up-menu-view (gadget-dialog-view)
  ()
  (:documentation "A dialog view that presents the elements of a
COMPLETION presentation type as a pop-up menu."))

(defparameter +pop-up-menu-view+ (make-instance 'pop-up-menu-view))

;;; By storing these parameters and options from the COMPLETION
;;; presentation type  in this object, we avoid having to dig them
;;; out of the presentation type on each call to select-query. That
;;; would not be possible if we are accepting a subtype of COMPLETION.
(defclass av-pop-up-menu-record (accepting-values-record)
  ((pop-up-sequence :accessor pop-up-sequence :initform nil)
   (pop-up-test :accessor pop-up-test :initform nil)
   (pop-up-value-key :accessor pop-up-value-key :initform nil)
   (pop-up-name-key :accessor pop-up-name-key :initform nil)))

(define-presentation-method accept-present-default
    ((type completion) stream (view pop-up-menu-view)
     default default-supplied-p present-p query-identifier)
  (declare (ignore present-p))
  (unless default-supplied-p
    (setq default (funcall value-key (elt sequence 0))))
  (let ((record (updating-output (stream :unique-id query-identifier
				  :cache-value default
				  :record-type 'av-pop-up-menu-record)
		  (with-output-as-presentation
		      (stream query-identifier 'selectable-query)
		    (surrounding-output-with-border
			(stream :shape :inset :move-cursor t)
		      (write-string (funcall name-key default) stream))))))
    (setf (pop-up-sequence record) sequence)
    (setf (pop-up-test record) test)
    (setf (pop-up-value-key record) value-key)
    (setf (pop-up-name-key record) name-key)
    record))

(defgeneric select-query (stream query record)
  (:documentation "Does whatever is needed for input (e.g., calls accept) when
a query is selected for input. It is responsible for updating the
  query object when a new value is entered in the query field." ))

(defgeneric deselect-query (stream query record)
  (:documentation "Deselect a query field: turn the cursor off, turn off
highlighting, etc." ))

(defmethod select-query (stream query record)
  (declare (ignore stream query record))
  nil)

(defmethod deselect-query (stream query record)
  (declare (ignore stream query record))
  nil)

(defmethod select-query (stream query (record av-pop-up-menu-record))
  (declare (ignore stream))
  (let* ((value-key (pop-up-value-key record))
	 (name-key (pop-up-name-key record)))
    (multiple-value-bind (new-value item event)
	(menu-choose (map 'list
			  #'(lambda (item)
			      `(,(funcall name-key item)
				 :value ,(funcall value-key item)))
			  (pop-up-sequence record)))
      (declare (ignore item))
      (when event
	(setf (value query) new-value)
	(setf (changedp query) t)))))

(defmethod deselect-query (stream query (record av-pop-up-menu-record))
  (declare (ignore stream query))
  nil)

(defmethod finalize-query-record (query (record av-pop-up-menu-record))
  (declare (ignore query))
  nil)
