;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: calcuator.lisp,v 1.0 22/08/200 $

;;;  (c) copyright 2000 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;	      Robert Strandh (strandh@labri.u-bordeaux.fr)

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

(in-package :CLIM-DEMO)

(defun calculator ()
  (loop for port in climi::*all-ports*
      do (destroy-port port))
  (setq climi::*all-ports* nil)
  (run-frame-top-level (make-application-frame 'calculator)))

(defun show (number)
  (setf (gadget-value (slot-value *application-frame* 'text-field))
	(princ-to-string number)))

(defun queue-number (number)
  (lambda (gadget)
    (declare (ignore gadget))
    (with-slots (state) *application-frame*
      (if (numberp (first state))
	  (setf (first state) (+ (* 10 (first state)) number))
	  (push number state))
      (show (first state)))))

(defun queue-operator (operator)
  (lambda (gadget)
    (declare (ignore gadget))
    (do-operation t)
    (with-slots (state) *application-frame*
      (if (functionp (first state))
	  (setf (first state) operator)
	  (push operator state)))))

(defun do-operation (gadget)
  (declare (ignore gadget))
  (with-slots (state) *application-frame*
    (when (= 3 (length state))
      (setf state (list (funcall (second state) (third state) (first state))))
      (show (first state)))))

(defun initac (gadget)
  (declare (ignore gadget))
  (with-slots (state) *application-frame*
    (setf state (list 0)))
  (show 0))

(defun initce (gadget)
  (declare (ignore gadget))
  (with-slots (state) *application-frame*
    (when (numberp (first state))
      (pop state))
    (show 0)))
      
(defmethod calculator-frame-top-level ((frame application-frame)
				       &key (command-parser 'command-line-command-parser)
				       (command-unparser 'command-line-command-unparser)
				       (partial-command-parser
					'command-line-read-remaining-arguments-for-partial-command)
				       (prompt "Command: "))
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (setf (slot-value frame 'text-field)
	(find-if #'(lambda (gadget) (typep gadget 'text-field-pane))
		 (frame-panes frame)))
  (loop (event-read (frame-pane frame))))
     
(define-application-frame calculator ()
  ((text-field :initform nil)
   (state :initform (list 0)))
  (:panes
   (plus             :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "+"
		     :activate-callback (queue-operator #'+))
   (dash             :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "-"
		     :activate-callback (queue-operator #'-))
   (multiplicate     :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "*"
		     :activate-callback (queue-operator #'*))
   (divide           :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "/"
		     :activate-callback (queue-operator #'round))
   (result           :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "="
		     :activate-callback #'do-operation)
   (one              :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "1"
		     :activate-callback (queue-number 1))
   (two              :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "2"
		     :activate-callback (queue-number 2))
   (three            :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "3"
		     :activate-callback (queue-number 3))
   (four             :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "4"
		     :activate-callback (queue-number 4))
   (five             :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "5"
		     :activate-callback (queue-number 5))
   (six              :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "6"
		     :activate-callback (queue-number 6))
   (seven            :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "7"
		     :activate-callback (queue-number 7))
   (eight            :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "8"
		     :activate-callback (queue-number 8))
   (nine             :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "9"
		     :activate-callback (queue-number 9))
   (zero             :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "0"
		     :activate-callback (queue-number 0))
  (screen            :text-field
		     :value "0"
		     :space-requirement (make-space-requirement :width 200 :height 50))
  (ac                :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "AC"
		     :activate-callback #'initac)
  (ce                :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "CE"
		     :activate-callback #'initce))
  
  (:layouts
   (defaults (vertically ()
	       screen
	       (horizontally () ac ce)
	       (tabling ()
		 (list one two plus)
		 (list three four dash)
		 (list five six multiplicate)
		 (list seven eight divide)
		 (list nine zero result)))))
  (:top-level (calculator-frame-top-level . nil)))

