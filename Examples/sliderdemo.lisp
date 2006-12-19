;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;;;  (c) copyright 2000 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)

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

(in-package :clim-demo)

(defparameter calc '(0))
(defvar *text-field* nil)

(defun slidertest ()
  (loop for port in climi::*all-ports*
      do (destroy-port port))
  (setq climi::*all-ports* nil)
  (let ((frame (make-application-frame 'sliderdemo)))
    (run-frame-top-level frame)))

(defmacro queue-number(int)
  `(lambda (gadget)
     (declare (ignore gadget))
     (let ((last-item (first (last calc))))
       (if (numberp last-item)
	   (setf (car (last calc)) (+ (* 10 last-item) ,int))
	   (setf calc (nconc calc (list ,int))))
       (setf (gadget-value *text-field*) (princ-to-string (first (last calc)))))))

(defmacro queue-operator (operator)
  `(lambda (gadget)
     (declare (ignore gadget))
     (do-operation t)
     (if (functionp (first (last calc)))
	 (setf (first (last calc)) ,operator)
       	 (setf calc (nconc calc (list ,operator))))))

(defun do-operation (gadget)
  (declare (ignore gadget))
  (when (= 3 (length calc))
    (setf (car calc) (apply (second calc) (list (first calc) (third calc)))
	  (cdr calc) nil)
    (setf (gadget-value *text-field*) (princ-to-string (first calc)))))

(defun initac (gadget)
  (declare (ignore gadget))
  (setf calc (list 0)
	(gadget-value *text-field*) (princ-to-string 0)))

(defun initce (gadget)
  (declare (ignore gadget))
  (let ((last-item (first (last calc))))
    (unless (or (null calc) (not (numberp last-item)))
      (setf calc (butlast calc)
	    (gadget-value *text-field*) (princ-to-string 0)))))

(defun print-screen (gadget)
  (declare (ignore gadget)))

(defun slide (gadget value)
  (declare (ignore gadget))
  (setf (gadget-value *text-field*) (princ-to-string value)))

(defun find-text-field (frame)
  (first (member-if #'(lambda (gadget) (typep gadget 'text-field))
		    (frame-panes frame))))

(defmethod sliderdemo-frame-top-level ((frame application-frame)
				       &key (command-parser 'command-line-command-parser)
				       (command-unparser 'command-line-command-unparser)
				       (partial-command-parser
					'command-line-read-remaining-arguments-for-partial-command)
				       (prompt "Command: "))
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (setf *text-field* (find-text-field frame))
  (clim-extensions:simple-event-loop))

(define-application-frame sliderdemo () ()
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
		     :activate-callback #'initce)
  (slider            :slider
		     :value-changed-callback #'slide
		     :min-value 0
		     :max-value 100
		     :value 0
		     :normal +white+
		     :highlighted +cyan+
		     :pushed-and-highlighted +blue+))

  (:layouts
   (defaults (horizontally ()
	        (vertically ()
		   screen
		   (horizontally () ac ce)
		   (tabling ()
		      (list one two plus)
		      (list three four dash)
		      (list five six multiplicate)
		      (list seven eight divide)
		      (list nine zero result)))
		slider)))
  (:top-level (sliderdemo-frame-top-level . nil)))

