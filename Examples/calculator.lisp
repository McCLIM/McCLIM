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
  (progn
    (loop for port in climi::*all-ports*
        do (destroy-port port))
    (setq climi::*all-ports* nil))
  (let ((frame (make-application-frame 'calculator)))
    (unless clim-sys:*multiprocessing-p*
      (run-frame-top-level frame))
    frame))

(defun show (number)
  (setf (gadget-value (find-if #'(lambda (gadget) (typep gadget 'text-field-pane))
                               (frame-panes *application-frame*)))
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
  (loop (event-read (climi::frame-pane frame))))
     
(defmacro make-button (label operator 
                       &key width height
                            max-width min-width
                            max-height min-height)
  `(make-pane 'push-button-pane
              :label ,label
              :name ,label
              :activate-callback (let ((ap *application-frame*))
                                   (lambda (&rest xs)
                                     (let ((*application-frame* ap))
                                       (apply ,operator xs))))
              :width ,width :height ,height
              :max-width ,max-width :min-width ,min-width
              :max-height ,max-height :min-height ,min-height) )

(define-application-frame calculator ()
  ((text-field :initform nil)
   (state :initform (list 0)))
  (:panes
   (plus     (make-button "+" (queue-operator #'+)))
   (dash     (make-button "-" (queue-operator #'-)))
   (multiply (make-button "*" (queue-operator #'*)))
   (divide   (make-button "/" (queue-operator #'round)))
   (result   (make-button "=" #'do-operation))
   (one      (make-button "1" (queue-number 1)))
   (two      (make-button "2" (queue-number 2)))
   (three    (make-button "3" (queue-number 3)))
   (four     (make-button "4" (queue-number 4)))
   (five     (make-button "5" (queue-number 5)))
   (six      (make-button "6" (queue-number 6)))
   (seven    (make-button "7" (queue-number 7)))
   (eight    (make-button "8" (queue-number 8)))
   (nine     (make-button "9" (queue-number 9)))
   (zero     (make-button "0" (queue-number 0)))
   (screen   :text-field :value "0"
             ;:background +black+
             ;:foreground +white+
             )
   (ac       (make-button "AC" #'initac :max-width 150))
   (ce       (make-button "CE" #'initce :max-width 150)))

  (:layouts
   #+NIL
   (defaults 
       (spacing (:thickness 10)
         (vertically (:width '(16 :character) :equalize-width t);; (:width 150 :height 310)
           screen
           (horizontally (#|:height 50|#) ac ce)
           (horizontally (:equalize-height t :background +green+)
             (tabling (:background +red+)
               (list one two three)
               (list four five six )
               (list seven eight nine)))
           (vertically (:background +blue+)
             plus dash multiply divide result))
         (horizontally ()
           (1/2 zero))))
             

   (defaults 
       (progn 
         (vertically (:width 140 :equalize-width t);; (:width 150 :height 310)
           (labelling (:label "screen")
             screen)
           (horizontally (#|:height 50|# :name "ACCE" :equalize-height t) ac ce)
           (tabling ()
             (list one two plus)
             (list three four dash)
             (list five six multiply)
             (list seven eight divide)
             (list nine zero result))))))
  (:top-level (calculator-frame-top-level . nil)))

