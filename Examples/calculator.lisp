;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: calcuator.lisp,v 1.0 22/08/200 $

(in-package :CLIM-DEMO)

(defparameter calc '(0))

(defun calculator ()
  (loop for port in climi::*all-ports*
      do (destroy-port port))
  (setq climi::*all-ports* nil)
  (setq frame (make-application-frame 'calculator))
  (setq fm (frame-manager frame))
  (setq port (climi::frame-manager-port fm))
  (setq pane (third (frame-panes frame)))
  (setq medium (sheet-medium pane))
  (setq graft (graft frame))
  (setq vbox (frame-pane frame))
  (run-frame-top-level frame))

(defmacro treat (int)
  `(lambda (x)
     (declare (ignore x))
     (format t (princ-to-string ,int))
;     (draw-text* ,pane (princ-to-string ,int) 10 10 :align-x left)
     (append calc (list ,int))))

(defmacro conc (char)
  `(lambda (x)
     (result x)
     (format t "~%")
     (append calc (list ,char))))

(defun result (x)
  (declare (ignore x)))

(defun print-screen (x)
  (declare (ignore x)))

(defun do-print-result (application)
  (declare (ignore x)))

(defun initac (x)
  (declare (ignore x))
  (setf calc '(0))
  (format t "~%0~%"))

(defun initce (x)
  (declare (ignore x)))

(defmethod calculator-frame-top-level ((frame application-frame)
				       &key (command-parser 'command-line-command-parser)
				       (command-unparser 'command-line-command-unparser)
				       (partial-command-parser
					'command-line-read-remaining-arguments-for-partial-command)
				       (prompt "Command: "))
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (let ((*standard-input* (frame-standard-input frame))
	(*standard-output* (frame-standard-output frame))
	(*query-io* (frame-query-io frame))
	(print-screen (last (frame-panes frame))))
    (setf (cursor-visibility (stream-text-cursor *standard-input*)) nil)
    (loop
     ;; don't know why this line is needed
     (read *standard-input*))))

(define-application-frame calculator () ()
  (:panes
   (print-screen     :application
		     :space-requirement (make-space-requirement :width 200 :height 50)
		     :incremental-redisplay t
		     :display-function #'print-screen)
   (plus             :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "+"
		     :activate-callback (conc #\+))
   (dash             :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)
		     :label "-"
		     :activate-callback (conc #\-))
   (multiplicate     :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "*"
		     :activate-callback (conc #\*))
   (divide           :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "/"
		     :activate-callback (conc #\/))
   (result           :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "="
		     :activate-callback #'result)
   (one              :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "1"
		     :activate-callback (treat 1))
   (two              :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "2"
		     :activate-callback (treat 2))
   (three            :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "3"
		     :activate-callback (treat 3))
   (four             :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "4"
		     :activate-callback (treat 4))
   (five             :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "5"
		     :activate-callback (treat 5))
   (six              :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "6"
		     :activate-callback (treat 6))
   (seven            :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "7"
		     :activate-callback (treat 7))
   (eight            :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "8"
		     :activate-callback (treat 8))
   (nine             :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "9"
		     :activate-callback (treat 9))
   (zero             :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "0"
		     :activate-callback (treat 0))
   (ac               :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)     
		     :label "AC"
		     :activate-callback #'initac)
   (ce               :push-button
		     :space-requirement (make-space-requirement :width 50 :height 50)		     
		     :label "CE"
		     :activate-callback #'initce))
  (:layouts
   (defaults (vertically ()
		print-screen
		(horizontally () ac ce)
		(tabling ()
		   (list one two plus)
		   (list three four dash)
		   (list five six multiplicate)
		   (list seven eight divide)
		   (list nine zero result)))))
  (:top-level (calculator-frame-top-level . nil)))
