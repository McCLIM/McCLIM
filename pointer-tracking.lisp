;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Alexey Dejneka (adejneka@comail.ru)

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

;;; TODO:
;;;
;;; - Single/multiple window tracking.
;;;
;;; - Keyboard gestures.
;;;
;;; - Optimization
;;;
;;; - - too many repeated checks within a loop;
;;;
;;; - - remove invoke-tracking-pointer; remove unnecessary checks.

(in-package :clim-internals)

;;; The Spec specifies the tracking-pointer clause arguments as, e.g.,
;;; (&key presentation event x y), implying that the user must write
;;; the &key keyword, but real code doesn't do that. Check if &key is in
;;; the arg list and add it if it is not.
(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun fix-tracking-pointer-args (args)
   (unless (member '&allow-other-keys args)
     (setq args (append args '(&allow-other-keys))))
   (unless (eq (car args) '&key)
     (cons '&key args))))


(defmacro tracking-pointer
    ((sheet &rest args
            &key pointer multiple-window transformp context-type
            (highlight nil highlight-p))
     &body body)
  (declare (ignorable pointer multiple-window transformp context-type highlight))
  (when (eq sheet 't)
    (setq sheet '*standard-output*))
  (check-type sheet symbol)
  (loop
     with motion-events = (assoc :pointer-motion body)
     for event-name in '(:pointer-motion
                         :presentation
                         :pointer-button-press
                         :presentation-button-press
                         :pointer-button-release
                         :presentation-button-release
                         :keyboard)
     for (handler-args . handler-body) = (cdr (assoc event-name body))
     for handler-name = (if handler-body
                            (gensym (symbol-name event-name))
                            nil)
     when handler-body collect `(,handler-name ,(fix-tracking-pointer-args
						 handler-args)
				 ,@handler-body) into bindings
     and collect `#',handler-name into handler-names
     collect (if handler-name `#',handler-name nil) into handlers
     finally
     (return `(flet ,bindings
                (declare (dynamic-extent ,@handler-names))
	        ,(if motion-events
		     `(letf (((sheet-motion-hints ,sheet) nil))
		        (invoke-tracking-pointer ,sheet ,@handlers ,@args))
		     `(invoke-tracking-pointer ,sheet ,@handlers ,@args))))))


(defun invoke-tracking-pointer
    (sheet
     pointer-motion-handler presentation-handler
     pointer-button-press-handler presentation-button-press-handler
     pointer-button-release-handler presentation-button-release-handler
     keyboard-handler
     &key pointer multiple-window transformp (context-type t)
     (highlight nil highlight-p))
  ;; (setq pointer (port-pointer (port sheet))) ; FIXME
  (let ((port (port sheet))        
        (presentations-p (or presentation-handler
                             presentation-button-press-handler
                             presentation-button-release-handler)))
    (unless highlight-p (setq highlight presentations-p))
    (with-method (distribute-event :around ((port (eql port)) event)
                                   ;; XXX specialize on EVENT?
                                   ;; :SUPER-AROUND?
                                   (queue-event sheet event))
      (with-input-context (context-type :override t)  ()
        (loop
            (let ((event (event-read sheet)))
              (when (and (eq sheet (event-sheet event))
                         (typep event 'pointer-motion-event))
                (queue-event sheet event)
                (highlight-applicable-presentation (pane-frame sheet) sheet *input-context*))
              (cond ((and (typep event 'pointer-event)
                          #+nil
                          (eq (pointer-event-pointer event)
                              pointer))                     
                     (let* ((x (pointer-event-x event))
                            (y (pointer-event-y event))
                            (window (event-sheet event))
                            (presentation (and presentations-p
                                               (find-innermost-applicable-presentation
                                                *input-context*
                                                sheet ; XXX
                                                x y
                                                :modifier-state (event-modifier-state event)))))
                       (when (and highlight presentation)
                         (frame-highlight-at-position (pane-frame sheet) window x y))
                       ;; FIXME Convert X,Y to SHEET coordinates; user
                       ;; coordinates
                       (typecase event
                         (pointer-motion-event
                          (if (and presentation presentation-handler)
                              (funcall presentation-handler
                                       :presentation presentation
                                       :window window :x x :y y)
                              (maybe-funcall pointer-motion-handler
                                             :window window :x x :y y)))
                         (pointer-button-press-event
                          (if (and presentation presentation-button-press-handler)
                              (funcall presentation-button-press-handler
                                       :presentation presentation
                                       :event event :x x :y y)
                              (maybe-funcall pointer-button-press-handler
                                             :event event :x x :y y)))
                         (pointer-button-release-event
                          (if (and presentation presentation-button-release-handler)
                              (funcall presentation-button-release-handler
                                       :presentation presentation
                                       :event event :x x :y y)
                              (maybe-funcall pointer-button-release-handler
                                             :event event :x x :y y))))))
                    ((typep event '(or keyboard-event character symbol))
                     (maybe-funcall keyboard-handler
                                    :gesture event #|XXX|#))
                    (t (handle-event #|XXX|# (event-sheet event) event)))))))))
