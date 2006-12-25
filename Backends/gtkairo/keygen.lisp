;;; -*- Mode: Lisp; -*-

;;;  (c) 2006 David Lichteblau (david@lichteblau.com)

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

(in-package :clim-gtkairo)

#+(or)
(defparameter *key-table* (clim-gtkairo::collect-key-table))
#+(or)
(print-key-table)

(define-application-frame keygen ()
    ()
  (:panes (target :application))
  (:layouts (default target))
  (:top-level ())) 

(defmacro with-backend (name &body body)
  `(let* ((clim:*default-server-path* (list ,name))
	  (clim::*default-frame-manager*
	   (car (climi::frame-managers (find-port)))))
     ,@body))

(defun collect-key-table ()
  (with-backend :clx
    (run-frame-top-level
     (make-application-frame
      'keygen
      :top-level-lambda
      (lambda (clx)
	(with-backend :gtkairo
	  (run-frame-top-level
	   (make-application-frame
	    'keygen
	    :top-level-lambda
	    (lambda (gtk)
	      (collect-key-table-1 clx gtk))))))))))

(defun collect-key-table-1 (clx gtk)
  (let ((real-handler (fdefinition 'key-handler-impl))
	(table (make-hash-table)))
    (unwind-protect
	(progn
	  (setf (fdefinition 'key-handler-impl)
		(lambda (widget event)
		  (cffi:with-foreign-slots
		      ((type time state keyval) event gdkeventkey)
		    (setf (gethash time table)
			  (list (state-without-buttons state) keyval)))
		  (funcall real-handler widget event)))
	  (collect-key-table-2 clx gtk table))
      (setf (fdefinition 'key-handler-impl) real-handler))))

(defmacro do-modifiers ((var) &rest body)
  `(dolist (.shift. '(0 1))
     (dolist (.meta. '(0 4))
       (dolist (.control. '(0 8))
	 (let ((,var (logior .shift. .meta. .control.)))
	   ,@body)))))

(defun collect-key-table-2 (clx gtk native-events)
  (let* ((clx-target (find-pane-named clx 'target))
	 (clx-win (clim:sheet-mirror clx-target))
	 (dpy (xlib:window-display clx-win))
	 (screen (xlib:display-default-screen dpy))
	 (min (xlib:display-min-keycode dpy))
	 (max (xlib:display-max-keycode dpy))
	 (gtk-target (find-pane-named gtk 'target))
	 (gtk-win
	  (xlib::lookup-window dpy
			       (gdk_x11_drawable_get_xid
				(cffi:foreign-slot-value
				 (mirror-widget
				  (clim:sheet-mirror gtk-target))
				 'gtkwidget
				 'gdkwindow))))
	 (time 0)
	 (clx-events (make-hash-table))
	 (gtk-events (make-hash-table)))
    (format t "Waiting for windows to come up...~%")
    (sleep 5)
    (do-modifiers (state)
      (format t "Sending events for state ~D...~%" state)
      (loop for code from min to max do
	    (dolist (type '(:key-press :key-release))
	      (send-key-event screen clx-win state type code :time time)
	      (send-key-event screen gtk-win state type code :time time)
	      (slurp-events clx-target clx-events t)
	      (slurp-events gtk-target gtk-events)
	      (incf time))))
    (format t "Waiting for events to come in...~%")
    (sleep 5)
    (slurp-events clx-target clx-events)
    (slurp-events gtk-target gtk-events)
    (format t "Done.~%")
    (let ((result (make-array time))
	  (real-failures 0)
	  (mod-failures 0)
	  (misses 0))
      (dotimes (x time)
	(let* ((e (gethash x clx-events))
	       (f (gethash x gtk-events))
	       (a (de e))
	       (b (de f)))
	  (cond
	    ((null f)
	      (incf misses))
	    ((equal a b)
	      ;; (format t "PASS ~A~%" a)
	      )
	    ((equal (cdr a) (cdr b))
	      (format t "FAIL ~A/~A~%" a b)
	      (incf mod-failures))
	    (t
	      (format t "FAIL ~A/~A~%" a b)
	      (incf real-failures)))
	  (setf (elt result x) (cons e (gethash x native-events)))))
      (format t "~D failures, ~D modifier failures, ~D misses~%"
	      real-failures
	      mod-failures
	      misses)
      result)))

(defun slurp-events (target table &optional block)
  (loop
      for e = (slurp-key-event target block)
      while e
      do
	(setf (gethash (event-timestamp e) table) e)
	(setf block nil)))

(defun de (ev)
  (if ev
      (list (event-modifier-state ev)
	    (keyboard-event-key-name ev)
	    (keyboard-event-character ev))
      nil))

(defun send-key-event (screen win state type code &key time)
  (xlib:send-event win
		   type (list type)
		   :code code
		   :state state
		   :window win
		   :root (xlib:screen-root screen)
		   :time (or time 0)
		   :x 1
		   :y 2
		   :root-x 10
		   :root-y 20
		   :same-screen-p t))

(defun slurp-key-event (pane &optional block)
  (loop
      for event = (if block
		      (event-read pane)
		      (event-read-no-hang pane))
      until (typep event '(or null key-press-event key-release-event))
      finally (return event)))

(defun cwd ()
  (slot-value (asdf:find-system :clim-gtkairo) 'asdf::relative-pathname))

(defun print-key-table ()
  (let ((table (make-hash-table)))
    (loop
	for (ok state value) across *key-table*
	do
	  (when value
	    (let* ((name (if ok (keyboard-event-key-name ok) 'throw-away))
		   (char (if ok (keyboard-event-character ok) 'throw-away))
		   (def (gethash value table)))
	      (dolist (clause def
			(push (list (list state) name char)
			      (gethash value table)))
		(when (and (eql (second clause) name)
			   (eql (third clause) char))
		  (pushnew state (car clause))
		  (return))))))
    (with-open-file (s (merge-pathnames "Backends/gtkairo/keys.lisp" (cwd))
		     :direction :output
		     :if-exists :rename-and-delete)
      (write-line ";; autogenerated by keygen.lisp" s)
      (print '(in-package :clim-gtkairo) s)
      (loop
	  for (value . spec)
	      :in (sort (loop
			    for value being each hash-key in table
			    using (hash-value spec)
			    collect (cons value spec))
			#'<
			:key #'car)
	  do
	    (print `(define-key ,value ,@(simplify-spec spec)) s)))))

(defun simplify-spec (clauses)
  (flet ((count-keys (x) (length (car x))))
    (let* ((max (reduce #'max clauses :key #'count-keys))
	   (clause (find max clauses :key #'count-keys)))
      (append (remove clause clauses) `((t ,@(cdr clause)))))))
