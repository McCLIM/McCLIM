(in-package :common-lisp-user)

(defpackage "APP"
  (:use :clim :clim-lisp)
  (:export "APP-MAIN"))

(in-package :app)

(define-application-frame superapp ()
  ((numbers :initform (loop repeat 20 collect (list (random 100000000)))
	    :accessor numbers)
   (cursor :initform 0 :accessor cursor))
  (:pointer-documentation t)
  (:panes
    (app :application
	 :height 400 :width 600
	 :incremental-redisplay t
	 :display-function 'display-app)
    (int :interactor :height 200 :width 600))
  (:layouts
    (default (vertically () app int))))

(defun display-app (frame pane)
  (loop for element in (numbers frame)
	for line from 0
	do (princ (if (= (cursor frame) line) "*" " ") pane)
	do (updating-output (pane :unique-id element
				  :id-test #'eq
				  :cache-value (car element)
				  :cache-test #'eql)
	     (format pane "~a~%" (car element)))))

(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))

(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-superapp-command (com-add :name t) ((number 'integer))
  (incf (car (elt (numbers *application-frame*)
		  (cursor *application-frame*)))
	number))

(define-superapp-command (com-next :name t) ()
  (incf (cursor *application-frame*))
  (when (= (cursor *application-frame*)
	   (length (numbers *application-frame*)))
    (setf (cursor *application-frame*) 0)))

(define-superapp-command (com-prev :name t) ()
  (decf (cursor *application-frame*))
  (when (minusp (cursor *application-frame*))
    (setf (cursor *application-frame*)
	  (1- (length (numbers *application-frame*))))))



