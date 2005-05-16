
(in-package :cl-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Package definition
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :glimpse
  (:use :clim :clim-lisp)
  (:export #:glimpse
	   #:*tree-output-type*
	   #:*full-report*))

(in-package :glimpse)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parameters used within Glimpse; the user might like to modify these.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *glimpse-proc-num* 0
"Indicates how many Glimpse frames have been constructed during the Lisp
session; used to generate FRAME-PRETTYNAME.")

;;; Fixme: test for -ve values?
(defparameter *tree-output-indent* 0
"Size of the left margin when window mode displays windows as an indented
list. Default is 0 (no margin).")

;;; Fixme: test for -ve values?
(defparameter *tree-indent-increment* 4
"Size that each 'generation' should be indented relative to the previous
generation when window mode displays windows as an indented list. Default
is 4.")

;;; ::FIXME:: if :graph is specified here, we have problems because the graph
;;;           doesn't fit on the pane, but the scroll bar updates and pane
;;; resizing don't happen. At least when we output text, the pane is sized so
;;; it can be scrolled. Have implemented work-around pointed out by Paolo for
;;; this.

;;; Fixme: test that value is (or :text :graph)?
(defparameter *tree-output-type* :text
"The style of display used when in window mode. This value can be either
:text or :graph. When :text, the values specified in
glimpse:*tree-output-indent* (for the left margin) and
glimpse:*tree-indent-increment* (for generational indent) affect output.
The default is :text.")

(defparameter *full-report* nil
"Most objects displayed in the glimpse 'detail' frame have many more slots
than indicated there (by default). By setting this parameter to T, objects
will be fully described. If set to NIL, objects will only be partially
described. The default is NIL.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define application frame (will become the CLOS class for the application)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-application-frame glimpse ()
  ()
  (:panes (app :application
	       :display-time t
	       :scroll-bars t
	       :height 400 :width 800
	       :end-of-line-action :allow)
	  (doc :pointer-documentation)
	  (out :application
	       :display-time t
	       :scroll-bars t
	       :end-of-page-action :scroll
	       :height 100 :width 800))
  (:layouts (default
	      (vertically ()
			  ;; I'd like to see labelling panes that *don't* force you to have
			  ;; a border (other than some space for the label depending on
			  ;; label position). Ah well.
			  (labelling (:label "Mode Data"
				      :text-style (make-text-style :sans-serif :italic :very-small))
				     app)
			  (labelling (:label "Detail"
				      :text-style (make-text-style :sans-serif :italic :very-small))
				     out)
			  (horizontally ()
					(labelling (:label "Modes"
						    :text-style (make-text-style :sans-serif :italic :very-small))
						   (horizontally ()  ; different modes
								 (make-pane 'push-button
									    :label "Windows"
									    :activate-callback
									    #'(lambda (gadget)
										(declare (ignore gadget))
										(com-show-sheet-hierarchy)))
								 (make-pane 'push-button
									    :label "Processes"
									    :activate-callback
									    #'(lambda (gadget)
										(declare (ignore gadget))
										(com-show-processes)))))
					(labelling (:label "Commands"
						    :text-style (make-text-style :sans-serif :italic :very-small))
						   (horizontally ()
								 (make-pane 'push-button
									    :label "Clear Detail"
									    :activate-callback
									    #'(lambda (gadget)
										(declare (ignore gadget))
										(com-clear-detail-window)))
								 ;; Only want to do this when showing windows!
								 ;; Also can this be disabled initially?
								 (make-pane 'push-button
									    :name 'toggle-output
									    :label "Toggle Output Style"
									    :activate-callback
									    #'(lambda (gadget)
										(declare (ignore gadget))
										(com-toggle-output-style)))
								 (make-pane 'push-button
									    :label "Quit"
									    :activate-callback
									    #'(lambda (gadget)
										(declare (ignore gadget))
										(com-quit))))))
			  doc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Application entry point; run the frame top level.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun glimpse (&key (new-process t)
		     (process-name nil))
"Executes the Glimpse utility.

Glimpse can run either as a stand-alone process, or within the
current process depending on the value of the keyword parameter
'new-process'. This value defaults to T (run as separate process).

The name of the process that Glimpse runs as can be set explicitly
on invocation using the 'process-name' keyword parameter. This
value defaults to NIL.
If the process name is NIL (the default) the process will be named
'Glimpse N' where N is a count of the number of glimpse frames
created during the Lisp session."
  (declare (special *glimpse-proc-num*))
  (when (null process-name)
    (setf *glimpse-proc-num* (incf *glimpse-proc-num*))
    (setf process-name (concatenate 'string "Glimpse " (format nil "~a" *glimpse-proc-num*))))
  
  (flet ((run ()
	      (run-frame-top-level (make-application-frame 'glimpse :pretty-name process-name))))
    (if new-process
	(clim-sys:make-process #'run :name process-name)
      (run))))

