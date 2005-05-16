
;;; Simple user-configuration editor

(in-package :cl-user)

;;; Package definition

(defpackage :profile
  (:use :clim :clim-lisp)
  (:export #:profile
	   #:define-profile-variable
	   #:profile-setq))

(in-package :profile)

(defparameter *variable-classes*
  (list :important :ucl :network :file :mouse :compile :display :common :eval :input :zmacs :mail :gc :error))

(defparameter *profile-proc-num* 0)

(define-application-frame profile ()
  ()
  (:panes (app :application
	       :display-time t
	       :scroll-bars t
	       :height 300 :width 800
	       :end-of-line-action :allow)
	  ;; Should work out how command-menu-pane type works!! Not now...
	  #+nil
	  (actions :command-menu
		   :contents (list (make-instance 'push-button
						  :label "Store Options"
						  :activate-callback
						  #'(lambda (gadget)
						      (declare (ignore gadget))
						      (com-store-options)))
				   (make-instance 'push-button
						  :label "Restore System Defaults"
						  :activate-callback
						  #'(lambda (gadget)
						      (declare (ignore gadget))
						      (com-restore-system-defaults)))
				   (make-instance 'push-button
						  :label "Restore User Defaults"
						  :activate-callback
						  #'(lambda (gadget)
						      (declare (ignore gadget))
						      (com-restore-user-defaults)))
				   (make-instance 'push-button
						  :label "Exit"
						  :activate-callback
						  #'(lambda (gadget)
						      (declare (ignore gadget))
						      (com-exit)))))
	  ;; Should work out how command-menu-pane types work!
	  #+nil
	  (variables :command-menu
		     :contents (list (make-instance 'push-button
						    :label "Important Variables"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-important-variables)))
				     (make-instance 'push-button
						    :label "UCL Variables"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-ucl-variables)))
				     (make-instance 'push-button
						    :label "Network Variables"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-network-variables)))
				     (make-instance 'push-button
						    :label "File System Variables"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-file-system-variables)))
				     (make-instance 'push-button
						    :label "Mouse Variables"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-mouse-variables)))
				     (make-instance 'push-button
						    :label "Compiler Variables"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-compiler-variables)))
				     (make-instance 'push-button
						    :label "Display Variables"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-display-variables)))
				     (make-instance 'push-button
						    :label "Common Lisp Globals"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-common-lisp-globals)))
				     (make-instance 'push-button
						    :label "Evaluation Variables"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-evaluation-variables)))
				     (make-instance 'push-button
						    :label "Input Variables"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-input-variables)))
				     (make-instance 'push-button
						    :label "Zmacs Variables"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-zmacs-variables)))
				     (make-instance 'push-button
						    :label "Mail Variables"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-mail-variables)))
				     (make-instance 'push-button
						    :label "GC Variables"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-gc-variables)))
				     (make-instance 'push-button
						    :label "Error Handling Variables"
						    :activate-callback
						    #'(lambda (gadget)
							(declare (ignore gadget))
							(com-show-error-handling-variables)))))
	  (doc :pointer-documentation))
  (:layouts (default
	      (vertically ()
			  app
			  (labelling (:label "Actions"
					     :text-style (make-text-style :sans-serif :italic :very-small))
				     (horizontally ()
						   (make-pane 'push-button
							      :label "Store Options"
							      :activate-callback
							      #'(lambda (gadget)
								  (declare (ignore gadget))
								  (com-store-options)))
						   (make-pane 'push-button
							      :label "Restore System Defaults"
							      :activate-callback
							      #'(lambda (gadget)
								  (declare (ignore gadget))
								  (com-restore-system-defaults)))
						   (make-pane 'push-button
							      :label "Restore User Defaults"
							      :activate-callback
							      #'(lambda (gadget)
								  (declare (ignore gadget))
								  (com-restore-user-defaults)))
						   (make-pane 'push-button
							      :label "Exit"
							      :activate-callback
							      #'(lambda (gadget)
								  (declare (ignore gadget))
								  (com-exit)))))
			  ;; There *must* be a nicer way to display this; suspect either 'radio-box' panes,
			  ;; or 'command-menu' pane.
			  (labelling (:label "Variables currently displayed:"
					     :text-style (make-text-style :sans-serif :italic :very-small))
				     ;; maybe 'grid-pane'?
				     (tabling ()
					      (list
;;; Could make these radio boxes, but not sure can table those; ideally _all_ these toggle
;;; buttons would be part of the same radio box.
;;; Looks like McCLIM doesn't accept the specified arguments to :value-changed-callback; only gadget + value
;;;					       (make-pane 'radio-box
;;;							  :orientation :horizontal
;;;							  :contents (list
								     (make-pane 'toggle-button
										:label "Important Variables"
										:value nil
										:value-changed-callback
;;;										#'(lambda (gadget client gadget-id value)
;;;										    (declare (ignore gadget client gadget-id))
										#'(lambda (gadget value)
										    (declare (ignore gadget))
										    (when value
										      (com-show-important-variables))))
								     (make-pane 'toggle-button
										:label "UCL Variables"
										:value nil
										:value-changed-callback
;;;										#'(lambda (gadget client gadget-id value)
;;;										    (declare (ignore gadget client gadget-id))
										#'(lambda (gadget value)
										    (declare (ignore gadget))
										    (when value
										      (com-show-ucl-variables))))
								     (make-pane 'toggle-button
										:label "Error Handling Variables"
										:value nil
										:value-changed-callback
;;;										#'(lambda (gadget client gadget-id value)
;;;										    (declare (ignore gadget client gadget-id))
										#'(lambda (gadget value)
										    (declare (ignore gadget))
										    (when value
										      (com-show-error-handling-variables))))
								     (make-pane 'toggle-button
										:label "Network Variables"
										:value nil
										:value-changed-callback
;;;										#'(lambda (gadget client gadget-id value)
;;;										    (declare (ignore gadget client gadget-id))
										#'(lambda (gadget value)
										    (declare (ignore gadget))
										    (when value
										      (com-show-network-variables))))
								     (make-pane 'toggle-button
										:label "GC Variables"
										:value nil
										:value-changed-callback
;;;										#'(lambda (gadget client gadget-id value)
;;;										    (declare (ignore gadget client gadget-id))
										#'(lambda (gadget value)
										    (declare (ignore gadget))
										    (when value
										      (com-show-gc-variables)))))  ;))
					      (list
					       (make-pane 'toggle-button
							  :label "Compiler Variables"
							  :value nil
							  :value-changed-callback
;;;							  #'(lambda (gadget client gadget-id value)
;;;							      (declare (ignore gadget client gadget-id))
							  #'(lambda (gadget value)
							      (declare (ignore gadget))
							      (when value
								(com-show-compiler-variables))))
					       (make-pane 'toggle-button
							  :label "Mail Variables"
							  :value nil
							  :value-changed-callback
;;;							  #'(lambda (gadget client gadget-id value)
;;;							      (declare (ignore gadget client gadget-id))
							  #'(lambda (gadget value)
							      (declare (ignore gadget))
							      (when value
								(com-show-mail-variables))))
					       (make-pane 'toggle-button
							  :label "Zmacs Variables"
							  :value nil
							  :value-changed-callback
;;;							  #'(lambda (gadget client gadget-id value)
;;;							      (declare (ignore gadget client gadget-id))
							  #'(lambda (gadget value)
							      (declare (ignore gadget))
							      (when value
								(com-show-zmacs-variables))))
					       (make-pane 'toggle-button
							  :label "Mouse Variables"
							  :value nil
							  :value-changed-callback
;;;							  #'(lambda (gadget client gadget-id value)
;;;							      (declare (ignore gadget client gadget-id))
							  #'(lambda (gadget value)
							      (declare (ignore gadget))
							      (when value
								(com-show-mouse-variables))))
					       (make-pane 'toggle-button
							  :label "Input Variables"
							  :value nil
							  :value-changed-callback
;;;							  #'(lambda (gadget client gadget-id value)
;;;							      (declare (ignore gadget client gadget-id))
							  #'(lambda (gadget value)
							      (declare (ignore gadget))
							      (when value
								(com-show-input-variables)))))
					      (list
					       (make-pane 'toggle-button
							  :label "File System Variables"
							  :value nil
							  :value-changed-callback
;;;							  #'(lambda (gadget client gadget-id value)
;;;							      (declare (ignore gadget client gadget-id))
							  #'(lambda (gadget value)
							      (declare (ignore gadget))
							      (when value
								(com-show-file-system-variables))))
					       (make-pane 'toggle-button
							  :label "Evaluation Variables"
							  :value nil
							  :value-changed-callback
;;;							  #'(lambda (gadget client gadget-id value)
;;;							      (declare (ignore gadget client gadget-id))
							  #'(lambda (gadget value)
							      (declare (ignore gadget))
							      (when value
								(com-show-evaluation-variables))))
					       (make-pane 'toggle-button
							  :label "Common Lisp Globals"
							  :value nil
							  :value-changed-callback
;;;							  #'(lambda (gadget client gadget-id value)
;;;							      (declare (ignore gadget client gadget-id))
							  #'(lambda (gadget value)
							      (declare (ignore gadget))
							      (when value
								(com-show-common-lisp-globals))))
					       (make-pane 'toggle-button
							  :label "Display Variables"
							  :value nil
							  :value-changed-callback
;;;							  #'(lambda (gadget client gadget-id value)
;;;							      (declare (ignore gadget client gadget-id))
							  #'(lambda (gadget value)
							      (declare (ignore gadget))
							      (when value
								(com-show-display-variables))))
					       ;; looks like table-panes (in McCLIM at least) must have
					       ;; the same number of columns in each row; stick an
					       ;; empty do-nothing pane in.
					       (make-clim-stream-pane :scroll-bars nil
								      :borders nil))))
			  doc))))

(define-profile-command (com-exit :name t) ()
  (frame-exit *application-frame*))

(defun profile (&key (new-process t)
		     (process-name nil))
  (declare (special *profile-proc-num*))
  (when (null process-name)
    (setf *profile-proc-num* (incf *profile-proc-num*))
    (setf process-name (concatenate 'string "Profile " (format nil "~a" *profile-proc-num*))))

  (flet ((run ()
	      (run-frame-top-level (make-application-frame 'profile :pretty-name process-name))))
    (if new-process
	(clim-sys:make-process #'run :name process-name)
      (run))))