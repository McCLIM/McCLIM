;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

(in-package :CLIM-INTERNALS)

(defvar *application-frame* nil)
(defvar *default-frame-manager* nil)

;;; Frame-Manager class

(defclass frame-manager ()
  ((port :initarg :port
	 :reader frame-manager-port)
   (frames :initform nil
	   :reader frame-manager-frames)
   )
  )

(defun frame-manager-p (x)
  (typep x 'frame-manager))

(defun find-frame-manager (&rest options &key port &allow-other-keys)
  (declare (special *frame-manager*))
  (if (boundp '*frame-manager*)
      *frame-manager*
    (if (and *default-frame-manager*
	     (frame-manager-p *default-frame-manager*))
	*default-frame-manager*
      (first (frame-managers (or port (apply #'find-port options)))))))

(defmacro with-frame-manager ((frame-manager) &body body)
  `(let (('*frame-manager* ,frame-manager))
     (declare (special *frame-manager*))
     (block ,@body)))
      
;;; Application-Frame class

(defclass application-frame ()
  ((port :initform nil
	 :initarg :port
	 :accessor port)
   (graft :initform nil
	  :initarg :graft
	  :accessor graft)
   (name :initarg :name
	 :reader frame-name)
   (pretty-name :initarg :pretty-name
		:accessor frame-pretty-name)
   (command-table :initarg :command-table
		  :initform nil
		  :accessor frame-command-table)
   (disabled-commands :initarg :disabled-commands
		      :initform nil
		      :accessor frame-disabled-commands)
   (pane :reader frame-pane)
   (panes :initform nil
	  :reader frame-panes)
   (layouts :initform nil
	    :initarg :layouts
	    :reader frame-layouts)
   (current-layout :initform nil
		   :initarg :current-layout
		   :reader frame-current-layout)
   (top-level-sheet :initform nil
		    :reader frame-top-level-sheet)
   (menu-bar :initarg :menu-bar
	     :initform nil)
   (calling-frame :initarg :calling-frame
		  :initform nil)
   (state :initarg :state
	  :initform nil
	  :accessor frame-state)
   (manager :initform nil
	    :reader frame-manager)
   (properties :initarg :properties
	       :initform nil)
   (top-level :initform '(default-frame-top-level)
	      :initarg :top-level
	      :reader frame-top-level)
   ))

(defun application-frame-p (x)
  (typep x 'application-frame))

(defmethod initialize-instance :after ((frame application-frame) &rest args)
  (declare (ignore args))
  )

(defclass standard-application-frame (application-frame)
  ())

(defmethod (setf frame-manager) (fm (frame application-frame))
  (let ((old-manager (frame-manager frame)))
    (setf (slot-value frame 'manager) nil)
    (when old-manager
      (disown-frame old-manager frame)
      (setf (slot-value frame 'panes) nil)
      (setf (slot-value frame 'layouts) nil))
    (setf (slot-value frame 'manager) fm)))

(defmethod (setf frame-current-layout) (name (frame application-frame))
  (declare (ignore name))
  (generate-panes (frame-manager frame) frame))

(defmethod generate-panes :before (fm  (frame application-frame))
  (declare (ignore fm))
  (when (and (slot-boundp frame 'pane)
	     (frame-pane frame))
    (sheet-disown-child (frame-top-level-sheet frame) (frame-pane frame))))

(defmethod generate-panes :after (fm  (frame application-frame))
  (declare (ignore fm))
  (sheet-adopt-child (frame-top-level-sheet frame) (frame-pane frame))
  (sheet-adopt-child (graft frame) (frame-top-level-sheet frame))
  (setf (sheet-transformation (frame-top-level-sheet frame))
	(make-translation-transformation 100 100))
  (let ((space (compose-space (frame-top-level-sheet frame))))
    ;; automatically generates a window-configuation-event
    ;; which then calls allocate-space
    (setf (sheet-region (frame-top-level-sheet frame))
	  (make-bounding-rectangle 0 0
				   (space-requirement-width space)
				   (space-requirement-height space)))))

(defmethod find-pane-named ((frame application-frame) name)
  (loop for pane in (frame-panes frame)
      if (eq (pane-name pane) name)
      return pane))

(defmethod layout-frame ((frame application-frame) &optional width height)
  (let ((pane (frame-pane frame)))
    (if (and  width (not height))
	(error "LAYOUT-FRAME must be called with both WIDTH and HEIGHT or neither"))
    (if (and (null width) (null height))
	(let ((space (compose-space pane)))
	  (setq width (space-requirement-width space))
	  (setq height (space-requirement-height space))))
    (allocate-space pane width height)))

(defun find-pane-of-type (panes type)
  (setq panes (copy-list panes))
  (loop for pane in panes
	if (typep pane type)
	   return pane
	do (setq panes (nconc panes (copy-list (sheet-children pane))))
	finally (return nil)))
	      
(defmethod frame-standard-output ((frame application-frame))
  (or (find-pane-of-type (frame-panes frame) 'application-pane)
      (find-pane-of-type (frame-panes frame) 'interactor-pane)))

(defmethod frame-standard-input ((frame application-frame))
  (or (find-pane-of-type (frame-panes frame) 'interactor-pane)
      (frame-standard-output frame)))

(defmethod frame-query-io ((frame application-frame))
  (or (frame-standard-input frame)
      (frame-standard-output frame)))

(defmethod frame-error-output ((frame application-frame))
  (frame-standard-output frame))

(defvar *pointer-documentation-output* nil)

(defmethod frame-pointer-documentation-output ((frame application-frame))
  (loop for pane in (frame-panes frame)
      if (typep pane 'pointer-documentation-pane)
      return pane
      finally (return nil)))

;;; Command loop interface

(defmethod run-frame-top-level ((frame application-frame))
  (apply (first (frame-top-level frame)) frame (rest (frame-top-level frame))))

(defmethod run-frame-top-level :around ((frame application-frame))
  (let ((*application-frame* frame)
	(*input-context* nil)
	(*input-wait-test* nil)
	(*input-wait-handler* nil)
	(*pointer-button-press-handler* nil))
    (declare (special *input-context* *input-wait-test* *input-wait-handler*
		      *pointer-button-press-handler*))
    (call-next-method)))

(defmethod default-frame-top-level
    ((frame application-frame)
     &key (command-parser 'command-line-command-parser)
	  (command-unparser 'command-line-command-unparser)
	  (partial-command-parser
	   'command-line-read-remaining-arguments-for-partial-command)
	  (prompt "Command: "))
  (let ((*standard-input* (frame-standard-input frame))
	(*standard-output* (frame-standard-output frame))
	(*query-io* (frame-query-io frame))
	;; during development, don't alter *error-output*
	;(*error-output* (frame-error-output frame))
	(*command-parser* command-parser)
	(*command-unparser* command-unparser)
	(*partial-command-parser* partial-command-parser)
	(prompt-style (make-text-style :fixed :italic :normal))
	results)
    (setf (cursor-visibility (stream-text-cursor *standard-input*)) t)
    (loop do
	  (with-text-style (*standard-input* prompt-style)
	    (if (stringp prompt)
		(stream-write-string *standard-input* prompt)
	      (apply prompt (list *standard-input* frame))))
	  (setq results (multiple-value-list (execute-frame-command frame (read-frame-command frame *standard-input*))))
	  (loop for result in results
		do (print result *standard-input*))
	  (terpri *standard-input*))
    ))

(defmethod read-frame-command ((frame application-frame) stream)
  (read-command (frame-command-table frame) :stream stream))

(defmethod execute-frame-command ((frame application-frame) command)
  (apply (command-name command) (command-arguments command)))

(defmethod make-pane-1 ((fm frame-manager) (frame application-frame) type &rest args)
  `(make-pane-1 ,fm ,frame ',type ,@args))

(defmethod adopt-frame ((fm frame-manager) (frame application-frame))
  (setf (slot-value fm 'frames) (cons frame (slot-value fm 'frames)))
  (setf (frame-manager frame) fm)
  (let* ((*application-frame* frame)
	 (t-l-s (make-pane-1 fm frame 'top-level-sheet-pane
			     :name 'top-level-sheet)))
    (setf (slot-value frame 'top-level-sheet) t-l-s)
    (generate-panes fm frame)))
  
(defmethod disown-frame ((fm frame-manager) (frame application-frame))
  (setf (slot-value fm 'frames) (remove frame (slot-value fm 'frames)))
  (sheet-disown-child (graft frame) (frame-top-level-sheet frame))
  (setf (frame-manager frame) nil))

(defmacro with-look-and-feel-realization ((frame-manager frame) &body body)
  (declare (ignore frame-manager frame))
  `(progn
     ,@body))

(defun make-single-pane-generate-panes-form (class-name pane)
  `(defmethod generate-panes ((fm frame-manager) (frame ,class-name))
     (let ((*application-frame* frame))
       (let ((pane ,pane))
	 (setf (slot-value frame 'pane) pane)))))

(defun make-panes-generate-panes-form (class-name panes layouts)
  `(defmethod generate-panes ((fm frame-manager) (frame ,class-name))
     (let ((*application-frame* frame))
       (let ,(loop for (name . form) in panes
		   collect `(,name (or (find-pane-named frame ',name)
				       (let ((pane
					      ,(cond
						((and (= (length form) 1)
						      (listp (first form)))
						 (first form))
						((keywordp (first form))
						 (let ((maker (intern (concatenate 'string
									"MAKE-CLIM-"
									(symbol-name (first form))
									"-PANE") :clim)))
						   (if (fboundp maker)
						       `(,maker :name ',name ,@(cdr form))
						     `(make-pane ',(intern (concatenate 'string
									     (symbol-name (first form))
									     "-PANE")
									   :clim)
								 :name ',name ,@(cdr form)))))
						(t `(make-pane ',(first form) :name ',name ,@(cdr form))))))
					 (push pane (slot-value frame 'panes))
					 pane))))
	 (setf (slot-value frame 'pane)
	   (with-look-and-feel-realization (fm frame)
	     (ecase (frame-current-layout frame)
	       ,@layouts)))
	 ))))

(defmacro define-application-frame (name superclasses slots &rest options)
  (if (null superclasses)
      (setq superclasses '(standard-application-frame)))
  (let ((pane nil)
	(panes nil)
	(layouts nil)
	(current-layout nil)
	(command-table nil)
	(menu-bar t)
	(disabled-commands nil)
	(command-definer t)
	(top-level '(default-frame-top-level))
	(others nil)
	(command-name (intern (concatenate 'string "DEFINE-" (symbol-name name) "-COMMAND"))))
    (loop for (prop . values) in options
	do (case prop
	     (:pane (setq pane (first values)))
	     (:panes (setq panes values))
	     (:layouts (setq layouts values))
	     (:command-table (setq command-table (first values)))
	     (:menu-bar (setq menu-bar (first values)))
	     (:disabled-commands (setq disabled-commands values))
	     (:command-definer (setq command-definer (first values)))
	     (:top-level (setq top-level (first values)))
	     (t (push (cons prop values) others))))
    (if (or (and pane panes)
	    (and pane layouts))
	(error ":pane cannot be specified along with either :panes or :layouts"))
    (if pane
	(setq panes (list 'single-pane pane)
	      layouts (list :default (first pane))))
    (setq current-layout (first (first layouts)))
    `(progn
       (defclass ,name ,superclasses
	 ,slots
	 (:default-initargs
	   :name ',name
	   :pretty-name ,(string-capitalize name)
	   :command-table ,command-table
	   :disabled-commands ',disabled-commands
	   :menu-bar ,menu-bar
	   :current-layout ',current-layout
	   :layouts ',layouts
	   :top-level ',top-level
	     )
	 ,@others)
       ,(if pane
	    (make-single-pane-generate-panes-form name pane)
	  (make-panes-generate-panes-form name panes layouts))
       ,@(if command-definer
	     `((defmacro ,command-name (name-and-options arguements &rest body)
		 (let ((name (if (listp name-and-options) (first name-and-options) name-and-options))
		       (options (if (listp name-and-options) (cdr name-and-options) nil)))
		   `(define-command ,name ,arguements ,@body))))))))

(defun make-application-frame (frame-name
			       &rest options
			       &key pretty-name frame-manager enable state
				    left top right bottom width height save-under
				    frame-class
			       &allow-other-keys)
  (setq options (loop for (key value) on options by #'cddr
		    if (not (member key '(:pretty-name :frame-manager :enable :state
					  :left :top :right :bottom :width :height :save-under
					  :frame-class)
				    :key #'eq))
		       nconc (list key value)))
  (if (null frame-class)
      (setq frame-class frame-name))
  (if (null pretty-name)
      (setq pretty-name (string-capitalize frame-name)))
  (if (null frame-manager)
      (setq frame-manager (find-frame-manager)))
  (let ((frame (apply #'make-instance frame-class
		      :port (frame-manager-port frame-manager)
		      :graft (find-graft :port (frame-manager-port frame-manager))
		      :name frame-name :pretty-name pretty-name options)))
    (adopt-frame frame-manager frame)
    frame))

