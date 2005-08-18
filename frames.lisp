;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2004 by
;;;           Gilbert Baumann <unk6@rz.uni-karlsruhe.de>

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

(in-package :clim-internals)

;; *application-frame* is in decls.lisp
(defvar *default-frame-manager* nil)

;;; Frame-Manager class

(define-protocol-class frame-manager ()
  ((port :initarg :port
	 :reader frame-manager-port)
   (frames :initform nil
	   :reader frame-manager-frames)))

(defun find-frame-manager (&rest options &key port &allow-other-keys)
  (declare (special *frame-manager*))
  (if (boundp '*frame-manager*)
      *frame-manager*
    (if (and *default-frame-manager*
	     (frame-manager-p *default-frame-manager*))
	*default-frame-manager*
      (first (frame-managers (or port (apply #'find-port options)))))))

(defmacro with-frame-manager ((frame-manager) &body body)
  `(let ((*frame-manager* ,frame-manager))
     (declare (special *frame-manager*))
     (locally ,@body)))

;;; XXX These should force the redisplay of the menu bar. They don't
;;; yet.

(defmethod note-command-enabled (frame-manager frame command-name)
  (declare (ignore frame-manager frame command-name))
  nil)

(defmethod note-command-disabled (frame-manager frame command-name)
  (declare (ignore frame-manager frame command-name))
  nil)

;;; Application-Frame class
;;; XXX All these slots should move to a mixin or to standard-application-frame.
;;; -- moore

;;; Generic operations
(defgeneric frame-name (frame))
(defgeneric frame-pretty-name (frame))
(defgeneric (setf frame-pretty-name) (name frame))
(defgeneric frame-command-table (frame))
(defgeneric (setf frame-command-table) (command-table frame))
(defgeneric frame-standard-output (frame)
  (:documentation
   "Returns the stream that will be used for *standard-output* for the FRAME."))
(defgeneric frame-standard-input (frame)
  (:documentation
   "Returns the stream that will be used for *standard-input* for the FRAME."))
(defgeneric frame-query-io (frame)
  (:documentation
   "Returns the stream that will be used for *query-io* for the FRAME."))
(defgeneric frame-error-output (frame)
  (:documentation
   "Returns the stream that will be used for *error-output* for the FRAME."))
(defgeneric frame-pointer-documentation-output (frame)
  (:documentation
   "Returns the stream that will be used for *pointer-documentation-output*
for the FRAME."))
(defgeneric frame-calling-frame (frame)
  (:documentation
   "Returns the application frame that invoked the FRAME."))
(defgeneric frame-parent (frame)
  (:documentation
   "Returns the object that acts as the parent for the FRAME."))
(defgeneric frame-panes (frame)
  (:documentation
   "Returns the pane that is the top-level pane in the current layout
of the FRAME's named panes."))
(defgeneric frame-top-level-sheet (frame)
  (:documentation
   "Returns the shhet that is the top-level sheet for the FRAME. This
is the sheet that has as its descendants all of the panes of the FRAME."))
(defgeneric frame-current-panes (frame)
  (:documentation
   "Returns a list of those named panes in the FRAME's current layout.
If there are no named panes, only the single, top level pane is returned."))
(defgeneric get-frame-pane (frame pane-name)
  (:documentation
   "Returns the named CLIM stream pane in the FRAME whose name is PANE-NAME."))
(defgeneric find-pane-named (frame pane-name)
  (:documentation
   "Returns the pane in the FRAME whose name is PANE-NAME."))
(defgeneric frame-current-layout (frame))
(defgeneric (setf frame-current-layout) (layout frame))
(defgeneric frame-all-layouts (frame))
(defgeneric layout-frame (frame &optional width height))
(defgeneric frame-exit-frame (condition)
  (:documentation
   "Returns the frame that is being exited from associated with the
FRAME-EXIT condition."))
(defgeneric frame-exit (frame)
  (:documentation
   "Exits from the FRAME."))
(defgeneric pane-needs-redisplay (pane))
(defgeneric (setf pane-needs-redisplay) (value pane))
(defgeneric redisplay-frame-pane (frame pane &key force-p))
(defgeneric redisplay-frame-panes (frame &key force-p))
(defgeneric frame-replay (frame stream &optional region))
(defgeneric notify-user (frame message &key associated-window title
                         documentation exit-boxes name style text-style))
(defgeneric frame-properties (frame property))
(defgeneric (setf frame-properties) (value frame property))
(defgeneric (setf client-setting) (value frame setting))
(defgeneric reset-frame (frame &rest client-settings))
(defgeneric frame-maintain-presentation-histories (frame))

; extension
(defgeneric frame-schedule-timer-event (frame sheet delay token))

(defgeneric note-input-focus-changed (pane state)
  (:documentation "Called when a pane receives or loses the keyboard
input focus. This is a McCLIM extension."))

(define-protocol-class  application-frame ()
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
   (named-panes :accessor frame-named-panes :initform nil)
   (panes :initform nil :reader frame-panes
	  :documentation "The tree of panes in the current layout.")
   (layouts :initform nil
	    :initarg :layouts
	    :reader frame-layouts)
   (current-layout :initform nil
		   :initarg :current-layout
		   :accessor frame-current-layout)
   (panes-for-layout :initform nil :accessor frame-panes-for-layout
		     :documentation "alist of names and panes (as returned by make-pane)")
   (top-level-sheet :initform nil
		    :reader frame-top-level-sheet)
   (menu-bar :initarg :menu-bar
	     :initform nil)
   (calling-frame :initarg :calling-frame
		  :initform nil)
   (state :initarg :state
	  :initform :disowned
	  :reader frame-state)
   (manager :initform nil
	    :reader frame-manager
            :accessor %frame-manager)
   (keyboard-input-focus :initform nil
                         :accessor keyboard-input-focus)
   (properties :accessor %frame-properties
	       :initarg :properties
	       :initform nil)
   (top-level :initform '(default-frame-top-level)
	      :initarg :top-level
	      :reader frame-top-level)
   (top-level-lambda :initarg :top-level-lambda
		     :reader frame-top-level-lambda)
   (hilited-presentation :initform nil
			 :initarg :hilited-presentation
			 :accessor frame-hilited-presentation)
   (user-supplied-geometry :initform nil
			   :initarg :user-supplied-geometry
                           :documentation "plist of defaulted :left, :top, :bottom, :right, :width and :height options.")
   (process :reader frame-process :initform (current-process))
   (client-settings :accessor client-settings :initform nil)))

(defmethod frame-geometry ((frame application-frame))
  (slot-value frame 'user-supplied-geometry))

(defmethod frame-geometry* ((frame application-frame))
  "-> width height &optional top left"
  (let ((pane (frame-top-level-sheet frame)))
    (destructuring-bind (&key left top right bottom width height) (frame-geometry frame)
      ;; Find width and height from looking at the respective options
      ;; first, then at left/right and top/bottom and finally at what
      ;; compose-space says.
      (setf width (or width
                      (and left right (- right left))
                      (space-requirement-width (compose-space pane))))
      (setf height (or height
                       (and top bottom (- bottom top))
                       (space-requirement-height (compose-space pane))))
      ;; See if a position is wanted and return left, top.
      (setf left (or left
                     (and right (- right width))))
      (setf top (or top
                    (and bottom (- bottom height))))
      (values width height left top))))
    
(defclass standard-application-frame (application-frame
				      presentation-history-mixin)
  ((event-queue :initarg :frame-event-queue
                :initarg :input-buffer
                :initform nil
		:accessor frame-event-queue
		:documentation "The event queue that, by default, will be
  shared by all panes in the stream")
   (documentation-state :accessor frame-documentation-state
			:initform nil
			:documentation "Used to keep of track of what
  needs to be rendered in the pointer documentation frame.")
   (calling-frame :reader frame-calling-frame
		  :initarg :calling-frame
		  :initform nil
		  :documentation "The frame that is the parent of this
frame, if any")
   (disabled-commands :accessor disabled-commands
		      :initform nil
		      :documentation "A list of command names that have been
				      disabled in this frame")))

;;; Support the :input-buffer initarg for compatibility with "real CLIM"

(defmethod initialize-instance :after ((obj standard-application-frame)
                                       &key &allow-other-keys)
  (when (and (frame-calling-frame obj)
	   (null (frame-event-queue obj)))
    (setf (frame-event-queue obj)
	  (frame-event-queue (frame-calling-frame obj))))
  (unless (frame-event-queue obj)
    (setf (frame-event-queue obj)
          (make-instance 'port-event-queue))))

(defmethod (setf frame-manager) (fm (frame application-frame))
  (let ((old-manager (frame-manager frame)))
    (setf (%frame-manager frame) nil)
    (when old-manager
      (disown-frame old-manager frame)
      (setf (slot-value frame 'panes) nil)
      (setf (slot-value frame 'layouts) nil))
    (setf (%frame-manager frame) fm)))

(define-condition frame-layout-changed (condition)
  ((frame :initarg :frame :reader frame-layout-changed-frame)))

(defmethod (setf frame-current-layout) :after (name (frame application-frame))
  (declare (ignore name))
  (when (frame-manager frame)
    (generate-panes (frame-manager frame) frame)
    (multiple-value-bind (w h) (frame-geometry* frame)
      (layout-frame frame w h))  
    (signal 'frame-layout-changed :frame frame)))

(defmethod generate-panes :before (fm  (frame application-frame))
  (declare (ignore fm))
  (when (and (frame-panes frame)
	     (eq (sheet-parent (frame-panes frame))
		 (frame-top-level-sheet frame)))
    (sheet-disown-child (frame-top-level-sheet frame) (frame-panes frame)))
  (loop
     for (nil . pane) in (frame-panes-for-layout frame)
     for parent = (sheet-parent pane)
     if  parent
     do (sheet-disown-child parent pane)))

(defmethod generate-panes :after (fm  (frame application-frame))
  (declare (ignore fm))
  (sheet-adopt-child (frame-top-level-sheet frame) (frame-panes frame))
  (unless (sheet-parent (frame-top-level-sheet frame))
    (sheet-adopt-child (graft frame) (frame-top-level-sheet frame)))
  ;; Find the size of the new frame
  (multiple-value-bind (w h x y) (frame-geometry* frame)
    ;; automatically generates a window-configuation-event
    ;; which then calls allocate-space
    ;;
    ;; Not any longer, we turn of CONFIGURE-NOTIFY events until the
    ;; window is mapped and do the space allocation now, so that all
    ;; sheets will have their correct geometry at once. --GB
    (setf (sheet-region (frame-top-level-sheet frame))
	  (make-bounding-rectangle 0 0 w h))
    (allocate-space (frame-top-level-sheet frame) w h) ))

(defmethod layout-frame ((frame application-frame) &optional width height)
  (let ((pane (frame-panes frame)))
    (if (and  width (not height))
	(error "LAYOUT-FRAME must be called with both WIDTH and HEIGHT or neither"))
    (if (and (null width) (null height))
	(let ((space (compose-space pane))) ;I guess, this might be wrong. --GB 2004-06-01
	  (setq width (space-requirement-width space))
	  (setq height (space-requirement-height space))))
    (let ((tpl-sheet (frame-top-level-sheet frame)))
      (unless (and (= width (bounding-rectangle-width tpl-sheet))
                   (= height (bounding-rectangle-height tpl-sheet)))
        (resize-sheet (frame-top-level-sheet frame) width height)))
    (allocate-space pane width height)))

(defun find-pane-if (predicate panes)
  "Returns a pane satisfying PREDICATE in the forest growing from PANES"
  (map-over-sheets #'(lambda (p)
		       (when (funcall predicate p)
			 (return-from find-pane-if p)))
		   panes)
  nil)

(defun find-pane-of-type (panes type)
  (find-pane-if #'(lambda (pane) (typep pane type)) panes))

;;; There are several ways to do this; this isn't particularly efficient, but
;;; it shouldn't matter much.  If it does, it might be better to map over the
;;; panes in frame-named-panes looking for panes with parents.
(defmethod frame-current-panes ((frame application-frame))
  (let ((panes nil)
	(named-panes (frame-named-panes frame)))
    (map-over-sheets #'(lambda (p)
			 (when (member p named-panes)
			   (push p panes)))
		     (frame-panes frame))
    panes))

(defmethod get-frame-pane ((frame application-frame) pane-name)
  (let ((pane (find-pane-named frame pane-name)))
    (if (typep pane 'clim-stream-pane)
	pane
	nil)))

(defmethod find-pane-named ((frame application-frame) pane-name)
  (find pane-name (frame-named-panes frame) :key #'pane-name))

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
  (find-pane-of-type (frame-panes frame) 'pointer-documentation-pane))

#+nil
(defmethod redisplay-frame-panes ((frame application-frame) &key force-p)
  (map-over-sheets
   (lambda (sheet)
     (when (typep sheet 'pane)
       (when (and (typep sheet 'clim-stream-pane)
                  (not (eq :no-clear (pane-redisplay-needed sheet))))        
         (window-clear sheet))
       (redisplay-frame-pane frame sheet :force-p force-p)))
   (frame-top-level-sheet frame)))

(defmethod redisplay-frame-panes ((frame application-frame) &key force-p)
  (map-over-sheets (lambda (sheet)
		     (redisplay-frame-pane frame sheet :force-p force-p))
		   (frame-top-level-sheet frame)))


(defmethod frame-replay (frame stream &optional region)
  (declare (ignore frame))
  (stream-replay stream region))

(defmethod frame-properties ((frame application-frame) property)
  (getf (%frame-properties frame) property))

(defmethod (setf frame-properties) (value (frame application-frame) property)
  (setf (getf (%frame-properties frame) property) value))

;;; Command loop interface

(define-condition frame-exit (condition)
  ((frame :initarg :frame :reader %frame-exit-frame)))

;; I make the assumption here that the contents of *application-frame* is
;; the frame the top-level loop is running. With the introduction of
;; window-stream frames that may be sharing the event queue with the main
;; application frame, we need to discriminate between them here to avoid
;; shutting down the application at the wrong time.
;; ...
;; A better way to do this would be to make the handler bound in
;; run-frame-top-level check whether the frame signalled is the one
;; it was invoked on..                    -- Hefner

(defmethod frame-exit ((frame standard-application-frame))
  (if (eq *application-frame* frame)
      (signal 'frame-exit :frame frame)
    (disown-frame (frame-manager frame) frame)))

(defmethod frame-exit-frame ((c frame-exit))
  (%frame-exit-frame c))

(defmethod redisplay-frame-pane ((frame application-frame) pane &key force-p)
  (declare (ignore pane force-p))
  nil)

(defmethod redisplay-frame-pane :around ((frame application-frame) pane
					 &key force-p)
  (multiple-value-bind (redisplayp clearp)
      (pane-needs-redisplay pane)
    (when force-p
      (setq redisplayp (or redisplayp t)
	    clearp t))
    (when redisplayp
      (let ((hilited (frame-hilited-presentation frame)))
	(when hilited
	  (highlight-presentation-1 (car hilited) (cdr hilited) :unhighlight)
	  (setf (frame-hilited-presentation frame) nil)))
      (when clearp
	(window-clear pane))
      (call-next-method)
      (unless (or (eq redisplayp :command-loop) (eq redisplayp :no-clear))
	(setf (pane-needs-redisplay pane) nil)))))

(defmethod run-frame-top-level ((frame application-frame)
				&key &allow-other-keys)
  (handler-case
      (funcall (frame-top-level-lambda frame) frame)
    (frame-exit ()
      nil)))

(defmethod run-frame-top-level :around ((frame application-frame) &key)
  (let ((*application-frame* frame)
	(*input-context* nil)
	(*input-wait-test* nil)
	(*input-wait-handler* nil)
	(*pointer-button-press-handler* nil)
	(original-state (frame-state frame)))
    (declare (special *input-context* *input-wait-test* *input-wait-handler*
		      *pointer-button-press-handler*))
    (when (eq (frame-state frame) :disowned) ; Adopt frame into frame manager
      (adopt-frame (or (frame-manager frame) (find-frame-manager))
		   frame))
    (unless (or (eq (frame-state frame) :enabled)
		(eq (frame-state frame) :shrunk))
      (enable-frame frame))
    (unwind-protect
	 (loop
	    for query-io = (frame-query-io frame)
	    for *default-frame-manager* = (frame-manager frame)
	    do (handler-case
		   (return (if query-io
			       (with-input-focus (query-io)
				 (call-next-method))
			       (call-next-method)))
		 (frame-layout-changed () nil)))
      (let ((fm (frame-manager frame)))
	(case original-state
	  (:disabled
	   (disable-frame frame))
	  (:disowned
	   (disown-frame fm frame)))))))

(defparameter +default-prompt-style+ (make-text-style :fix :italic :normal))

(defmethod default-frame-top-level
    ((frame application-frame)
     &key (command-parser 'command-line-command-parser)
	  (command-unparser 'command-line-command-unparser)
	  (partial-command-parser
	   'command-line-read-remaining-arguments-for-partial-command)
	  (prompt "Command: "))
  ;; Give each pane a fresh start first time through.
  (let ((first-time t))
    (loop
       ;; The variables are rebound each time through the loop because the
       ;; values of frame-standard-input et al. might be changed by a command.
       (let* ((*standard-input*  (or (frame-standard-input frame)
				     *standard-input*))
	      (*standard-output* (or (frame-standard-output frame)
				     *standard-output*))
	      (query-io  (frame-query-io frame))
	      (*query-io* (or query-io *query-io*))
	      (*pointer-documentation-output*
	       (frame-pointer-documentation-output frame))
	      ;; during development, don't alter *error-output*
	      ;; (*error-output* (frame-error-output frame))
	      (*command-parser* command-parser)
	      (*command-unparser* command-unparser)
	      (*partial-command-parser* partial-command-parser)
	      (interactorp (typep *query-io* 'interactor-pane)))
	 (restart-case
	     (progn
	       (redisplay-frame-panes frame :force-p first-time)
	       (setq first-time nil)
	       (if query-io
                 ;; We don't need to turn the cursor on here, as Goatee has its own
                 ;; cursor which will appear. In fact, leaving it on causes much
                 ;; bit flipping and slows command output somewhat. So, leave it
                 ;; off by default, and hope this doesn't violate the spec.  
		   (progn
		     (setf (cursor-visibility (stream-text-cursor *query-io*))
			   nil)
		     (when (and prompt interactorp)
		       (with-text-style (*query-io* +default-prompt-style+)
			 (if (stringp prompt)
			     (write-string prompt *query-io*)
			     (funcall prompt *query-io* frame))
			 (finish-output *query-io*)))
		     (let ((command (read-frame-command frame
							:stream *query-io*)))
		       (when interactorp
			 (fresh-line *query-io*))
		       (when command
			 (execute-frame-command frame command))
		       (when interactorp
			 (fresh-line *query-io*))))
		   (simple-event-loop)))
	   (abort ()
	     :report "Return to application command loop"
	     (if interactorp
		 (format *query-io* "~&Command aborted.~&")
		 (beep))))))))

(defmethod read-frame-command :around ((frame application-frame)
				       &key (stream *standard-input*))  
  (with-input-context ('menu-item)
      (object)
      (call-next-method)
    (menu-item
     (let ((command (command-menu-item-value object)))
       (unless (listp command)
	 (setq command (list command)))       
       (if (and (typep stream 'interactor-pane)
		(member *unsupplied-argument-marker* command :test #'eq))
	   (command-line-read-remaining-arguments-for-partial-command
	    (frame-command-table frame) stream command 0)
	   command)))))

(defmethod read-frame-command ((frame application-frame)
			       &key (stream *standard-input*))
  ;; The following is the correct interpretation according to the spec.
  ;; I think it is terribly counterintuitive and want to look into
  ;; what existing CLIMs do before giving in to it.
  ;; If we do things as the spec says, command accelerators will
  ;; appear to not work, confusing new users.
  #+NIL (read-command (frame-command-table frame) :use-keystrokes nil :stream stream)
  (read-command (frame-command-table frame) :use-keystrokes t :stream stream))

(defmethod execute-frame-command ((frame application-frame) command)
  (apply (command-name command) (command-arguments command)))

(defmethod command-enabled (command-name (frame standard-application-frame))
  (and (command-accessible-in-command-table-p command-name
					      (frame-command-table frame))
       (not (member command-name (disabled-commands frame)))))

(defmethod (setf command-enabled)
    (enabled command-name (frame standard-application-frame))
  (unless (command-accessible-in-command-table-p command-name
						 (frame-command-table frame))
    (return-from command-enabled nil))
  (with-accessors ((disabled-commands disabled-commands))
      frame
    (if enabled
	(progn
	  (setf disabled-commands (delete command-name disabled-commands))
	  (note-command-enabled (frame-manager frame)
				frame
				command-name)
	  enabled)
	(progn
	  (pushnew command-name disabled-commands)
	  (note-command-disabled (frame-manager frame)
				 frame
				 command-name)
	  nil))))


(defmethod make-pane-1 :around (fm (frame standard-application-frame) type
				&rest args
				&key (input-buffer nil input-buffer-p)
				(name nil namep)
				&allow-other-keys)
  (declare (ignore name input-buffer))
  "Default input-buffer to the frame event queue."
  (let ((pane (if input-buffer-p
		  (call-next-method)
		  (apply #'call-next-method fm frame type
			 :input-buffer (frame-event-queue frame)
			 args))))
    (when namep
      (push pane (frame-named-panes frame)))
    pane))

(defmethod adopt-frame ((fm frame-manager) (frame application-frame))
  (setf (slot-value fm 'frames) (cons frame (slot-value fm 'frames)))
  (setf (frame-manager frame) fm)
  (setf (port frame) (frame-manager-port fm))
  (setf (graft frame) (find-graft :port (port frame)))
  (let* ((*application-frame* frame)
	 (t-l-s (make-pane-1 fm frame 'top-level-sheet-pane
			     :name 'top-level-sheet
			     ;; enabling should be left to enable-frame
			     :enabled-p nil))
         #+clim-mp (event-queue (sheet-event-queue t-l-s)))
    (setf (slot-value frame 'top-level-sheet) t-l-s)
    (generate-panes fm frame)
    (setf (slot-value frame 'state)  :disabled)
    #+clim-mp
    (when (typep event-queue 'port-event-queue)
      (setf (event-queue-port event-queue)
            (frame-manager-port fm)))
    frame))

(defmethod disown-frame ((fm frame-manager) (frame application-frame))
  #+CLIM-MP
  (let* ((t-l-s (frame-top-level-sheet frame))
         (queue (sheet-event-queue t-l-s)))
    (when (typep queue 'port-event-queue)
      (setf (event-queue-port queue) nil)))
  (setf (slot-value fm 'frames) (remove frame (slot-value fm 'frames)))
  (sheet-disown-child (graft frame) (frame-top-level-sheet frame))
  (setf (%frame-manager frame) nil)
  (setf (slot-value frame 'state) :disowned)
  (port-force-output (frame-manager-port fm))
  frame)

(defgeneric enable-frame (frame))
(defgeneric disable-frame (frame))

(defgeneric note-frame-enabled (frame-manager frame))
(defgeneric note-frame-disbled (frame-manager frame))

(defmethod enable-frame ((frame application-frame))
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) t)
  (setf (slot-value frame 'state) :enabled)
  (note-frame-enabled (frame-manager frame) frame))

(defmethod disable-frame ((frame application-frame))
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) nil)
  (setf (slot-value frame 'state) :disabled)
  (note-frame-disabled (frame-manager frame) frame))

(defmethod note-frame-enabled ((fm frame-manager) frame)
  (declare (ignore frame))
  t)

(defmethod note-frame-disabled ((fm frame-manager) frame)
  (declare (ignore frame))
  t)

(defvar *pane-realizer* nil)

(defmacro with-look-and-feel-realization ((frame-manager frame) &body body)
  `(let ((*pane-realizer* ,frame-manager)
	 (*application-frame* ,frame))
     (locally
         ,@body)))

; The menu-bar code in the following two functions is incorrect.
; it needs to be moved to somewhere after the backend, since
; it depends on the backend chosen.
;
; This hack slaps a menu-bar into the start of the application-frame,
; in such a way that it is hard to find.
;
; FIXME
(defun make-single-pane-generate-panes-form (class-name menu-bar pane)
  `(progn
     (defmethod generate-panes ((fm frame-manager) (frame ,class-name))
       ;; v-- hey, how can this be?
       (with-look-and-feel-realization (fm frame)
	 (let ((pane ,(cond
		       ((eq menu-bar t)
			`(vertically () (clim-internals::make-menu-bar
                                         ',class-name)
				     ,pane))
		       ((consp menu-bar)
			`(vertically () (clim-internals::make-menu-bar
                                         (make-command-table nil
							     :menu ',menu-bar))
				     ,pane))
		       (menu-bar
			`(vertically () (clim-internals::make-menu-bar
					 ',menu-bar)
				     ,pane))
		       ;; The form below is unreachable with (listp
		       ;; menu-bar) instead of (consp menu-bar) above
		       ;; --GB
		       (t pane))))
	   (setf (slot-value frame 'panes) pane))))
     (defmethod frame-all-layouts ((frame ,class-name))
       nil)))

(defun find-pane-for-layout (name frame)
  (cdr (assoc name (frame-panes-for-layout frame) :test #'eq)))

(defun save-pane-for-layout (name pane frame)
  (push (cons name pane) (frame-panes-for-layout frame))
  pane)

(defun coerce-pane-name (pane name)
  (when pane 
    (setf (slot-value pane 'name) name)    
    (push pane (frame-named-panes (pane-frame pane))))
  pane)

(defun do-pane-creation-form (name form)  
  (cond
    ((and (= (length form) 1)
	  (listp (first form)))
     `(coerce-pane-name ,(first form) ',name))
    ((keywordp (first form))
     (let ((maker (intern (concatenate 'string
				       (symbol-name '#:make-clim-)
				       (symbol-name (first form))
				       (symbol-name '#:-pane))
			  :clim)))
       (if (fboundp maker)
	   `(,maker :name ',name ,@(cdr form))
	   `(make-pane ',(first form)
		       :name ',name ,@(cdr form)))))
    (t `(make-pane ',(first form) :name ',name ,@(cdr form)))))

(defun make-panes-generate-panes-form (class-name menu-bar panes layouts
				       pointer-documentation)
  (when pointer-documentation
    (setf panes (append panes
			'((%pointer-documentation%
			   pointer-documentation-pane)))))
  `(progn
     (defmethod generate-panes ((fm frame-manager) (frame ,class-name))
       (let ((*application-frame* frame))
	 (with-look-and-feel-realization (fm frame)
	   (let ,(loop
		    for (name . form) in panes
		    collect `(,name (or (find-pane-for-layout ',name frame)
					(save-pane-for-layout
					 ',name
					 ,(do-pane-creation-form name form)
					 frame))))
	     ;; [BTS] added this, but is not sure that this is correct for
	     ;; adding a menu-bar transparently, should also only be done
	     ;; where the exterior window system does not support menus
	     ,(if (or menu-bar pointer-documentation)
		  `(setf (slot-value frame 'panes)
			 (ecase (frame-current-layout frame)
			   ,@(mapcar (lambda (layout)
				       `(,(first layout)
					  (vertically ()
					    ,@(cond
					       ((eq menu-bar t)
						`((clim-internals::make-menu-bar
						   ',class-name)))
					       ((consp menu-bar)
						`((clim-internals::make-menu-bar
						   (make-command-table
						    nil
						    :menu ',menu-bar))))
					       (menu-bar
						`((clim-internals::make-menu-bar
						   ',menu-bar)))
					       (t nil))
					    ,@(rest layout)
					    ,@(when pointer-documentation
						'(%pointer-documentation%)))))
				     layouts)))
		  `(setf (slot-value frame 'panes)
			 (ecase (frame-current-layout frame)
			   ,@layouts)))))))
     (defmethod frame-all-layouts ((frame ,class-name))
       ',(mapcar #'car layouts))))

(defmacro define-application-frame (name superclasses slots &rest options)
  (if (null superclasses)
      (setq superclasses '(standard-application-frame)))
  (let ((pane nil)
	(panes nil)
	(layouts nil)
	(current-layout nil)
	(command-table (list name))
	(menu-bar t)
	(disabled-commands nil)
	(command-definer t)
	(top-level '(default-frame-top-level))
	(others nil)	
	(pointer-documentation nil)
	(geometry nil)
	(frame-arg (gensym "FRAME-ARG")))
    (loop for (prop . values) in options
	do (case prop
	     (:pane (setq pane (first values)))
	     (:panes (setq panes values))
	     (:layouts (setq layouts values))
	     (:command-table (setq command-table (first values)))
	     (:menu-bar (setq menu-bar (if (listp values)
                                           (first values)
                                           values)))
	     (:disabled-commands (setq disabled-commands values))
	     (:command-definer (setq command-definer (first values)))
	     (:top-level (setq top-level (first values)))
	     (:pointer-documentation (setq pointer-documentation (car values)))
	     (:geometry (setq geometry values))
	     (t (push (cons prop values) others))))
    (when (eq command-definer t)
      (setf command-definer
            (intern (concatenate 'string
                                 (symbol-name '#:define-)
                                 (symbol-name name)
                                 (symbol-name '#:-command)))))
    (if (or (and pane panes)
	    (and pane layouts))
	(error ":pane cannot be specified along with either :panes or :layouts"))
    (if pane
	(setq panes (list 'single-pane pane)
	      layouts `((:default ,(car pane)))))
    (setq current-layout (first (first layouts)))
    `(progn
      (defclass ,name ,superclasses
        ,slots
        (:default-initargs
         :name ',name
         :pretty-name ,(string-capitalize name)
         :command-table (find-command-table ',(first command-table))
         :disabled-commands ',disabled-commands
         :menu-bar ',menu-bar
         :current-layout ',current-layout
         :layouts ',layouts
         :top-level (list ',(car top-level) ,@(cdr top-level))
         :top-level-lambda (lambda (,frame-arg)
                             (,(car top-level) ,frame-arg
                               ,@(cdr top-level))))
        ,@others)
      ;; We alway set the frame class default geometry, so that the
      ;; user can undo the effect of a specified :geometry option.
      ;; --GB 2004-06-01
      (setf (get ',name 'application-frame-geometry) ',geometry)
      ,(if pane
           (make-single-pane-generate-panes-form name menu-bar pane)
           (make-panes-generate-panes-form name menu-bar panes layouts
                                           pointer-documentation))
      ,@(if command-table
            `((define-command-table ,@command-table)))
      ,@(if command-definer
            `((defmacro ,command-definer (name-and-options arguments &rest body)
                (let ((name (if (listp name-and-options) (first name-and-options) name-and-options))
                      (options (if (listp name-and-options) (cdr name-and-options) nil))
                      (command-table ',(first command-table)))
                  `(define-command (,name :command-table ,command-table ,@options) ,arguments ,@body))))))))

(defun get-application-frame-class-geometry (name indicator)
  (getf (get name 'application-frame-geometry) indicator nil))

(defun make-application-frame (frame-name
			       &rest options
			       &key (pretty-name
				     (string-capitalize frame-name))
			            (frame-manager nil frame-manager-p)
			            enable
			            (state nil state-supplied-p)
				    (left (get-application-frame-class-geometry frame-name :left))
				    (top (get-application-frame-class-geometry frame-name :top))
				    (right (get-application-frame-class-geometry frame-name :right))
				    (bottom (get-application-frame-class-geometry frame-name :bottom))
				    (width (get-application-frame-class-geometry frame-name :width))
				    (height  (get-application-frame-class-geometry frame-name :height))
			            save-under (frame-class frame-name)
			       &allow-other-keys)
  (declare (ignore save-under))
  (with-keywords-removed (options (:pretty-name :frame-manager :enable :state
				   :left :top :right :bottom :width :height
				   :save-under :frame-class))
    (let ((frame (apply #'make-instance frame-class
			:name frame-name
			:pretty-name pretty-name
                        :user-supplied-geometry
                        (list :left left :top top
                              :right right :bottom bottom
                              :width width :height height)
			options)))
      (when frame-manager-p
	(adopt-frame frame-manager frame))
      (cond ((or enable (eq state :enabled))
	     (enable-frame frame))
	    ((and (eq state :disowned)
		  (not (eq (frame-state frame) :disowned)))
	     (disown-frame (frame-manager frame) frame))
	    (state-supplied-p
	     (warn ":state ~S not supported yet." state)))
      frame)))

;;; Menu frame class

(defclass menu-frame ()
  ((left :initform 0 :initarg :left)
   (top :initform 0 :initarg :top)
   (min-width :initform nil :initarg :min-width)
   (top-level-sheet :initform nil :reader frame-top-level-sheet)
   (panes :reader frame-panes :initarg :panes)
   (graft :initform nil :accessor graft)
   (manager :initform nil :accessor frame-manager)))

(defmethod adopt-frame ((fm frame-manager) (frame menu-frame))
  (setf (slot-value fm 'frames) (cons frame (slot-value fm 'frames)))
  (setf (frame-manager frame) fm)
  (let* ((t-l-s (make-pane-1 fm *application-frame*
                             'unmanaged-top-level-sheet-pane
			     :name 'top-level-sheet)))
    (setf (slot-value frame 'top-level-sheet) t-l-s)
    (sheet-adopt-child t-l-s (frame-panes frame))
    (let ((graft (find-graft :port (frame-manager-port fm))))
      (sheet-adopt-child graft t-l-s)
      (setf (graft frame) graft))
    (let ((pre-space (compose-space t-l-s))
          (frame-min-width (slot-value frame 'min-width)))
      (multiple-value-bind (width min-width max-width height min-height max-height)
          (space-requirement-components pre-space)
        (flet ((foomax (x y) (max (or x 1) (or y 1))))
          (let ((space (make-space-requirement :min-width  (foomax frame-min-width min-width)
                                               :width      (foomax frame-min-width width)
                                               :max-width  (foomax frame-min-width max-width)
                                               :min-height min-height
                                               :height     height
                                               :max-height max-height)))
            (allocate-space (frame-panes frame)
                            (space-requirement-width space)
                            (space-requirement-height space))
            (setf (sheet-region t-l-s)
                  (make-bounding-rectangle 0 0
                                           (space-requirement-width space)
                                           (space-requirement-height space))))
          (setf (sheet-transformation t-l-s)
                (make-translation-transformation (slot-value frame 'left)
                                                 (slot-value frame 'top))))))))

(defmethod disown-frame ((fm frame-manager) (frame menu-frame))
  (setf (slot-value fm 'frames) (remove frame (slot-value fm 'frames)))
  (sheet-disown-child (graft frame) (frame-top-level-sheet frame))
  (setf (frame-manager frame) nil))

(defun make-menu-frame (pane &key (left 0) (top 0) (min-width 1))
  (make-instance 'menu-frame :panes pane :left left :top top :min-width min-width))

;;; Frames and presentations
(defmethod frame-maintain-presentation-histories
    ((frame standard-application-frame))
  (if (find-pane-of-type (frame-panes frame) 'interactor-pane)
      t
      nil))

(defmethod frame-find-innermost-applicable-presentation
    ((frame standard-application-frame) input-context stream x y
     &key event)
  (find-innermost-applicable-presentation input-context stream
					  x y
					  :frame frame :event event))

(defmethod frame-input-context-button-press-handler
    ((frame standard-application-frame)
     (stream output-recording-stream)
     button-press-event)
  (let ((presentation (find-innermost-applicable-presentation
		       *input-context*
		       stream
		       (pointer-event-x button-press-event)
		       (pointer-event-y button-press-event)
		       :frame frame
		       :event button-press-event)))
    (when presentation
      (throw-highlighted-presentation presentation
				      *input-context*
				      button-press-event))))

(defmethod frame-input-context-button-press-handler
    ((frame standard-application-frame) stream button-press-event)
  (declare (ignore stream button-press-event))
  nil)

(defgeneric frame-update-pointer-documentation
    (frame input-context stream event))

(defconstant +button-documentation+ '((#.+pointer-left-button+ "L")
				      (#.+pointer-middle-button+ "M")
                                      (#.+pointer-right-button+ "R")
                                      (#.+pointer-wheel-up+ "WheelUp")
                                      (#.+pointer-wheel-down+ "WheelDown")))

(defconstant +modifier-documentation+
  '((#.+shift-key+ "sh" "Shift")
    (#.+control-key+ "c" "Control")
    (#.+meta-key+ "m" "Meta")
    (#.+super-key+ "s" "Super")
    (#.+hyper-key+ "h" "Hyper")))

;;; Give a coherent order to sets of modifier combinations.  Multi-key combos
;;; come after single keys.

(defun cmp-modifiers (a b)
  (let ((cnt-a (logcount a))
	(cnt-b (logcount b)))
    (cond ((eql cnt-a cnt-b)
	   (< a b))
	  (t (< cnt-a cnt-b)))))

(defun print-modifiers (stream modifiers style)
  (if (zerop modifiers)
      (when (eq style :long)
	(write-string "<nothing>" stream))
      (loop with trailing = nil
	    for (bit short long) in +modifier-documentation+
	    when (logtest bit modifiers)
	    do (progn
		 (format stream "~:[~;-~]~A" trailing (if (eq style :short)
							  short
							  long))
		 (setq trailing t)))))


;;; We don't actually want to print out the translator documentation and redraw
;;; the pointer documentation window on every motion event.  So, we compute a
;;; state object (basically modifier state and a list of the applicable
;;; presentation, translator and input context on each mouse button),
;;; compare it to the previous state object, and only write out documentation
;;; if they are different.  I suppose it's possible that this state object
;;; doesn't capture all possible documentation changes -- the doc generator is
;;; a function, after all -- but that's just tough.
;;;
;;; It would be nice to evolve this into a protocol so that elements other than
;;; presentations -- menu choices, for example -- could influence pointer
;;; documentation window.

(defgeneric frame-compute-pointer-documentation-state 
    (frame input-context stream event)
  (:documentation
   "Compute a state object that will be used to generate pointer documentation."))

(defmethod frame-compute-pointer-documentation-state
    ((frame standard-application-frame) input-context stream event)
  (let* ((current-modifier (event-modifier-state event))
	 (x (device-event-x event))
	 (y (device-event-y event))
	 (new-translators
	  (loop for (button) in +button-documentation+
	      for context-list = (multiple-value-list
				  (find-innermost-presentation-context
				   input-context
				   stream
				   x y
				   :modifier-state current-modifier
				   :button button))
	      when (car context-list)
	      collect (cons button context-list))))
    (list current-modifier new-translators)))

(defgeneric frame-compare-pointer-documentation-state
    (frame input-context stream old-state new-state))

(defmethod frame-compare-pointer-documentation-state
    ((frame standard-application-frame) input-context stream
     old-state new-state)
  (declare (ignore input-context stream))
  (equal old-state new-state))

(defgeneric frame-print-pointer-documentation
    (frame input-context stream state event))

(defmethod frame-print-pointer-documentation
    ((frame standard-application-frame) input-context stream state event)
  (unless state
    (return-from frame-print-pointer-documentation nil))
  (destructuring-bind (current-modifier new-translators)
      state
    (let ((x (device-event-x event))
	  (y (device-event-y event))
	  (pstream *pointer-documentation-output*))
      (loop for (button presentation translator context)
	    in new-translators
	    for name = (cadr (assoc button +button-documentation+))
	    for first-one = t then nil
	    do (progn
		 (unless first-one
		   (write-string "; " pstream))
		 (unless (zerop current-modifier)
		   (print-modifiers pstream current-modifier :short)
		   (write-string "-" pstream))
		 (format pstream "~A: " name)
		 (document-presentation-translator translator
						   presentation
						   (input-context-type context)
						   *application-frame*
						   event
						   stream
						   x y
						   :stream pstream
						   :documentation-type
						   :pointer))
	    finally (when new-translators
		      (write-char #\. pstream)))
      ;; Wasteful to do this after doing
      ;; find-innermost-presentation-context above... look at doing this
      ;; first and then doing the innermost test.
      (let ((all-translators (find-applicable-translators
			      (stream-output-history stream)
			      input-context
			      *application-frame*
			      stream
			      x y
			      :for-menu t))
	    (other-modifiers nil))
	(loop for (translator) in all-translators
	      for gesture = (gesture translator)
	      unless (eq gesture t)
	      do (loop for (name type modifier) in gesture
		       unless (eql modifier current-modifier)
		       do (pushnew modifier other-modifiers)))
	(when other-modifiers
	  (setf other-modifiers (sort other-modifiers #'cmp-modifiers))
	  (terpri pstream)
	  (write-string "To see other commands, press "	pstream)
	  (loop for modifier-tail on other-modifiers
		for (modifier) = modifier-tail
		for count from 0
		do (progn
		     (if (null (cdr modifier-tail))
			 (progn
			   (when (> count 1)
			     (write-char #\, pstream))
			   (when (> count 0)
			     (write-string " or " pstream)))
			 (when (> count 0)
			   (write-string ", " pstream)))
		     (print-modifiers pstream modifier :long)))
	  (write-char #\. pstream))))))

(defmethod frame-update-pointer-documentation
    ((frame standard-application-frame) input-context stream event)
  (when *pointer-documentation-output*
    (with-accessors ((frame-documentation-state frame-documentation-state))
	frame
      (let ((new-state (frame-compute-pointer-documentation-state frame
								  input-context
								  stream
								  event)))
	(unless (frame-compare-pointer-documentation-state
		 frame
		 input-context
		 stream
		 frame-documentation-state
		 new-state)
	  (window-clear *pointer-documentation-output*)
	  (frame-print-pointer-documentation frame
					     input-context
					     stream
					     new-state
					     event)
	  (setq frame-documentation-state new-state))))))

;;; A hook for applications to draw random strings in the
;;; *pointer-documentation-output* without screwing up the real pointer
;;; documentation too badly.

(defgeneric frame-display-pointer-documentation-string
    (frame documentation-stream string))

(defmethod frame-display-pointer-documentation-string
    ((frame standard-application-frame) documentation-stream string)
  (when *pointer-documentation-output*
    (with-accessors ((frame-documentation-state frame-documentation-state))
        frame
      (unless (frame-compare-pointer-documentation-state
	       frame nil documentation-stream frame-documentation-state string)
	(window-clear documentation-stream)
	(write-string string documentation-stream)
	(setq frame-documentation-state string)))))

(defmethod frame-input-context-track-pointer
    ((frame standard-application-frame)
     input-context
     (stream output-recording-stream) event)
  (declare (ignore input-context event))
  nil)

(defmethod frame-input-context-track-pointer
    ((frame standard-application-frame) input-context stream event)
  (declare (ignore input-context stream event))
  nil)

(defun frame-highlight-at-position (frame stream x y modifier input-context
				    &key (highlight t))
  "Given stream x,y; key modifiers; input-context, find the applicable
   presentation and maybe highlight it."
  (flet ((maybe-unhighlight (presentation)
	   (when (and (frame-hilited-presentation frame)
		      (or (not highlight)
			  (not (eq presentation
			       (car (frame-hilited-presentation frame))))))
	     (highlight-presentation-1 (car (frame-hilited-presentation frame))
				       (cdr (frame-hilited-presentation frame))
				       :unhighlight)
	     (setf (frame-hilited-presentation frame) nil))))
    (if (output-recording-stream-p stream)
	(let ((presentation (find-innermost-applicable-presentation
			     input-context
                             stream
			     x y
			     :frame frame
			     :modifier-state modifier)))
	  (maybe-unhighlight presentation)
	  (when (and presentation
		     highlight
		     (not (eq presentation
			      (car (frame-hilited-presentation frame)))))
	    (setf (frame-hilited-presentation frame)
		  (cons presentation stream))
	    (highlight-presentation-1 presentation stream :highlight))
	  presentation)
	(progn
	  (maybe-unhighlight nil)
	  nil))))

(defmethod frame-input-context-track-pointer :before
    ((frame standard-application-frame) input-context
     (stream output-recording-stream) event)
  (frame-highlight-at-position frame stream
                               (device-event-x event)
                               (device-event-y event)
                               (event-modifier-state event)
                               input-context)
  (frame-update-pointer-documentation frame input-context stream event))

(defun simple-event-loop ()
  "An simple event loop for applications that want all events to be handled by
 handle-event methods"
  (let ((queue (frame-event-queue *application-frame*)))
    (loop for event = (event-queue-read queue)
       ;; EVENT-QUEUE-READ in single-process mode calls PROCESS-NEXT-EVENT itself.
       do (handle-event (event-sheet event) event))))

;;; Am I missing something?  Does this need to do more? - moore
(defmacro with-application-frame ((frame) &body body)
  `(let ((,frame *application-frame*))
     ,@body))


(defmethod note-input-focus-changed (pane state)
  (declare (ignore pane state)))

(defmethod (setf keyboard-input-focus) :after (focus frame)
  (%set-port-keyboard-focus (port frame) focus))

(defmethod (setf client-setting) (value frame setting)
  (setf (getf (client-settings frame) setting) value))

(defmethod reset-frame (frame &rest client-settings)
  (loop for (setting value) on client-settings by #'cddr
	do (setf (client-setting frame setting) value)))

;;; tracking-pointer stuff related to presentations

(defclass frame-tracking-pointer-state (tracking-pointer-state)
  ((presentation-handler :reader presentation-handler :initarg :presentation)
   (presentation-button-release-handler
    :reader presentation-button-release-handler
    :initarg :presentation-button-release)
   (presentation-button-press-handler :reader presentation-button-press-handler
				      :initarg :presentation-button-press)
   (applicable-presentation :accessor applicable-presentation :initform nil)
   (input-context :reader input-context)
   (highlight :reader highlight))
  (:default-initargs :presentation nil
                     :presentation-button-press nil
		     :presentation-button-release nil
		     :context-type t))

(defmethod initialize-instance :after
    ((obj frame-tracking-pointer-state)
     &key presentation presentation-button-press presentation-button-release
     (highlight nil highlightp) context-type
     multiple-window)
  (declare (ignore multiple-window))
  (setf (slot-value obj 'highlight) (if highlightp
					highlight
					(or presentation
					    presentation-button-press
					    presentation-button-release)))
  (setf (slot-value obj 'input-context)
	(make-fake-input-context context-type)))

(defmethod make-tracking-pointer-state
    ((frame standard-application-frame)	sheet args)
  (declare (ignore sheet))
  (apply #'make-instance 'frame-tracking-pointer-state
	 args))

(defmethod tracking-pointer-loop :before
    ((state frame-tracking-pointer-state) frame sheet &rest args)
  (declare (ignore args))
  (if (highlight state)
      (highlight-current-presentation frame (input-context state))
      (let ((hilited (frame-hilited-presentation frame)))
	(when hilited
	  (highlight-presentation-1 (car hilited)
				    (cdr hilited)
				    :unhighlight)))))

;;; Poor man's find-applicable-translators. tracking-pointer doesn't want to
;;; see any results from presentation translators.

(defun highlight-for-tracking-pointer (frame stream x y input-context)
  (let ((context-ptype (input-context-type (car input-context)))
	(presentation nil)
	(current-hilited (frame-hilited-presentation frame)))
    (if (output-recording-stream-p stream)
	(progn
	  (block found-presentation
	    (flet ((do-presentation (p)
		     (when (presentation-subtypep (presentation-type p)
						  context-ptype)
		       (setq presentation p)
		       (return-from found-presentation nil))))
	      (declare (dynamic-extent #'do-presentation))
	      (map-over-presentations-containing-position
	       #'do-presentation (stream-output-history stream) x y)))
	  (when (and current-hilited
		     (not (eq (car current-hilited) presentation)))
	    (highlight-presentation-1 (car current-hilited)
				      (cdr current-hilited)
				      :unhighlight))
	  (if presentation
	      (progn
		(setf (frame-hilited-presentation frame)
		      (cons presentation stream))
		(highlight-presentation-1 presentation stream :highlight)))
	  presentation))))

(defmethod tracking-pointer-loop-step :before
    ((state frame-tracking-pointer-state) (event pointer-event) x y)
  (declare (ignore x y))
  (when (highlight state)
    (let ((stream (event-sheet event)))
      (setf (applicable-presentation state)
	    (highlight-for-tracking-pointer *application-frame* stream
					    (device-event-x event)
					    (device-event-y event)
					    (input-context state))))))


(macrolet ((frob (event handler)
	     `(defmethod tracking-pointer-loop-step
		  ((state frame-tracking-pointer-state) (event ,event)  x y)
		(let ((handler (,handler state))
		      (presentation (applicable-presentation state)))
		  (if (and handler presentation)
		      (funcall handler :presentation presentation
                               :event event
			       :window (event-sheet event)
			       :x x :y y)
		      (call-next-method))))))
  (frob pointer-motion-event presentation-handler)
  (frob pointer-button-press-event presentation-button-press-handler)
  (frob pointer-button-release-event presentation-button-release-handler))

(defun make-drag-bounding (old-highlighting new-highlighting
			   old-presentation new-presentation)
  (let (x1 y1 x2 y2)
    (flet ((union-with-bounds (rect)
	     (cond ((null rect)
		    nil)
		   ((null x1)
		    (setf (values x1 y1 x2 y2) (bounding-rectangle* rect)))
		   (t (with-bounding-rectangle* (r-x1 r-y1 r-x2 r-y2)
			  rect
			(setf (values x1 y1 x2 y2)
			      (bound-rectangles x1 y1 x2 y2
						r-x1 r-y1 r-x2 r-y2)))))))
      (union-with-bounds old-highlighting)
      (union-with-bounds new-highlighting)
      (union-with-bounds old-presentation)
      (union-with-bounds new-presentation)
      (values x1 y1 x2 y2))))

(defun make-drag-and-drop-feedback-function (from-presentation)
  (multiple-value-bind (record-x record-y)
      (output-record-position from-presentation)
    (let ((current-to-presentation nil)
	  (current-from-higlighting nil))
      (lambda (frame from-presentation to-presentation initial-x initial-y
	       x y event)
	(let ((dx (- record-x initial-x))
	      (dy (- record-y initial-y)))
	  (typecase event
	    (null
	     ())))))))

(defun frame-drag (translator-name command-table object presentation
		   context-type frame event window x y)
  (let* ((translator (gethash translator-name
			      (translators (presentation-translators
					    (find-command-table
					     command-table)))))
	 (tester (tester translator))
	 (drag-type (from-type translator))
	 (feedback-fn (feedback translator))
	 (hilite-fn (highlighting translator))
	 (drag-c-type `(drag-over ))
	 (drag-context (make-fake-input-context drag-c-type))
	 (*dragged-object* object)
	 (destination-object nil))
    (multiple-value-bind (x0 y0)
	(stream-pointer-position window)
      (funcall feedback-fn *application-frame* object window
	       x0 y0 x0 y0 :highlight)
      (tracking-pointer (window :context-type drag-c-type :highlight nil)
       (:pointer-motion (&key event x y)
	 (multiple-value-bind (presentation translator)
	     (find-innermost-presentation-match drag-context window
						x y :event event)))))))
