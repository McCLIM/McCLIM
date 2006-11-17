(in-package :clim-listener)

;;; This is a lisp listener.

;;; (C) Copyright 2003 by Andy Hefner (hefner1@umbc.edu)

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


;; Wholine Pane

(defclass wholine-pane (application-pane) ()
  (:default-initargs :background +gray90+))

(defmethod compose-space ((pane wholine-pane) &key width height)
  (declare (ignore width height))  
  (let ((h (* 1.5 (text-style-height (medium-text-style pane) pane)))) ; magic padding
    (make-space-requirement :height h
                            :min-height h
                            :max-height h)))

;; When the pane is grown, we must repaint more than just the newly exposed
;; regions, because the decoration within the previous region must move.
;; Likewise, shrinking the pane requires repainting some of the interior.
(defmethod allocate-space :after ((pane wholine-pane) width height)
  (repaint-sheet pane (sheet-region pane)))

(defun print-package-name (stream)
  (let ((foo (package-name *package*)))
    (with-drawing-options (stream :ink +royalblue+)
      (format stream "~A" (reduce (lambda (&optional (a foo) (b foo))
                                    (if (< (length a) (length b)) a b))
                                  (package-nicknames *package*))))))

(defun frob-pathname (pathname)
  (namestring (truename pathname)))

;; How to add repaint-time decoration underneath the contents of a
;; stream pane: Write your own handle-repaint that draws the
;; decoration then replays the recorded output, and define a
;; window-clear method which calls the next window-clear method,
;; then calls handle-repaint to redraw the decoration.

(defmethod handle-repaint ((pane wholine-pane) region)
  (declare (ignore region))
  (with-output-recording-options (pane :draw t :record nil)
    (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region pane)
      (climi::draw-bordered-rectangle* (sheet-medium pane)
                                       x0 y0 x1 y1
                                       :style :mickey-mouse-inset)
      #+NIL (draw-rectangle* (sheet-medium pane) x0 y0 x1 y1 :ink +red+))
    (replay-output-record (stream-output-history pane) pane)))

(defmethod window-clear ((pane wholine-pane))
  (call-next-method)
  (handle-repaint pane (sheet-region pane)))

(defun generate-wholine-contents (frame pane)
  (declare (ignore frame))
  (let* ((*standard-output* pane)
         (username (or #+cmu (cdr (assoc :user ext:*environment-list*))
		       #+scl (cdr (assoc "USER" ext:*environment-list*
					 :test 'string=))
		       #+allegro (sys:getenv "USER")
		       #-(or allegro cmu scl) (getenv "USER")
                       "luser"))  ; sorry..
         (sitename (machine-instance))
         (memusage #+(or cmu scl) (lisp::dynamic-usage)
                   #+sbcl  (sb-kernel:dynamic-usage)
                   #+lispworks (getf (system:room-values) :total-allocated)
		   #+openmcl (+ (ccl::%usedbytes) (ccl::%freebytes))
                   #+clisp (values (sys::%room))
                   #-(or cmu scl sbcl lispworks openmcl clisp) 0))
    (with-text-family (t :serif)
      (formatting-table (t :x-spacing '(3 :character))
        (formatting-row (t)                        
          (macrolet ((cell ((align-x) &body body)                         
                       `(formatting-cell (t :align-x ,align-x) ,@body)))
            (cell (:left)   (format t "~A@~A" username sitename))
            (cell (:center)
              (format t "Package ")
              (print-package-name t))
            (cell (:center)
              (when (probe-file *default-pathname-defaults*)
                (with-output-as-presentation (t (truename *default-pathname-defaults*) 'pathname)
                  (format t "~A" (frob-pathname *default-pathname-defaults*))))
              (when *directory-stack*
                (with-output-as-presentation (t *directory-stack* 'directory-stack)
                  (format t "  (~D deep)" (length *directory-stack*)))))
          ;; Although the CLIM spec says the item formatter should try to fill
          ;; the available width, I can't get either the item or table formatters
          ;; to really do so such that the memory usage appears right justified.
            (cell (:center)
              (when (numberp memusage)
                (present memusage 'lisp-memory-usage)))))))))

(defun display-wholine (frame pane)
  (invoke-and-center-output pane
    (lambda () (generate-wholine-contents frame pane))
    :horizontally nil :hpad 5))    

;; This is a toy command history.
;; Possibly this should become something integrated with the presentation
;; histories, which I have not played with.

(defclass command-history-mixin ()
  ((history :initform nil :accessor history)
   (history-length :initform 25 :initarg :history-length :accessor history-length)))

(defmethod execute-frame-command :after ((frame command-history-mixin) command)
  ;; FIXME: not safe against commands sent from other frames.
  (push command (history frame))  
  (when (> (length (history frame)) (history-length frame))
    (setf (history frame)
          (subseq (history frame) 0 (max (length (history frame))
                                         (history-length frame))))))

(define-command (com-show-command-history :name "Show Command History"
                                          :command-table application-commands
                                          :menu ("Show Command History" :after "Clear Output History"))
    ()
  (formatting-table ()
     (loop for n from 0 by 1
           for command in (history *application-frame*)
           do (formatting-row ()
                (formatting-cell ()
                   (princ n))
                (formatting-cell ()
                   (present command 'command))))))

(defparameter *listener-initial-function* nil)

(defun listener-initial-display-function (frame pane)
  (declare (ignore frame pane))
  (when *listener-initial-function*
    (funcall-in-listener
     (lambda ()
       (funcall *listener-initial-function*)
       (fresh-line)))))

;;; Listener view
;;;
;;; FIXME: this TEXTUAL-VIEW thing is a lie: we can draw graphics.
;;; However, all the various presentation methods around the world are
;;; specialized on textual view, and it sucks to have to reimplement
;;; them all.
(defclass listener-view (textual-view) ())

(defclass listener-pointer-documentation-view 
    (listener-view pointer-documentation-view)
  ())

(defparameter +listener-view+ (make-instance 'listener-view))
(defparameter +listener-pointer-documentation-view+
  (make-instance 'listener-pointer-documentation-view))

(define-presentation-method present :around
  ((object sequence) (type sequence) stream (view listener-view)
   &key acceptably for-context-type)
  (present object 'expression :stream stream :view view
           :acceptably acceptably :for-context-type for-context-type))

(define-presentation-method accept :around
  ((type sequence) stream (view listener-view) &key default default-type)
  (let* ((token (read-token stream))
         (result (handler-case (read-from-string token)
                   (error (c)
                     (declare (ignore c))
                     (simple-parse-error 
                      "Error parsing ~S for presentation type ~S"
                      token type)))))
    (if (presentation-typep result type)
        (values result type)
        (input-not-of-required-type result type))))

;;; Listener application frame
(define-application-frame listener (standard-application-frame
                                    command-history-mixin)
    ((system-command-reader :accessor system-command-reader
			    :initarg :system-command-reader
			    :initform t))
  (:panes (interactor :interactor :scroll-bars t
                      :display-function #'listener-initial-display-function
                      :display-time t)
          (doc :pointer-documentation)
          (wholine (make-pane 'wholine-pane
                     :display-function 'display-wholine :scroll-bars nil
                     :display-time :command-loop :end-of-line-action :allow)))
  (:top-level (default-frame-top-level :prompt 'print-listener-prompt))
  (:command-table (listener
                   :inherit-from (application-commands lisp-commands filesystem-commands show-commands)
                   :menu (("Application" :menu application-commands)
                          ("Lisp"        :menu lisp-commands)
                          ("Filesystem"  :menu filesystem-commands)
                          ("Show"        :menu show-commands))))
  (:disabled-commands com-pop-directory com-drop-directory com-swap-directory)
  (:menu-bar t)
  (:layouts (default
	      (vertically ()
                interactor
                doc
                wholine))))

;;; Lisp listener command loop

;; Set this to true if you want the listener to bind *debug-io* to the
;; listener window.
(defparameter *listener-use-debug-io* #+hefner t #-hefner nil)

(defmethod run-frame-top-level ((frame listener) &key listener-funcall &allow-other-keys)
  (let ((*debug-io* (if *listener-use-debug-io*
                        (get-frame-pane frame 'interactor)
			*debug-io*))
	;; Borrowed from OpenMCL.
	;; from CLtL2, table 22-7:
        (*listener-initial-function* listener-funcall)
	(*package* *package*)
	(*print-array* *print-array*)
	(*print-base* *print-base*)
	(*print-case* *print-case*)
	(*print-circle* *print-circle*)
	(*print-escape* *print-escape*)
	(*print-gensym* *print-gensym*)
	(*print-length* *print-length*)
	(*print-level* *print-level*)
	(*print-lines* *print-lines*)
	(*print-miser-width* *print-miser-width*)
	(*print-pprint-dispatch* *print-pprint-dispatch*)
	(*print-pretty* *print-pretty*)
	(*print-radix* *print-radix*)
	(*print-readably* *print-readably*)
	(*print-right-margin* *print-right-margin*)
	(*read-base* *read-base*)
	(*read-default-float-format* *read-default-float-format*)
	(*read-eval* *read-eval*)
	(*read-suppress* *read-suppress*)
	(*readtable* *readtable*))
    (setf (stream-default-view (get-frame-pane frame 'interactor))
          +listener-view+)
    (setf (stream-default-view (get-frame-pane frame 'doc))
          +listener-pointer-documentation-view+)
    (loop while 
      (catch 'return-to-listener
	(restart-case (call-next-method)
	  (return-to-listener ()
	    :report "Return to listener."
	    (throw 'return-to-listener t)))))))

;; Oops. As we've ditched our custom toplevel, we now have to duplicate all
;; this setup work to implement one little trick.
(defun funcall-in-listener (fn)
  (let* ((frame *application-frame*)
         (*standard-input*  (or (frame-standard-input frame)
                                *standard-input*))
         (*standard-output* (or (frame-standard-output frame)
                                *standard-output*))
         (query-io  (frame-query-io frame))
         (*query-io* (or query-io *query-io*))
         (*pointer-documentation-output* (frame-pointer-documentation-output frame))
         (interactorp (typep *query-io* 'interactor-pane)))
    ;; FIXME - Something strange is happening which causes the initial command
    ;; prompt to be indented incorrectly after performing this output. Various
    ;; things like as calling TERPRI, manually moving the cursor, and closing
    ;; the open output record, don't seem to help.
    (with-room-for-graphics (*standard-output* :first-quadrant nil
                                               :move-cursor t)
      (funcall fn)
      (stream-close-text-output-record *standard-output*)
      (fresh-line))))      

(defparameter *form-opening-characters*
  '(#\( #\) #\[ #\] #\# #\; #\: #\' #\" #\* #\, #\` #\- 
    #\+ #\/ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defmethod read-frame-command ((frame listener) &key (stream *standard-input*))  
  "Specialized for the listener, read a lisp form to eval, or a command."
  (if (system-command-reader frame)
      (multiple-value-bind (object type)
	  (accept 'command-or-form :stream stream :prompt nil)
	(if (presentation-subtypep type 'command)
	    object
	    `(com-eval ,object)))
      (let* ((command-table (find-command-table 'listener))
             (*accelerator-gestures* (climi::compute-inherited-keystrokes command-table))
            object type)
        (flet ((sensitizer (stream cont)
                 (case type
                   ((command) (with-output-as-presentation 
                                  (stream object type :single-box t)
                                (funcall cont)))
                   ((form) (with-output-as-presentation
                               (stream object 'command :single-box t)
                             (with-output-as-presentation
                                 (stream (cadr object) 
                                         (presentation-type-of (cadr object))
                                         :single-box t)
                               (funcall cont))))
                   (t (funcall cont)))))
          (handler-case
              ;; Body
              (with-input-editing 
                  (stream :input-sensitizer #'sensitizer)
                (let ((c (read-gesture :stream stream :peek-p t)))
                  (setf object
                        (if (member c *form-opening-characters*)
                            (prog2
                                (when (char= c #\,)
                                  ;; lispm behavior 
                                  (read-gesture :stream stream))
                                (list 'com-eval (accept 'form :stream stream :prompt nil))
                              (setf type 'form))
                            (prog1
                                (accept '(command :command-table listener)  :stream stream
                                        :prompt nil)
                              (setf type 'command))))))
            ;; Handlers
            ((or simple-parse-error input-not-of-required-type) (c)
              (beep)
             (fresh-line *query-io*)
             (princ c *query-io*)
             (terpri *query-io*)
             nil)
            (accelerator-gesture (c)
              (let ((command (lookup-keystroke-command-item (accelerator-gesture-event c)
                                                            command-table)))              
                (setf ;type 'command
                 object (if (partial-command-p command)
                            (funcall *partial-command-parser*
                                     command-table stream command
                                     (position *unsupplied-argument-marker* command))
                            command))))))
	object)))

(defmethod read-frame-command :around ((frame listener)
				       &key (stream *standard-input*))
  "Read a command or form, taking care to manage the input context
   and whatever else need be done."
  (multiple-value-bind (x y)  (stream-cursor-position stream)    
    (with-input-context ('command) (object object-type)
        (call-next-method)
      (command
       ;; Kludge the cursor position - Goatee will have moved it all around
       (setf (stream-cursor-position stream) (values x y))
       (present object object-type
                :view (stream-default-view stream)
                :stream stream :single-box t)
       object))))

(defun print-listener-prompt (stream frame)
  (declare (ignore frame))
  (with-text-face (stream :italic)
    (print-package-name stream)
    (princ "> " stream)))

(defmethod frame-standard-output ((frame listener))
  (get-frame-pane frame 'interactor))

(defun run-listener (&key (system-command-reader nil)
                          (new-process nil)
                          (width 760)
                          (height 550)
                          (process-name "Listener")
                          (eval nil))
  (flet ((run ()
           (let ((frame (make-application-frame 
                         'listener
                         :width width :height height
                         :system-command-reader system-command-reader)))
             (run-frame-top-level 
              frame :listener-funcall (cond ((null eval) nil)
                                            ((functionp eval) eval)
                                            (t (lambda () (eval eval))))))))
    (if new-process
        (clim-sys:make-process #'run :name process-name)
        (run))))
