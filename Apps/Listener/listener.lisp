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

(defclass wholine-pane (application-pane) ())

(defmethod compose-space ((pane wholine-pane) &key width height)
  (declare (ignore width height))  
  (let ((h (+ 3 (text-style-height (medium-text-style pane) pane)))) ; magic padding
  (make-space-requirement :min-width 500 :width 768                  ; magic space requirements
                          :height h
                          :min-height h
                          :max-height h)))

(defvar *reconfiguring-wholine* nil)

(defmethod allocate-space ((pane wholine-pane) width height)
  (unless *reconfiguring-wholine*
    (let ((*reconfiguring-wholine* t))
      (call-next-method)
      (window-clear pane)
      (redisplay-frame-pane (pane-frame pane) pane))))



(defun print-package-name (stream)
  (let ((foo (package-name *package*)))
    (with-drawing-options (stream :ink +royalblue+)
      (format stream "~A" (reduce (lambda (&optional (a foo) (b foo))
                                    (if (< (length a) (length b)) a b))
                                  (package-nicknames *package*))))))

(defun frob-pathname (pathname)
  (namestring (truename pathname)))

(defun display-wholine (frame pane)
  (declare (ignore frame))
  (let* ((*standard-output* pane)
         (username (or #+cmu (cdr (assoc :user ext:*environment-list*))
		       #-cmu (getenv "USER")
                       "luser"))  ; sorry..
         (sitename (machine-instance))
         (memusage #+cmu (lisp::dynamic-usage)
                   #+sbcl  (sb-kernel:dynamic-usage)
                   #+lispworks (getf (system:room-values) :total-allocated)
		   #+openmcl (+ (ccl::%usedbytes) (ccl::%freebytes))
                   #-(or cmu sbcl lispworks openmcl) 0))
    (with-text-family (T :serif)
      (formatting-table (T :x-spacing '(3 :character))
        (formatting-row (T)                        
          (macrolet ((cell ((align-x) &body body)                         
                       `(formatting-cell (T :align-x ,align-x) ,@body)))
            (cell (:left)   (format T "~A@~A" username sitename))
            (cell (:center)
              (format T "Package ")
              (print-package-name T))
            (cell (:center)
              (when (probe-file *default-pathname-defaults*)
                (with-output-as-presentation (T (truename *default-pathname-defaults*) 'pathname)
                  (format T "~A" (frob-pathname *default-pathname-defaults*))))
              (when *directory-stack*
                (with-output-as-presentation (T *directory-stack* 'directory-stack)
                  (format T "  (~D deep)" (length *directory-stack*)))))
          ;; Although the CLIM spec says the item formatter should try to fill
          ;; the available width, I can't get either the item or table formatters
          ;; to really do so such that the memory usage appears right justified.            
            (cell (:center)
              (when (numberp memusage)
                (present memusage 'lisp-memory-usage)))))))))

;; This is a (very simple) command history.
;; Should we move this into CLIM-INTERNALS ?
;; Possibly this should become something integrated with the presentation
;; histories which I have not played with.

(defclass command-history-mixin ()
  ((history :initform nil :accessor history)
   (history-length :initform 25 :initarg :history-length :accessor history-length)))

(defmethod execute-frame-command :after ((frame command-history-mixin) command)
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
  

;;; Listener application frame
(define-application-frame listener (standard-application-frame
                                    command-history-mixin)
    ((system-command-reader :accessor system-command-reader
			    :initarg :system-command-reader
			    :initform t))
  (:panes (interactor :interactor :scroll-bars T
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
    (loop while 
      (catch 'return-to-listener
	(restart-case (call-next-method)
	  (return-to-listener ()
	    :report "Return to listener."
	    (throw 'return-to-listener T)))))))

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

;; FIXME: Lisp forms are currently translated to invokations of COM-EVAL.
;; This works fine, but in the future (when there is a proper form reader),
;; you really might want to click a form and bring it as code into what you're 
;; typing, allowing you to try things inside-out from the listener.

(defmethod read-frame-command ((frame listener) &key (stream *standard-input*))  
  "Specialized for the listener, read a lisp form to eval, or a command."
  (if (system-command-reader frame)
      (multiple-value-bind (object type)
	  (accept 'command-or-form :stream stream :prompt nil)
	(if (presentation-subtypep type 'command)
	    object
	    `(com-eval ,object)))
      (let (object type)
	(handler-case 
	    (with-input-editing (stream :input-sensitizer
					(lambda (stream cont)
					  (if type
					      (with-output-as-presentation
						  (stream object type)
						(funcall cont))
					      (funcall cont))))
	      (let ((c (read-gesture :stream stream :peek-p t)))
		(setf object
		      (if (member c *form-opening-characters*)
			  (prog2
			      (when (char= c #\,)
				(read-gesture :stream stream)) ; lispm behavior 
                   #| ---> |# (list 'com-eval (accept 'form :stream stream :prompt nil))
			    (setf type 'command #|'form|# )) ; FIXME? 
			  (prog1
			      (accept '(command :command-table listener)  :stream stream
				      :prompt nil)
			    (setf type 'command))))))
	  ((or simple-parse-error input-not-of-required-type)  (c)
	    (beep)
	    (fresh-line *query-io*)
	    (princ c *query-io*)
	    (terpri *query-io*)
	    nil))
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
                  :stream stream)
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
                          (process-name "Listener")
                          (eval nil))
  (flet ((run ()
           (run-frame-top-level
            (make-application-frame 'listener
                                    :system-command-reader system-command-reader)
            :listener-funcall (cond ((null eval) nil)
                                    ((functionp eval) eval)
                                    (t (lambda () (eval eval)))))))
    (if new-process
        (clim-sys:make-process #'run :name process-name)
        (run))))
