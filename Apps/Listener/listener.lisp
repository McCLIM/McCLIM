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

(define-presentation-type listener-current-package () :inherit-from 'package)

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
              (with-output-as-presentation (t *package* 'listener-current-package)
                (print-package-name t)))
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
  (declare (ignorable default default-type))
  ;; oh, my word.  although TYPE here might look like it's bound to
  ;; the presentation type itself, in fact it is bound to the
  ;; parameter of the SEQUENCE presentation type.  We need the
  ;; presentation type itself, so we reconstruct it.
  (let ((ptype (list 'sequence type)))
    (let* ((token (read-token stream))
	   (result (handler-case (read-from-string token)
		     (error (c)
		       (declare (ignore c))
		       (simple-parse-error 
			"Error parsing ~S for presentation type ~S"
			token ptype)))))
      (if (presentation-typep result ptype)
	  (values result ptype)
	  (input-not-of-required-type result ptype)))))

;;; Listener interactor stream.  If only STREAM-PRESENT were
;;; specializable on the VIEW argument, this wouldn't be necessary.
;;; However, it isn't, so we have to play this game.  We currently
;;; only use this to get single-box presentation highlighting.

(defclass listener-interactor-pane (interactor-pane) ())

(defmethod stream-present :around 
    ((stream listener-interactor-pane) object type
     &rest args &key (single-box nil sbp) &allow-other-keys)
  (apply #'call-next-method stream object type :single-box t args)
  ;; we would do this, but CLIM:PRESENT calls STREAM-PRESENT with all
  ;; the keyword arguments explicitly.  *sigh*.
  #+nil 
  (if sbp
      (call-next-method)
      (apply #'call-next-method stream object type :single-box t args)))

;;; Listener application frame
(define-application-frame listener (standard-application-frame)
    ((system-command-reader :accessor system-command-reader
			    :initarg :system-command-reader
			    :initform t))
  (:panes (interactor-container
           (make-clim-stream-pane
            :type 'listener-interactor-pane
            :name 'interactor :scroll-bars t))
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
                interactor-container
                doc
                wholine))))

;;; Package selection popup

(define-listener-command (com-choose-package)
    ()
  (let ((new-package (menu-choose (sort (mapcar (lambda (package) (cons (package-name package)
                                                                        package))
                                                (list-all-packages))
                                        #'string<
                                        :key #'car)
                                  :label "Choose Package")))
    (when new-package
      (setf *package* new-package))))

(define-presentation-to-command-translator choose-package-translator
    (listener-current-package com-choose-package listener
     :echo nil
     :priority 100  ; These presentations appear in exactly one context, so give this a high priority.
     :documentation ((object stream)
                     (declare (ignore object))
                     (format stream "Choose package")))
  (current-package)
  nil)

;;; Lisp listener command loop

(defmethod read-frame-command ((frame listener) &key (stream *standard-input*))  
  "Specialized for the listener, read a lisp form to eval, or a command."
  (multiple-value-bind (object type)
      (accept 'command-or-form :stream stream :prompt nil)
    (format *trace-output* "~&object=~W~%" object)
    (if (presentation-subtypep type 'command)
        object
        `(com-eval ,object))))

(defun print-listener-prompt (stream frame)
  (declare (ignore frame))
  (with-text-face (stream :italic)
    (with-output-as-presentation (stream *package* 'package :single-box t)
      (print-package-name stream))
    (princ "> " stream)))

(defmethod frame-standard-output ((frame listener))
  (get-frame-pane frame 'interactor))

(defun run-listener (&key (new-process nil)
                          (width 760)
                          (height 550)
                          (process-name "Listener"))
  (flet ((run ()
           (let ((frame (make-application-frame 
                         'listener
                         :width width :height height)))
             (run-frame-top-level frame))))
    (if new-process
        (clim-sys:make-process #'run :name process-name)
        (run))))
