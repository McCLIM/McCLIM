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
; TODO: Handle resizing in a less ghetto fashion.

(defclass wholine-pane (application-pane))

(defmethod compose-space ((pane wholine-pane) &key width height)
  (declare (ignore width height))  
  (let ((h (+ 2 (text-style-height (medium-text-style pane) pane)))) ; magic padding
  (make-space-requirement :min-width 500 :width 768
                          :height h
                          :min-height h
                          :max-height h)))

(defmethod handle-repaint ((pane wholine-pane) region)
  (declare (ignore region))
  (window-clear pane)
  (redisplay-frame-pane (pane-frame pane) pane))

(defun print-package-name (stream)
  (let ((foo (package-name *package*)))
    (with-drawing-options (stream :ink +royalblue+)
      (format stream "~A" (reduce #'(lambda (&optional (a foo) (b foo))
                                      (if (< (length a) (length b)) a b))
                                  (package-nicknames *package*))))))

(defun frob-pathname (pathname)
  (namestring (truename pathname)))




;; Ought to try using table formatting here, see how well it works
(defun really-display-wholine (frame pane)
  (declare (ignore frame))
  (let* ((*standard-output* pane)
         (username (or #+cmu (cdr (assoc :user ext:*environment-list*))
                       #+sbcl (sb-ext:posix-getenv "USER")
                       "luser"))  ; sorry..
         (sitename (machine-instance))
         (memusage #+cmu (common-lisp::dynamic-usage)
                   #+sbcl  (sb-kernel:dynamic-usage)
                   #-(or cmu sbcl) nil)
         (memstring (if (numberp memusage)
                        (format nil "~,1F MB" (/ memusage (expt 1024 2)))
                      "")))
    (with-text-family (T :serif)
      (stream-increment-cursor-position pane 7 nil)
      (format T "~A@~A" username sitename)
      (stream-increment-cursor-position pane 33 nil)
      (format T "Package ")
      (print-package-name T)
      (stream-increment-cursor-position pane 60 nil)
      (with-output-as-presentation (T (truename *default-pathname-defaults*) 'pathname)
        (format T "~A" (frob-pathname *default-pathname-defaults*)))      
      (when *directory-stack*
        (with-output-as-presentation (T *directory-stack* 'directory-stack)
          (stream-increment-cursor-position pane 16 0)                                
          (format T "(~D deep)" (length *directory-stack*))))
      
      (setf (stream-cursor-position pane)
            (values (- (bounding-rectangle-width pane) (text-size pane memstring)) 0))
      (princ memstring pane))))

;; Why am I doing this?
;; The display function get stored in the application frame. If I happen to
;; change and recompile this display function, it doesn't take effect until
;; I restart the app, and that's no good at all.

(defun display-wholine (frame pane)
  (really-display-wholine frame pane))
    


;;; Listener application frame
(define-application-frame listener ()
    ()
  (:panes (interactor :interactor :scroll-bars T)
          (doc :pointer-documentation)
          (wholine (make-pane 'wholine-pane  ;; :min-height 18 :max-height 18
                     :display-function #'display-wholine :scroll-bars nil
                     :display-time :command-loop :end-of-line-action :allow)))
  (:top-level (listener-top-level))
  (:command-table (listener :inherit-from (dev-commands)))
  (:layouts (default
	      (vertically ()
                interactor
                doc
                wholine))))


;;; Lisp listener command loop

;; * Moore says that ACCEPT '(OR COMMAND FORM) is supposed to work, so I need
;;   to experiment with that at somepoint. that would save me from having to 
;;   reimplement half of accept as part of this toplevel.

; Now that I have LISTENER-TOP-LEVEL, should I move the binding of the restart
; inside there? Or should I move this inside McCLIM?
(defmethod run-frame-top-level ((frame listener) &key &allow-other-keys)
  (let ((*debug-io* (get-frame-pane frame 'interactor)))
    (loop while 
      (catch 'return-to-listener
	(restart-case (call-next-method)
	  (return-to-listener ()
	    :report "Return to listener."
	    (throw 'return-to-listener T)))))))

(defparameter *form-opening-characters*
  '(#\( #\) #\[ #\] #\# #\; #\: #\' #\" #\* #\, #\` #\- 
    #\+ #\/ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

;; FIXME: Lisp forms are currently translated to invokations of COM-EVAL.
;; This works fine, but in the future (when there is a proper form reader),
;; you really might want to click a form and bring it as code into what you're 
;; typing, allowing you to try things inside-out from the listener.

(defmethod read-frame-command ((frame listener) &key (stream *standard-input*))  
  "Specialized for the listener, read a lisp form to eval, or a command."
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
                          (read-gesture :stream stream))  ; lispm behavior                          
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
  object))

(defun listener-read (frame stream)
  "Read a command or form, taking care to manage the input context
   and whatever else need be done."
  (multiple-value-bind (x y)  (stream-cursor-position stream)    
    (with-input-context ('command) (object object-type)
            (read-frame-command frame :stream stream)
        (command
         ;; Kludge the cursor position - Goatee will have moved it all around
         (setf (stream-cursor-position stream) (values x y))
         (present object object-type
                  :view (stream-default-view stream)
                  :stream stream)
         object))))


(defun update-panes (frame)
  "Updates any panes that require redisplay."
  (map-over-sheets #'(lambda (pane)
                       (multiple-value-bind (redisplayp clearp)
                           (pane-needs-redisplay pane)
                         (when redisplayp
                           (when (and clearp
                                      (or (not (climi::pane-incremental-redisplay
                                                pane))
                                          (not climi::*enable-updating-output*)))
                             (window-clear pane))
                           (redisplay-frame-pane frame pane)
                           (unless (eq redisplayp :command-loop)
                             (setf (pane-needs-redisplay pane) nil)))))
                   (frame-top-level-sheet frame)))

(defun print-listener-prompt (stream)
  (with-text-face (stream :italic)
    (print-package-name stream)
    (princ "> " stream)))

(defun print-banner (stream)
  (let ((*standard-output* stream))
    (with-text-style (T (make-text-style :serif :bold nil))
      (format T "McCLIM Listener, ~A ~A~%"
              (lisp-implementation-type)
              (lisp-implementation-version)))))

(defun listener-top-level
    (frame
     &key (command-parser 'command-line-command-parser)
	  (command-unparser 'command-line-command-unparser)
	  (partial-command-parser
	   'command-line-read-remaining-arguments-for-partial-command)
          &allow-other-keys)
    ;; Can't print a banner here without moving the return-to-listener restart.
    ;(print-banner (frame-standard-output frame))
    (loop	  
      (let ((*standard-input* (frame-standard-input frame))
            (*standard-output* (frame-standard-output frame))
            (*query-io* (frame-query-io frame))
            (*pointer-documentation-output* (frame-pointer-documentation-output
                                             frame))
            ;; during development, don't alter *error-output*
            ;; (*error-output* (frame-error-output frame))
            (*command-parser* command-parser)
            (*command-unparser* command-unparser)
            (*partial-command-parser* partial-command-parser)	  
            (interactor (get-frame-pane frame 'interactor)))       
        (update-panes frame)
        (print-listener-prompt interactor)
        (setf (cursor-visibility (stream-text-cursor *standard-input*)) :on)
        (let ((command (listener-read frame interactor)))
          (fresh-line)
          (cond ((partial-command-p command)
                 (format *query-io* "~&Argument ~D not supplied.~&"
                         (position *unsupplied-argument-marker* command)))
                (command (apply (command-name command)
                                (command-arguments command)))
                (T nil))
        (fresh-line)))))

(defmethod frame-standard-output ((frame listener))
  (get-frame-pane frame 'interactor))


(defun run-listener ()
   (clim:run-frame-top-level
    (clim:make-application-frame 'listener)))

(defun run-listener-process ()
  (clim-sys:make-process #'run-listener))

