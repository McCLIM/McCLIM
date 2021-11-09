;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2003 by Tim Moore <moore@bricoworks.com>
;;;  (c) Copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;  (c) Copyright 2021 by Daniel Kochmański <daniel.turtlewareeu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; notify-user
;;;
;;; See http://openmap.bbn.com/hypermail/clim/0028.html for example usage.
;;;
;;; TODO:
;;;   - associated-window argument?
;;;   - What is the correct return value from notify-user? We currently return
;;;     the name of the action given in the :exit-boxes argument.
;;;   - Invoke abort restart? Not necessary as it is with accepting-values,
;;;     but probably what "Classic CLIM" does.
;;;   - What are the default exit boxes? Just "Okay"? Okay and cancel?
;;;   - Reimplement using accepting-values, if accepting-values is ever
;;;     improved to produce comparable dialogs.
;;;   - Should the user really be able to close the window from the WM?
;;;
(in-package #:clim-internals)

(defmethod notify-user (frame message &rest args)
  (apply #'frame-manager-notify-user
         (if frame (frame-manager frame) (find-frame-manager))
         message
         :frame frame
         args))

(define-application-frame generic-notify-user-frame ()
  ((message-string :initarg :message-string)
   (exit-boxes :initarg :exit-boxes)
   (title :initarg :title)
   (style :initarg :style)
   (text-style :initarg :text-style)
   (return-value :initarg nil :initform :abort))
  (:pane (generate-notify-user-dialog *application-frame*)))

(defun generate-notify-user-dialog (frame)
  (with-slots (message-string exit-boxes text-style) frame
  (vertically ()
    (spacing (:thickness 6)
      (make-pane 'label-pane :label (or message-string "I'm speechless.") :text-style text-style))
    (spacing (:thickness 4)
      (make-pane 'hbox-pane :contents (cons '+fill+ (generate-exit-box-buttons exit-boxes)))))))

(defun generate-exit-box-buttons (specs)
  (mapcar
   (lambda (spec)
     (destructuring-bind (action string &rest args) spec
       (spacing (:thickness 2)
         (apply #'make-pane
                'push-button
                :label string
                :text-style (make-text-style :sans-serif :roman :small) ; XXX
                :activate-callback
                (lambda (gadget)
                  (declare (ignore gadget))
                  ;; This is fboundp business is weird, and only implied by a
                  ;; random message on the old CLIM list. Does the user function
                  ;; take arguments?
                  (when (or (typep action 'function) (fboundp action))
                    (funcall action))
                  (setf (slot-value *application-frame* 'return-value) action)
                  ;; This doesn't work:
                  #+NIL
                  (when (eql action :abort)
                    (and (find-restart 'abort)
                         (invoke-restart 'abort)))
                  (frame-exit *application-frame*))
                args))))
   specs))

(defmethod frame-manager-notify-user
    (frame-manager message-string &key frame associated-window
                   (title "")
                   documentation
                   (exit-boxes '((:exit "OK")))
                   ; The 'name' arg is in the spec but absent from the Lispworks
                   ; manual, and I can't imagine what it would do differently
                   ; than 'title'.
                   name
                   style
                   (text-style (make-text-style :sans-serif :roman :small)))
  (declare (ignore associated-window documentation))
  ;; Keywords from notify-user:
  ;; associated-window title documentation exit-boxes name style text-style
  (let ((frame (make-application-frame 'generic-notify-user-frame
                                       :calling-frame frame
                                       :pretty-name title
                                       :message-string message-string
                                       :frame-manager frame-manager
                                       :exit-boxes exit-boxes
                                       :title (or name title)
                                       :style style
                                       :text-style text-style)))
    (run-frame-top-level frame)
    (slot-value frame 'return-value)))
