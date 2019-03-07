(defpackage :clipboard-demo
  (:use :cl)
  (:export :clipboard-demo))

(in-package :clipboard-demo)

(defclass markup-text ()
  ((text :initarg :text
         :reader markup-text/text)))

(clim:define-presentation-method clim:present (obj (type markup-text) stream view &key)
  (clim:with-drawing-options (stream :ink clim:+blue+)
    (format stream "~a" (markup-text/text obj))))

(clim:define-presentation-method clim-extensions:convert-clipboard-content
    (obj (type markup-text) (output-type (eql :string)) check-only)
  (markup-text/text obj))

(clim:define-presentation-method clim-extensions:convert-clipboard-content
    (obj (type markup-text) (output-type (eql :html)) check-only)
  (format nil "Highlighted content: <b>~a</b>" (markup-text/text obj)))

(defclass paste-demo-stream-pane (clim:application-pane)
  ())

(defclass paste-output-stream-pane (clim:application-pane)
  ())

(clim:define-application-frame clipboard-demo ()
  ((pasted-list :initform nil
                :accessor clipboard-demo/pasted-list))
  (:panes (content (clim:make-pane 'paste-demo-stream-pane
                                   :display-function 'display-clipboard-demo
                                   :scroll-bars :both
                                   :incremental-redisplay nil
                                   :end-of-line-action :wrap*))
          (paste-output (clim:make-pane 'paste-output-stream-pane
                                        :display-function 'display-paste-output
                                        :incremental-redisplay nil
                                        :end-of-line-action :wrap*))
          (interaction-pane :interactor))
  (:menu-bar clipboard-demo-menubar-table)
  (:geometry :width 1000 :height 600)
  (:layouts (default (clim:vertically ()
                       (2/3 (clim:horizontally ()
                              (clim:scrolling ()
                                content)
                              (clim:scrolling ()
                                paste-output)))
                       (1/3 interaction-pane)))))

(clim:define-presentation-to-command-translator copy-markup-text
    (markup-text climi::com-copy-to-clipboard clipboard-demo :menu t)
    (obj)
  (list (make-instance 'climi::clipboard-object :content obj :type 'markup-text)))

(defmethod clim:handle-event :around ((pane paste-output-stream-pane) (event clim-extensions:clipboard-send-event))
  (let ((frame (clim:pane-frame pane)))
    (setf (clipboard-demo/pasted-list frame)
          (append (clipboard-demo/pasted-list frame)
                  (list (list (clim-extensions:clipboard-event-content event)
                              (clim-extensions:clipboard-event-type event)))))
    (clim:redisplay-frame-pane frame pane)))

(defun display-clipboard-demo (frame stream)
  (declare (ignore frame))
  (format stream "Select text using shift and the left mouse button~%")
  (format stream "You can also use the command \"Copy to clipboard\" to select a presentation.~%~%")
  (let ((obj (make-instance 'markup-text :text "This text can be copied in HTML form")))
    (clim:stream-present stream obj (clim:presentation-type-of obj)))
  (format stream "~%~%You can paste the clipboard into editor fields using Shift-Insert,~%")
  (format stream "or you can Shift-click the middle mouse button to paste from the selection.~%~%")
  (format stream "      ")
  (clim:surrounding-output-with-border (stream)
    (clim:with-output-as-gadget (stream)
      (clim:make-pane 'clim:text-field-pane :width 400))))

(defun display-paste-output (frame stream)
  (let ((pasted-list (clipboard-demo/pasted-list frame)))
    (loop
      for (content type) in pasted-list
      do (case type
           (:string (format stream "~a~%" content))
           (:html (format stream "~a~%" content))
           (:image (clim:with-room-for-graphics (stream)
                     (clim:draw-pattern* stream content 0 0)))))))

(defun clipboard-demo ()
  (let ((frame (clim:make-application-frame 'clipboard-demo)))
    (clim:run-frame-top-level frame)))

(define-clipboard-demo-command (com-paste-text :name "Paste Text")
    ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'paste-output)))
    (clim-extensions:request-clipboard-content (clim:port pane) pane :string)))

(define-clipboard-demo-command (com-paste-html :name "Paste HTML")
    ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'paste-output)))
    (clim-extensions:request-clipboard-content (clim:port pane) pane :html)))

(define-clipboard-demo-command (com-paste-image :name "Paste Image")
    ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'paste-output)))
    (clim-extensions:request-clipboard-content (clim:port pane) pane :image)))

(define-clipboard-demo-command (com-clear-paste-pane :name "Clear Paste Pane")
    ()
  (setf (clipboard-demo/pasted-list clim:*application-frame*) nil))

(defclass clipboard-object ()
  ((content :initarg :content
            :reader clipboard-object/content)
   (type    :initarg :type
            :reader clipboard-object/type)))

(clim:define-presentation-translator presentation-clipboard-object
    (t clipboard-object clim:global-command-table :tester ((obj presentation)
                                                           (climi::supported-clipboard-types obj (clim:presentation-type presentation))))
    (obj presentation)
  (make-instance 'clipboard-object :content obj :type (clim:presentation-type presentation)))

(clim:define-command (com-copy-to-clipboard :command-table clim:global-command-table :name "Copy Presentation to Clipboard")
    ((obj clipboard-object :prompt "Object"))
  (let ((pane (clim:frame-top-level-sheet clim:*application-frame*)))
    (clime:copy-to-clipboard (clim:port pane) pane (clipboard-object/content obj)
                             :presentation-type (clipboard-object/type obj))))

(clim:define-presentation-translator string-to-clipboard (string clipboard-object clim:global-command-table)
    (object)
  (make-instance 'clipboard-object :content object :type 'string))

(clim:define-command (com-copy-text-to-clipboard :command-table clim:global-command-table :name "Copy Text to Clipboard")
    ()
  (multiple-value-bind (object type)
      (clim-extensions:local-selection-content (clim:port *standard-output*))
    (if object
        (clim-extensions:copy-to-clipboard (clim:port *standard-output*) *standard-output* object :presentation-type type)
        (invoke-restart 'abort))))

(clim:make-command-table 'clipboard-demo-menubar-table
                         :errorp nil
                         :menu '(("Edit" :menu clipboard-demo-edit-table)))

(clim:make-command-table 'clipboard-demo-edit-table
                         :errorp nil
                         :menu '(("Copy Selection to Clipboard" :command com-copy-text-to-clipboard)
                                 ("Copy Presentation to Clipboard" :command com-copy-to-clipboard)
                                 ("Paste Text" :command com-paste-text)
                                 ("Paste HTML" :command com-paste-html)
                                 ("Paste Image" :command com-paste-image)
                                 ("Clear Paste Pane" :command com-clear-paste-pane)))
