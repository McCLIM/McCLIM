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
  ((pasted-list :initform nil
                :accessor paste-demo-stream-pane/pasted-list)))

(clim:define-application-frame clipboard-demo ()
  ()
  (:panes (content (clim:make-pane 'paste-demo-stream-pane
                                   :display-function 'display-clipboard-demo
                                   :scroll-bars :both
                                   :incremental-redisplay nil))
          (interaction-pane :interactor))
  (:menu-bar clipboard-demo-menubar-table)
  (:layouts (default (clim:vertically ()
                       (clim:scrolling ()
                         content)
                       interaction-pane))))

(clim:define-presentation-to-command-translator copy-markup-text
    (markup-text climi::com-copy-to-clipboard clipboard-demo :menu t)
    (obj)
  (list (make-instance 'climi::clipboard-object :content obj :type 'markup-text)))

(defmethod clim:handle-event :around ((pane paste-demo-stream-pane) (event clim-extensions:clipboard-send-event))
  (setf (paste-demo-stream-pane/pasted-list pane)
        (append (paste-demo-stream-pane/pasted-list pane)
                (list (list (clim-extensions:clipboard-event-content event)
                            (clim-extensions:clipboard-event-type event)))))
  (clim:redisplay-frame-pane (clim:pane-frame pane) pane))

(defun display-clipboard-demo (frame stream)
  (format stream "Select text using shift and the left mouse button~%")
  (format stream "You can also use the command \"Copy to clipboard\" to select a presentation.~%~%")
  (let ((obj (make-instance 'markup-text :text "This text can be copied in HTML form")))
    (clim:stream-present stream obj (clim:presentation-type-of obj)))
  (format stream "~%~%Styled text can also be copied: ")
  (clim:with-text-face (stream :bold)
    (format stream "bold "))
  (clim:with-text-face (stream :italic)
    (format stream "italic "))
  (clim:with-text-face (stream '(:bold :italic))
    (format stream "bold/italic."))
  (format stream "~%~%You can paste the clipboard into editor fields using Shift-Insert,~%")
  (format stream "or you can Shift-click the middle mouse button to paste from the selection.~%~%")
  (format stream "      ")
  (clim:surrounding-output-with-border (stream)
    (clim:with-output-as-gadget (stream)
      (clim:make-pane 'clim:text-field-pane :width 400)))
  (format stream "~%~%")
  (alexandria:when-let ((pasted-list (paste-demo-stream-pane/pasted-list (clim:find-pane-named frame 'content))))
    (loop
      for (content type) in pasted-list
      do (case type
           (:string (format stream "~a~%" content))
           (:html (format stream "~a~%" content))
           (:image (clim:with-room-for-graphics (stream)
                     (clim:draw-pattern* stream content 0 0)))))))

(defun clipboard-demo ()
  (let ((frame (clim:make-application-frame 'clipboard-demo :width 600 :height 800)))
    (clim:run-frame-top-level frame)))

(define-clipboard-demo-command (com-paste-text :name "Paste Text")
    ()
  (clim-extensions:request-clipboard-content (clim:find-pane-named clim:*application-frame* 'content) :string))

(define-clipboard-demo-command (com-paste-html :name "Paste HTML")
    ()
  (clim-extensions:request-clipboard-content (clim:find-pane-named clim:*application-frame* 'content) :html))

(define-clipboard-demo-command (com-paste-image :name "Paste Image")
    ()
  (clim-extensions:request-clipboard-content (clim:find-pane-named clim:*application-frame* 'content) :image))

(clim:make-command-table 'clipboard-demo-menubar-table
                         :errorp nil
                         :menu '(("Edit" :menu clipboard-demo-edit-table)))

(clim:make-command-table 'clipboard-demo-edit-table
                         :errorp nil
                         :menu '(("Copy Selection to Clipboard" :command climi::com-copy-text-to-clipboard)
                                 ("Paste Text" :command com-paste-text)
                                 ("Paste HTML" :command com-paste-html)
                                 ("Paste Image" :command com-paste-image)))
