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

(clim:define-presentation-method clim-internals::convert-clipboard-content
    (obj (type markup-text) (output-type (eql :string)) check-only)
  (markup-text/text obj))

(clim:define-presentation-method clim-internals::convert-clipboard-content
    (obj (type markup-text) (output-type (eql :html)) check-only)
  (format nil "Highlighted content: <b>~a</b>" (markup-text/text obj)))

(clim:define-application-frame clipboard-demo ()
  ((string-list :initform nil
                :accessor clipboard-demo/string-list))
  (:panes (content :application
                   :display-function 'display-clipboard-demo
                   :scroll-bars :both
                   :incremental-redisplay nil)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       content
                       interaction-pane))))

(defun display-clipboard-demo (frame stream)
  (format stream "Select text using shift and the left mouse button~%")
  (format stream "You can also use the command \"Copy to clipboard\" to select a presentation.~%~%")
  (let ((obj (make-instance 'markup-text :text "This text can be copied in HTML form")))
    (clim:stream-present stream obj (clim:presentation-type-of obj)))
  (format stream "~%~%You can paste the clipboard into editor fields using Shift-Insert,~%")
  (format stream "or you can Shift-click the middle mouse button the paste from the selection.~%~%")
  (when (clipboard-demo/string-list frame)
    (format stream "Pasted content:~%"))
  (loop
    for string in (clipboard-demo/string-list frame)
    do (format stream "~a~%" string)))

(defun clipboard-demo ()
  (let ((frame (clim:make-application-frame 'clipboard-demo :width 600 :height 800)))
    (clim:run-frame-top-level frame)))

(define-clipboard-demo-command (cmd-copy-text :name "Paste Text")
    ()
  (clim-extensions:request-clipboard-content (clim:find-pane-named clim:*application-frame* 'content) :string))

(define-clipboard-demo-command (cmd-copy-html :name "Paste HTML")
    ()
  (clim-extensions:request-clipboard-content (clim:find-pane-named clim:*application-frame* 'content) :html))
