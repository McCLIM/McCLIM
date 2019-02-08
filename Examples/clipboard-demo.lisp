(defpackage :clipboard-demo
  (:use :cl)
  (:export :clipboard-demo))

(in-package :clipboard-demo)

(defclass html-content ()
  ())

(defmethod climi::convert-clipboard-object ((object html-content) (type (eql :html)))
  "Test <b>bold</b>")

(defmethod climi::convert-clipboard-object ((object html-content) (type (eql :string)))
  "Test bold")

(clim:define-presentation-method clim:present (obj (type html-content) stream view &key)
  (clim:surrounding-output-with-border (stream)
    (format stream "Foo ")
    (clim:with-text-face (stream :bold)
      (format stream "bold"))))

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

(clim:define-presentation-translator html-to-clipboard (html-content climi::clipboard-object clipboard-demo)
    (object)
  (make-instance 'climi::clipboard-object :content object))

(defun display-clipboard-demo (frame stream)
  (format stream "Plain text~%")
  (let ((obj (make-instance 'html-content)))
    (clim:stream-present stream obj (clim:presentation-type-of obj)))
  (format stream "~%")
  (loop
    for string in (clipboard-demo/string-list frame)
    do (format stream "~a~%" string)))

(defun clipboard-demo ()
  (let ((frame (clim:make-application-frame 'clipboard-demo :width 600 :height 800)))
    (clim:run-frame-top-level frame)))

(define-clipboard-demo-command (cmd-copy-text :name "Paste text")
    ()
  (climi::request-clipboard-content (clim:find-pane-named clim:*application-frame* 'content) :string))

(define-clipboard-demo-command (cmd-copy-html :name "Paste HTML")
    ()
  (climi::request-clipboard-content (clim:find-pane-named clim:*application-frame* 'content) :html))
