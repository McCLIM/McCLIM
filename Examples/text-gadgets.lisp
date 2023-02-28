(defpackage #:clim-demo.text-gadgets
  (:use #:clim-lisp #:clim))
(in-package #:clim-demo.text-gadgets)

(defun value-changed (gadget value)
  (declare (ignore gadget))
  (with-application-frame (frame)
    (let ((pane (find-pane-named frame 'text-field-2)))
      (when pane
        (setf (gadget-value pane) value)))))

(defun gadget-activated (gadget)
  (with-application-frame (frame)
    (let ((pane (find-pane-named frame 'text-field-3)))
      (when pane
        (setf (gadget-value pane)
              (gadget-value gadget))))))

(defun editor-value ()
  (with-output-to-string (*standard-output*)
    (mapc (lambda (s) (format t "~a~%" s))
          (lorem-ipsum:paragraphs 8))))

(define-application-frame my-frame ()
  ()
  (:panes (text-field-a :text-field
                        :value-changed-callback 'value-changed
                        :activate-callback 'gadget-activated
                        :text-style (make-text-style nil nil :huge))
          (text-field-2 :text-field :background +light-yellow+ :editable-p nil
                                    :text-style (make-text-style nil nil :huge))
          (text-field-3 :text-field :background +light-slate-grey+ :editable-p nil
                                    :text-style (make-text-style nil nil :huge))
          (text-editor :text-editor :nlines 40 :ncolumns 80
                                    :value (editor-value)))
  (:reinitialize-frames t)
  (:layouts
   (b (horizontally ()
        (vertically ()
          text-field-a
          text-field-2
          text-field-3
          +fill+)
        text-editor))))

#+ (or) (find-application-frame 'my-frame)
