;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; This program demonstrates how to modify the pretty name and icon
;;; of an application frame (and the pretty name of the corresponding
;;; top-level sheet). Depending on the backend and windowing
;;; environment, these changes may or may not be immediately picked up
;;; and displayed by the window manager and/or graphical environment.

(cl:defpackage #:clim-demo.names-and-icons
  (:use
   #:clim-lisp
   #:clim)
  (:export
   #:frame-sheet-name-test))
(cl:in-package #:clim-demo.names-and-icons)

(define-presentation-type icon ()
  :inherit-from t)

(define-presentation-method present ((object t)
                                     (type   icon)
                                     (stream extended-output-stream)
                                     (view   t)
                                     &key)
  (draw-design stream object))

(defvar *glider-icon*
  (make-pattern-from-bitmap-file
   (merge-pathnames #p"images/glider.png"
                    (asdf:system-source-directory :clim-examples))))

(defun display-icons (frame pane)
  (declare (ignore frame))
  (formatting-table (pane :x-spacing 16)
    (formatting-row (pane)
      (formatting-cell (pane)
        (with-output-as-presentation (pane nil 'icon)
          (with-drawing-options (pane :text-face :italic)
            (write-string "no icon" pane))))
      (formatting-cell (pane)
        (present climi::*default-icon-large* 'icon :stream pane))
      (formatting-cell (pane)
        (present climi::*default-icon-small* 'icon :stream pane))
      (formatting-cell (pane)
        (present *glider-icon* 'icon :stream pane)))))

(define-application-frame frame-sheet-name-test ()
    ()
  (:menu-bar nil)
  (:pane
   (let* ((frame *application-frame*)
          (sheet (frame-top-level-sheet frame)))
     (spacing (:thickness 10)
       (vertically ()
         (labelling (:label "Icon")
           (spacing (:thickness 8)
             (make-pane :application :display-function 'display-icons)))
         (labelling (:label "Names")
           (tabling (:min-width 600 :spacing 8)
             (list (labelling (:label " ")) ; work around problems with empty labels for now
                   (labelling (:label "Name"))
                   (labelling (:label "Pretty Name")))
             (list (labelling (:label "Sheet"))
                   (make-pane :label :label (prin1-to-string (clime:sheet-name sheet)))
                   (outlining (:thickness 4 :background +white+)
                     (make-pane :text-field :name 'sheet-pretty-name
                                            :value (clime:sheet-pretty-name sheet)
                                            :value-changed-callback
                                            (lambda (gadget new-value)
                                              (declare (ignore gadget))
                                              (setf (clime:sheet-pretty-name sheet)
                                                    new-value)))))
             (list (labelling (:label "Frame"))
                   (make-pane :label :label (prin1-to-string (frame-name frame)))
                   (outlining (:thickness 4 :background +white+)
                     (make-pane :text-field :value (frame-pretty-name frame)
                                            :value-changed-callback
                                            (lambda (gadget new-value)
                                              (declare (ignore gadget))
                                              (setf (frame-pretty-name frame)
                                                    new-value)))))))))))
  (:default-initargs
   :pretty-name "Frame and Sheet Name Test"))

(define-command (com-set-icon :command-table frame-sheet-name-test)
    ((icon 'icon :gesture :select))
  (setf (clime:frame-icon *application-frame*) icon))

(defmethod clime:note-frame-pretty-name-changed ((frame-manager t)
                                                 (frame frame-sheet-name-test)
                                                 (new-name t))
  (alexandria:when-let* ((pane  (find-pane-named frame 'sheet-pretty-name))
                         (sheet (frame-top-level-sheet frame)))
    (setf (gadget-value pane :invoke-callback nil)
          (clime:sheet-pretty-name sheet))))
