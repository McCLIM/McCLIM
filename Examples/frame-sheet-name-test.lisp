;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; This program demonstrates how to modify the pretty name of an
;;; application frame (and the pretty name of the corresponding
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

(define-application-frame frame-sheet-name-test ()
  ()
  (:menu-bar nil)
  (:pane
   (let* ((frame *application-frame*)
          (sheet (frame-top-level-sheet frame)))
     (spacing (:thickness 10)
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
                                                new-value)))))))))
  (:default-initargs
   :pretty-name "Frame and Sheet Name Test"))

(defmethod clime:note-frame-pretty-name-changed ((frame-manager t)
                                                 (frame frame-sheet-name-test)
                                                 (new-name t))
  (alexandria:when-let* ((pane  (find-pane-named frame 'sheet-pretty-name))
                         (sheet (frame-top-level-sheet frame)))
    (setf (gadget-value pane :invoke-callback nil)
          (clime:sheet-pretty-name sheet))))
