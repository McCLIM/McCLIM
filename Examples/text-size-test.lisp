;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2006 David Lichteblau (david@lichteblau.com)
;;;  (c) copyright 2018, 2020 Jan Moringen (jmoringe@techfak.uni-bielefeld.de)
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Interactively demonstrates text size computations for different
;;; text styles.
;;;

(in-package #:clim-demo)

(defun make-callback (writer &optional transform)
  (let ((state (state *application-frame*)))
    (if transform
        (lambda (pane value)
          (declare (ignore pane))
          (funcall writer (funcall transform value) state))
        (lambda (pane value)
          (declare (ignore pane))
          (funcall writer value state)))))

(define-application-frame text-size-test ()
  ((%state :reader   state
           :initform (make-instance 'state
                                    :text-family :serif
                                    :text-face   nil
                                    :text-size   60
                                    :rectangle   :text-size)))
  (:menu-bar nil)
  (:panes
   (canvas (make-pane 'canvas :state      (state *application-frame*)
                              :min-width  700
                              :min-height 320))
   (text (make-pane 'text-editor
                    :nlines 4
                    :value  "ytmM"
                    :value-changed-callback (make-callback #'(setf text))))
   (family
    (with-radio-box (:value-changed-callback (make-callback
                                              #'(setf text-family)
                                              #'gadget-id))
      (toggle-button  :label "Fixed"      :id :fix)
      (radio-box-current-selection
       (toggle-button :label "Serif"      :id :serif))
      (toggle-button  :label "Sans Serif" :id :sans-serif)))
   (face
    (with-radio-box (:type :some-of
                     :value-changed-callback (make-callback
                                              #'(setf text-face)
                                              (lambda (value)
                                                (case (length value)
                                                  (0 nil)
                                                  (1 (gadget-id (first value)))
                                                  (2 '(:bold :italic))))))
      (toggle-button :label "Bold"   :id :bold)
      (toggle-button :label "Italic" :id :italic)))
   (rectangle
    (with-radio-box (:value-changed-callback (make-callback
                                              #'(setf rectangle)
                                              #'gadget-id))
      (radio-box-current-selection
       (toggle-button :label "Text-Size"
                      :id    :text-size))
      (toggle-button  :label "Text-Bounding-Rectangle"
                      :id    :text-bounding-rectangle)
      (toggle-button  :label "None"
                      :id    nil)))
   (size
    (let ((callback (make-callback #'(setf text-size*))))
      (make-pane 'slider :orientation            :horizontal
                         :show-value-p           t
                         :value                  (text-size* (state *application-frame*))
                         :min-value              1
                         :max-value              1000
                         :drag-callback          callback
                         :value-changed-callback callback))))
  (:layouts
   (default
    (vertically ()
      (labelling (:label "Text") text)
      (horizontally ()
        (labelling (:label "Family") family)
        (labelling (:label "Face") face)
        (labelling (:label "Rectangle") rectangle))
      (labelling (:label "Size") size)
      (:fill canvas)))))
