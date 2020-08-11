;;; ---------------------------------------------------------------------------
;;;   License: BSD-2-Clause
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019, 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Demonstrates the definition of an application frame class that
;;; updates the panes, layouts, command tables, etc of its instances
;;; when the application frame class is redefined.

(defpackage #:clim-demo.frame-class-redefinition
  (:use
   #:clim-lisp
   #:clim)

  (:export
   #:frame-class-redefinition))

(in-package #:clim-demo.frame-class-redefinition)

(defvar *redefintion-description*
  (format nil "The class of this frame is defined in ~@
               ~@
               ~2@T~A.~@
               ~@

               Try changing and re-evaluating (for example using C-c ~@
               C-c in SLIME) the ~A form in that file.~@

               ~@
               Things to try:~@
               • Change pane definitions~@
               ~2@T• Change initargs (labels, background colors, etc.)~@
               ~2@T• Change pane types (for example :push-button to :toggle-button)~@
               • Add new pane definitions (and add them to one or more layouts)~@
               • Change layouts~@
               • Reorder layouts to change the primary (i.e. first)~@
                 layout in the layout list"
          #.(or *compile-file-pathname* *load-pathname*)
          'define-application-frame))

(define-application-frame frame-class-redefinition ()
  ()
  (:panes
   (label         :label         :label        *redefintion-description*
                                 :background   +beige+)
   (push-button-1 :push-button   :label        "Button 1")
   (push-button-2 :push-button   :label        "Button 2"
                                 :foreground   +white+
                                 :background   +blue+)
   (slider        :slider        :min-value    0
                                 :max-value    10
                                 :value        1
                                 :show-value-p t
                                 :orientation  :horizontal)
   (toggle-button :toggle-button :label        "Toggle this")
   (interactor    :interactor))
  (:layouts
   (:default
    (climi::bordering (:thickness 8 :background +dark-gray+)
      (vertically (:spacing 8)
        label
        (horizontally (:spacing 8)
          (2/3 slider)
          (1/3 toggle-button))
        (horizontally (:spacing 8)
          push-button-1
          push-button-2)
        (make-pane 'clime:box-adjuster-gadget)
        (:fill interactor))))
   (:second-layout
    label))
  (:menu-bar nil)
  (:update-instances-on-redefinition t)
  (:default-initargs
   :pretty-name "Redefine me!"))
