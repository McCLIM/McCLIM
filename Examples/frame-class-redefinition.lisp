;;;; (C) Copyright 2019, 2020 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(in-package #:clim-demo)

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
