;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;;;  (c) copyright 2000 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

;;; Copied from colorslider

(in-package :clim-internals)

;; example gadget definition
(defclass gadget-test-pane (standard-gadget) ())

(in-package :clim-demo)

(defun gadget-test ()
  (loop for port in climi::*all-ports*
      do (destroy-port port))
  (setq climi::*all-ports* nil)
  #-gl
  (setq frame (make-application-frame 'gadget-test
              :frame-manager (make-instance 'clim-internals::pixie/clx-look :port (find-port))))
  #+gl ; a bit of a hack...
  (setq frame (make-application-frame 'gadget-test))
  (setq fm (frame-manager frame))
  (setq port (climi::frame-manager-port fm))
  (setq pane (first (frame-panes frame)))
  (setq medium (sheet-medium pane))
  (setq graft (graft frame))
  (setq vbox (climi::frame-pane frame))
  (run-frame-top-level frame))

(defmethod gadget-test-frame-top-level ((frame application-frame)
				       &key (command-parser 'command-line-command-parser)
				       (command-unparser 'command-line-command-unparser)
				       (partial-command-parser
					'command-line-read-remaining-arguments-for-partial-command)
				       (prompt "Command: "))
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (catch 'exit
    (clim-extensions:simple-event-loop))
  (frame-exit frame))

(make-command-table 'lisp-menu
                    :errorp nil
                    :menu '(("Heir" :menu lisp-sub-menu)
                            ("Lisp" :command test)
                            ("Lisp" :command test)))

(make-command-table 'lisp-sub-menu
                    :errorp nil
                    :menu '(("Does"  :command test)
                            ("This"  :command test)
                            ("Work?" :command test)))

(make-command-table 'edit-menu
                    :errorp nil
                    :menu '(("Edit" :command test)
                            ("Edit" :command test)
                            ("Edit" :command test)))

(make-command-table 'view-menu
                    :errorp nil
                    :menu '(("View" :command test)
                            ("View" :command test)
                            ("View" :command test)))

(make-command-table 'search-menu
                    :errorp nil
                    :menu '(("Search" :command test)
                            ("Search" :command test)
                            ("Search" :command test)))

(define-command test ()
  (format *error-output* "That was just a test~%")
  (finish-output *error-output*))

(define-application-frame gadget-test
    () ()
    (:menu-bar
     (("Lisp"   :menu lisp-menu)
      ("Edit"   :menu edit-menu)
      ("View"   :menu view-menu)
      ("Search" :menu search-menu)))
    (:panes
;    (raised     (raising (:border-width 3 :background +Gray83+)
;                  (make-pane 'check-box :choices '("First" "Second" "Third"))))
     (text-field :text-field
                 :value "Text Field")
     (text-edit  :text-editor
                 :value "Text Editor")
     (slider-h   :slider
                 :min-value 0
                 :max-value 100
                 :value 0
                 :show-value-p t
                 :orientation :horizontal
                 :current-color +black+
                 :width 120
                 :height 30)
     #-gl
     (image      :image
                ;:background +Gray83+
                 :background +Red+
                 :pathname "CLIM:IMAGES;GREEN-APPLE.PPM")
     (slider-v   :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :current-color +black+
                 :value 0
                 :width 120
                 :height 90)
     (slider-v1  :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :current-color +black+
                 :value 0
                 :width 30
                 :height 30)
     (slider-v2  :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :current-color +black+
                 :value 0
                 :width 30
                 :height 60)
     (slider-v3  :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :current-color +black+
                 :value 0
                 :width 30
                 :height 90)
     (slider-v4  :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :current-color +black+
                 :value 0
                 :width 30
                 :height 120)
     (push-btn   :push-button
                 :label "Push")
     (toggle-btn :toggle-button
                 :label "Toggle"
                 :value t
                 :width 120
                 :height 30
                 :normal +red+
                 :highlighted +red+
                 :pushed-and-highlighted +red+)
     #-gl
     (scroll    (raising (:border-width 3 :background +Gray83+)
                   (scrolling (:width 240 :height 120 :background +Gray83+)
                     (vertically ()
                       (with-radio-box (:orientation :horizontal)
                         (clim:radio-box-current-selection "First") "Second" "Third"
                         "Red" "Blue" "Orange"
                         "Elephant" "Dog" "Cat")
                       (with-radio-box (:orientation :horizontal :type :some-of)
                         (clim:radio-box-current-selection "Fourth") "Fifth" "Sixth")
                       (with-radio-box (:orientation :horizontal)
                         (clim:radio-box-current-selection "Seventh") "Eighth" "Ninth")
                       (with-radio-box (:orientation :horizontal :type :some-of)
                         (clim:radio-box-current-selection "Tenth") "Eleventh" "Twelth")))))
     (radio-box  (with-radio-box (:orientation :horizontal)
                   (clim:radio-box-current-selection "One") "Two" "Three"))
     (check-box  (with-radio-box (:type :some-of :orientation :horizontal)
                   (clim:radio-box-current-selection "First") "Second" "Third")))
    (:layouts
     (default
       (vertically ()
        ;raised
         text-field
        ;text-edit
         slider-h
         (horizontally ()
           (vertically ()
             slider-v
             slider-v2)
           slider-v3
           slider-v4
           #-gl
           image)
         push-btn
         toggle-btn
         #-gl
         scroll
         radio-box
         check-box
         )))
    (:top-level (gadget-test-frame-top-level . nil)))
