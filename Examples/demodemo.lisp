;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-DEMO; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: The demo demo
;;;   Created: 2002-02-11
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

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

(in-package :clim-demo)

(defun make-demo-button (title demo-frame-class)
  (make-pane 'push-button
             :label title
             :activate-callback
             (let ((frame nil))
               (lambda (&rest ignore)
                 (declare (ignore ignore))
                 (cond ((null frame)    ;; I broke this logic, sorry.. -Hefner
                        (setq frame
                          (run-frame-top-level
			   (make-application-frame
			    demo-frame-class
			    :calling-frame *application-frame*))))
                       (t
                        #+nil
                        (destroy-frame frame)))))))

(define-application-frame demodemo 
    () ()
    (:layouts
     (default
         (vertically (:equalize-width t)
           (progn ;;spacing (:thickness 10)
             (labelling (:label "McCLIM Demos"
                                :text-style (make-text-style :sans-serif :roman :huge)
                                :align-x :center)))
           (progn ;; spacing (:thickness 10)
             (horizontally ()
               ;; '+fill+
               (labelling (:label "Demos")
                 (vertically (:equalize-width t)
                   (make-demo-button "CLIM-Fig"  'clim-fig)
                   (make-demo-button "Calculator"  'calculator)
                   (make-demo-button "Method Browser" 'method-browser)
                   (make-demo-button "Address Book"  'address-book)
                   (make-demo-button "Puzzle"  'puzzle)
                   (make-demo-button "Gadget Test"  'gadget-test)
                   (make-demo-button "Drag and Drop" 'dragndrop)
                   (make-demo-button "Colorslider" 'colorslider)
                   (make-demo-button "Goatee Test" 'goatee::goatee-test)))
               (labelling (:label "Tests")
                 (vertically (:equalize-width t)
                   (make-demo-button "Label Test" 'label-test)
                   (make-demo-button "Table Test" 'table-test)
                   (make-demo-button "Scroll Test" 'Scroll-test)
                   (make-demo-button "List Test" 'list-test)
                   (make-demo-button "HBOX Test"  'hbox-test)))))))))

(defun demodemo ()
  #+nil
  (loop for port in climi::*all-ports*
      do (destroy-port port))
  (run-frame-top-level (make-application-frame 'demodemo)))

(define-application-frame hbox-test 
    () ()
    (:layouts
     (default
         (horizontally (:background climi::*3d-normal-color*)
           30
           (make-pane 'push-button-pane :label "Okay"
                      :width '(50 :mm))
           '+fill+
           (make-pane 'push-button-pane :label "Cancel")
           '+fill+
           (make-pane 'push-button-pane :label "Help")
           5
           ) )))

(define-application-frame table-test 
    () ()
    (:layouts
     (default
         (tabling (:background +red+)
           (list (make-pane 'push-button-pane :label "Last Name" :max-height +fill+)
                 (make-pane 'push-button-pane :label "First Name" #||:max-height +fill+||#))
           (list (make-pane 'push-button-pane :label "C 1 0")
                 (make-pane 'push-button-pane :label "C 1 1"))
           ) )))

(defun make-test-label (ax ay)
  (labelling (:label "Some Label"
                     :align-x ax
                     :label-alignment ay
                     :foreground +WHITE+
                     :background +PALETURQUOISE4+
                     :text-style (make-text-style :sans-serif :roman :normal))
    (make-pane 'push-button-pane :label (format nil "~S" (list ax ay))
               :text-style (make-text-style :sans-serif :roman :normal)
               :max-width 1000
               :max-height 1000)))

(defun make-test-label2 (ax ay)
  (labelling (:label (format nil "~(~S~)" (list ax ay))
                     :align-x ax
                     :label-alignment ay
                     :foreground +WHITE+
                     :background +PALETURQUOISE4+
                     :text-style (make-text-style :sans-serif :roman :normal))
    #+nil
    (make-pane 'push-button-pane :label 
               :text-style (make-text-style :sans-serif :roman :normal)
               :max-width 1000
               :max-height 1000)))

(define-application-frame label-test
    () ()
    (:layouts
     (default
                                        ;  (scrolling (:width 400 :height 200
                                        ; :max-width 1000 :max-height 2000)
         (vertically (:equalize-width t
                                      ;;:width 400 ;;:height 800
                                      :max-width 2000 :max-height 2000)
           10
           (labelling (:label "CLIM Label Tests"
                              :align-x :center
                              :text-style (make-text-style :sans-serif :roman :huge)))
           10
           (9/10
            (horizontally (:equalize-height t)
              (1/2
               (labelling (:label "Labels with content")
                 (vertically (:equalize-width t)
                   (make-test-label :left :top)
                   5 (make-test-label :center :top)
                   5 (make-test-label :right :top)
                   5 (make-test-label :left :bottom)
                   5 (make-test-label :center :bottom)
                   5 (make-test-label :right :bottom))))
              (1/2
               (labelling (:label "Labels w/o content")
                 (vertically (:equalize-width t)
                   (make-test-label2 :left :top)
                   5
                   (make-test-label2 :center :top)
                   5
                   (make-test-label2 :right :top)
                   5
                   (make-test-label2 :left :bottom)
                   5
                   (make-test-label2 :center :bottom)
                   5
                   (make-test-label2 :right :bottom))))))))))

(defclass foo-pane (basic-pane permanent-medium-sheet-output-mixin) ())

(defmethod compose-space ((pane foo-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :width 800
                          :height 1e3))

(defmethod repaint-sheet ((pane foo-pane) region)
  (draw-line* pane 50 50 200 50)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
    (let ((k 20))
      (loop for y from (* k (floor (- y1 10) k)) below (+ y2 10) by k do
            (draw-text* pane (format nil "~D" y) 20 y)))))

(defmethod dispatch-repaint ((foo-pane foo-pane) region)
  (repaint-sheet foo-pane region))

(defmethod handle-event ((pane foo-pane) (event window-repaint-event))
  (repaint-sheet pane (window-event-region event)))

(define-application-frame scroll-test
    () ()
    (:layouts
     (defaults
         (scrolling (:width 400 :height 400)
           (make-pane 'foo-pane)))))

(define-application-frame list-test
    () ()
    (:layouts
     (defaults
         (labelling (:label "Matching symbols"
                            :text-style (make-text-style :sans-serif :roman :normal))
           (scrolling (:height 200)
             (make-pane 'list-pane
                        :value 'clim:region-intersection
                        :items (apropos-list "INTER" :clim t)
                        :name-key (lambda (x) (format nil "~(~S~)" x))
                        ))))))



(format T "~&;; try (CLIM-DEMO::DEMODEMO)~%")
