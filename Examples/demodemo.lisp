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

(defun run-demo (name &key background)
  "Coerces `name' into symbol in package `clim-demo' and runs application
denoted by this symbol."
  (let ((frame (make-application-frame
                (find-symbol (string-upcase (string name))
                             (find-package "CLIM-DEMO")))))
    (if background
        (bt:make-thread (lambda () (run-frame-top-level frame)))
        (run-frame-top-level frame))
    frame))

(define-application-frame demodemo
    () ()
    (:menu-bar nil)
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
                   (make-demo-button "Calculator"  'calculator-demo:calculator-app)
                   (make-demo-button "Method Browser" 'method-browser)
                   (make-demo-button "Address Book"  'address-book)
                   (make-demo-button "Puzzle"  'puzzle)
                   (make-demo-button "Colorslider" 'colorslider)
                   (make-demo-button "Logic Cube" 'logic-cube)
                   (make-demo-button "Menu Test"  'menutest:menutest)
                   (make-demo-button "Gadget Test"  'gadget-test)
                   (make-demo-button "Drag and Drop" 'dragndrop)
                   (make-demo-button "D&D Translator" 'drag-test)
                   (make-demo-button "Draggable Graph" 'draggable-graph-demo)
                   (make-demo-button "Image viewer" 'image-viewer)
		   (make-pane 'push-button
			      :label "Font Selector"
			      :activate-callback
			      (lambda (&rest ignore)
				(declare (ignore ignore))
				(format *trace-output* "~&You chose: ~A~%"
					(select-font))))
                   (make-demo-button "Tab Layout" 'tabdemo:tabdemo)
                   (make-demo-button "Summation" 'summation)
                   (make-demo-button "Slider demo" 'sliderdemo:sliderdemo)
                   (make-demo-button "German Towns" 'town-example:town-example)
                   (make-demo-button "Data Graph Toy" 'graph-toy)
                   ;; this demo invokes the debugger
                   #+ (or) (make-demo-button "Traffic lights" 'traffic-lights)))
               (labelling (:label "Tests")
                 (vertically (:equalize-width t)
                   (make-demo-button "Stream test" 'stream-test)
                   (make-demo-button "Label Test" 'label-test)
                   (make-demo-button "Table Test" 'table-test)
                   (make-demo-button "Tables with borders" 'table-demo)
                   (make-demo-button "Scroll Test" 'Scroll-test)
                   (make-demo-button "Scroll Test 2" 'Scroll-test-2)
                   (make-demo-button "List Test" 'list-test)
                   (make-demo-button "Option Test" 'option-test)
                   (make-demo-button "HBOX Test"  'hbox-test)
                   (make-demo-button "Text Size Test"  'text-size-test)
                   (make-demo-button "Drawing Benchmark" 'drawing-benchmark)
                   (make-demo-button "Border Styles Test" 'bordered-output)
                   (make-demo-button "Misc. Tests" 'misc-tests)
                   (make-demo-button "Render Image Tests" 'render-image-tests)
		   (make-demo-button "Drawing Tests" 'drawing-tests)
                   (make-demo-button "Accepting Values Test"  'av-test)
                   (make-demo-button "Pane hierarchy viewer" 'hierarchy)))))))))

(defun demodemo ()
  (run-frame-top-level (make-application-frame 'demodemo)))

(define-application-frame hbox-test
    () ()
    (:layouts
     (default
         (horizontally (:background climi::*3d-normal-color*)
           30
           (make-pane 'push-button :label "Okay"
                      :width '(50 :mm))
           '+fill+
           (make-pane 'push-button :label "Cancel")
           '+fill+
           (make-pane 'push-button :label "Help")
           5
           ) )))

(define-application-frame table-test
    () ()
    (:layouts
     (default
         (tabling (:background +red+)
           (list (make-pane 'push-button :label "Last Name" :max-height +fill+)
                 (make-pane 'push-button :label "First Name" #||:max-height +fill+||#))
           (list (make-pane 'push-button :label "C 1 0")
                 (make-pane 'push-button :label "C 1 1"))
           ) )))

(defun make-test-label (ax ay)
  (labelling (:label "Some Label"
                     :align-x ax
                     :label-alignment ay
                     :foreground +WHITE+
                     :background +PALETURQUOISE4+
                     :text-style (make-text-style :sans-serif :roman :normal))
    (make-pane 'push-button :label (format nil "~S" (list ax ay))
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
    (make-pane 'push-button :label
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

(defmethod handle-repaint ((pane foo-pane) region)
  (draw-line* pane 50 50 200 50)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
    (declare (ignore x1 x2))
    (let ((k 20))
      (loop for y from (* k (floor (- y1 10) k)) below (+ y2 10) by k do
            (draw-text* pane (format nil "~D" y) 20 y)))))

(define-application-frame scroll-test
    () ()
    (:layouts
     (defaults
         (scrolling (:width 400 :height 400)
           (make-pane 'foo-pane)))))


;;; Scroll test 2
(define-application-frame scroll-test-2 ()
  ()
  (:geometry :width 1200 :height 400)
  (:panes (out :application-pane :display-function #'scroll-test-display)
          (bam :application-pane :display-function #'scroll-test-display)
          (foo :application-pane :display-function #'scroll-test-display)
          (qux :application-pane :display-function #'scroll-test-display))
  (:layouts (default
                (vertically ()
                  (horizontally ()
                    (labelling (:label "bam") (scrolling () bam))
                    (labelling (:label "foo") (scrolling () foo))
                    (labelling (:label "qux") (scrolling () qux)))
                  (labelling (:label "Description") out)))))

(defmethod scroll-test-display ((frame scroll-test-2) pane)
  (when (member (pane-name pane) '(bam qux))
    (draw-rectangle* pane -100 -100 +100 +100 :filled nil
                     :ink +red+ :line-thickness 5 :line-dashes t))
  (when (member (pane-name pane) '(foo qux))
    (draw-rectangle* pane +300 +300 +500 +500 :filled nil
                     :ink +red+ :line-thickness 5 :line-dashes t))

  (when (member (pane-name pane) '(out))
    (format pane "In this test we draw three panes: bam, foo and qux. You may try resizing the window and see what happens. If viewports are small enough, scroll-bars should appear to show bottom-right rectangle on \"foo\" and \"qux\". Upper-left rectangle has starting point having negative coordinates of its sheet, so scroll-bars won't update to uncover it (this may be supported in the future - in such case please change description of this test). Only bottom-right rectangle must be seen in full.

\"bam\" should have only partially drawn rectangle (upper-left corner).
\"foo\" is similar, but rectangle is drawn in bottom-right corner. This draw should extend scroll-bars, so user may see whole rectangle.
\"qux\" combines both previous panes. Part of the rectangle in the upper-left corner and (after scrolling) full rectangle on the bottom-right corner.")))

(define-application-frame list-test
    () ()
    (:panes
     (substring :text-field :value "INTER")
     (result-list
      (make-pane 'list-pane
		 :value 'clim:region-intersection
		 :items (apropos-list "INTER" :clim)
		 :presentation-type-key (constantly 'list-test-symbol)
		 :name-key (lambda (x) (format nil "~(~S~)" x))))
     (interactor :interactor :height 200))
    (:layouts
     (defaults
         (labelling (:label "Matching symbols"
                            :text-style (make-text-style :sans-serif :roman :normal))
           (vertically ()
	     (scrolling (:height 200)
	       result-list)
	     (horizontally ()
	       substring
	       (make-pane 'push-button
			  :label "Update"
			  :activate-callback 'update-list-test))
	     interactor)))))

(define-presentation-type list-test-symbol ())

(define-list-test-command com-describe-symbol
    ((sym list-test-symbol :gesture :select))
  ;; Let's print only three lines, we don't have space for more.
  (with-input-from-string (s (with-output-to-string (s) (describe sym s)))
    (dotimes (x 3)
      (write-line (read-line s nil "") *standard-input*))))

(defun update-list-test (pane)
  (declare (ignore pane))
  (setf (list-pane-items (find-pane-named *application-frame* 'result-list))
	(apropos-list (gadget-value
		       (find-pane-named *application-frame* 'substring))
		      :clim #+sbcl t)))

(define-application-frame option-test
    () ()
    (:panes (option-pane-1 :option-pane
                           :value 1
                           :items '(1 2 3 4 6 7)
                           :value-changed-callback (constantly nil))
            (option-pane-2 :option-pane
                           :value "Option 1"
                           :items '("Option 1" "Option 2" "Option 3" "Option 4" "Option 6" "Option 7")
                           :value-changed-callback (constantly nil)))
    (:layouts
     (:default
         (vertically (:label "Option panes example")
           (1/2 option-pane-1)
           (1/2 option-pane-2)))))

(format t "~&;; try (CLIM-DEMO:DEMODEMO)~%")
