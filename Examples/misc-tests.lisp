;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-DEMO; -*-

;;; Random McCLIM tests.

;;; Have some subtle stream/graphics/recording behavior which you'd
;;; like to ensure continues to work? Add a test for it here!

;;; (C) Copyright 2006 by Andy Hefner (ahefner@gmail.com)

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

(defstruct misc-test-item name drawer description)

(define-application-frame misc-tests ()
  ()
  (:panes
   (output :application-pane)
   (description :application-pane)
   (selector :list-pane
             :mode :exclusive
             :name-key #'misc-test-item-name
             :items (list
                     (make-misc-test-item :name "Empty Records 1"
                                          :drawer 'misc-empty-records-1
                                          :description "Tests the effect of empty output records on their parent's bounding rectangle. If successful, you will see a circle enclosed in a square. The square should tightly fit the circle. If the rectangle extends all the way to the top/left edges of the pane, McCLIM is not handling this correctly. This specifically exercises addition of empty children in recompute-extent-for-new-child.")
                     (make-misc-test-item :name "Empty Records 2"
                                          :drawer 'misc-empty-records-2
                                          :description "Tests the effect of empty output records on their parent's bounding rectangle. If successful, you will see a circle enclosed in a square. The square should tightly fit the circle. If the rectangle extends all the way to the top/left edges of the pane, McCLIM is not handling this correctly. This specifically tests addition and deletion of an empty child, and failure may point to recompute-extent-for-new-child or recompute-extent-for-changed-child.")
                     (make-misc-test-item :name "Empty Records 3"
                                          :drawer 'misc-empty-records-3
                                          :description "Tests the effect of empty output records on their parent's bounding rectangle. If successful, you will see a circle enclosed in a square. The square should tightly fit the circle. If the rectangle extends all the way to the top/left edges of the pane, McCLIM is not handling this correctly. This test creates a new output record, fills it with content, then clears the record contents.")
                     (make-misc-test-item :name "Empty Borders"
                                          :drawer 'misc-empty-bordering
                                          :description "Tests handling of empty output records by surrounding-output-with-border. If successful, you will see twelve small circles arranged themselves in a larger circle. A likely failure mode will exhibit the circles piled on each other in the upper-left corner of the pane.")
		     (make-misc-test-item :name "Underlining"
                                          :drawer 'misc-underlining-test
					  :description "Tests the underlining border style. You should see five lines of text, equally spaced, with the second and third lines having the phrase 'all live' underlined, first by a thick black line then by a thin dashed red line. If the lines are broken or the spacing is irregular, the :move-cursor nil key of surrounding-output-with-border may not have behaved as expected. "))
             :value-changed-callback
             (lambda (pane item)
               (declare (ignore pane))
               (let ((output (get-frame-pane *application-frame* 'output))
                     (description (get-frame-pane *application-frame* 'description)))
                 (window-clear output)
                 (window-clear description)
                 (with-text-style (description (make-text-style :sans-serif :roman :normal))
                   (write-string (misc-test-item-description item) description))
                 (funcall (misc-test-item-drawer item) output)))))
  (:layouts
   (default
     (spacing (:thickness 3)
       (horizontally ()
         (spacing (:thickness 3) (clim-extensions:lowering () selector))
         (vertically ()
	   (spacing (:thickness 3)
	     (clim-extensions:lowering ()
	       (scrolling (:width 600 :height 600) output)))
	   (spacing (:thickness 3)
	     (clim-extensions:lowering ()
	       (scrolling (:scroll-bar :vertical :height 200) description)))))))))

(defun misc-empty-records-1 (stream)
  (surrounding-output-with-border (stream :shape :rectangle)
    (draw-circle* stream 200 200 40)
    (with-new-output-record (stream))))

(defun misc-empty-records-2 (stream)
  (surrounding-output-with-border (stream :shape :rectangle)
    (draw-circle* stream 200 200 40)
    (let ((record (with-new-output-record (stream))))
      (delete-output-record record (output-record-parent record)))))

(defun misc-empty-records-3 (stream)
  (surrounding-output-with-border (stream :shape :rectangle)
    (draw-circle* stream 200 200 40)
    (let ((record (with-new-output-record (stream)
                    (draw-circle* stream 50 50 10))))
      (clear-output-record record))))

(defun misc-empty-bordering (stream)
  (with-room-for-graphics (stream :first-quadrant nil)
    (with-text-style (stream (make-text-style :sans-serif :roman :small))
      (loop with outer-radius = 180
            with inner-radius = 27
            with n = 12
            for i from 0 below n do
            (setf (stream-cursor-position stream)
                  (values (* outer-radius (sin (* i 2 pi (/ n))))
                          (* outer-radius (cos (* i 2 pi (/ n))))))
            (surrounding-output-with-border (stream :shape :ellipse
                                                    :circle t
                                                    :min-radius inner-radius
                                                    :shadow +gray88+
                                                    :shadow-offset 7
                                                    :filled t
						    :line-thickness 1
                                                    :background +gray50+
                                                    :outline-ink +gray40+)
              ;(multiple-value-call #'draw-point* stream (stream-cursor-position stream))
              #+NIL (print i stream))))))

(defun misc-underlining-test (stream)
  (with-text-family (stream :sans-serif)
    (format stream "~&We all live in a yellow subroutine.~%")
    (format stream "~&We ")
    (surrounding-output-with-border (stream :shape :underline
					    :line-thickness 2
					    :move-cursor nil)
      (format stream "all live"))
    (format stream " in a yellow subroutine.~%")
    (format stream "~&We ")
    (surrounding-output-with-border (stream :shape :underline
					    :ink +red+
					    :line-dashes t
					    :move-cursor nil)
      (format stream "all live"))
    (format stream " in a yellow subroutine.~%")
    (format stream "~&We all live in a yellow subroutine.~%")
    (format stream "~&We all live in a yellow subroutine.~%")))

