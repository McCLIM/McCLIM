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


(defvar *misc-tests* (make-hash-table :test 'equal))

(defstruct misc-test name description drawer)

(defmacro define-misc-test (name arglist description &body body)
  (check-type name string)
  (check-type description string)
  `(setf (gethash ,name *misc-tests*)
         (make-misc-test :name ,name
                         :description ,description
                         :drawer (lambda ,arglist ,@body))))

(define-application-frame misc-tests ()
  ()
  (:panes
   (output :application-pane)
   (description :application-pane)
   (selector :list-pane
             :mode :exclusive
             :name-key #'misc-test-name
             :items (sort (loop for x being the hash-values of *misc-tests*
                                collect x) #'string< :key #'misc-test-name)
             :value-changed-callback
             (lambda (pane item)
               (declare (ignore pane))
               (let ((output (get-frame-pane *application-frame* 'output))
                     (description (get-frame-pane *application-frame* 'description)))
                 (window-clear output)
                 (window-clear description)
                 (with-text-style (description (make-text-style :sans-serif :roman :normal))
                   (write-string (misc-test-description item) description))
                 (funcall (misc-test-drawer item) output)))))
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

(defun misc-test-postscript (test &optional filename)
  (let* ((test (if (stringp test) (gethash test *misc-tests*) test))
         (test-name (misc-test-name test))
         (filename (or filename (format nil "/tmp/~A.eps"
                                        test-name))))
    (with-open-file (out filename :direction :output :if-exists :supersede)
      (with-output-to-postscript-stream (stream out :device-type :eps)
        #+NIL
        (with-text-style (stream (make-text-style :sans-serif :roman :normal))
          (format stream "~&~A: ~A~%" test-name (misc-test-description test)))
        (funcall (misc-test-drawer test) stream)))))

(defun run-all-postscript-tests ()
  (loop for test being the hash-values of *misc-tests* do
       (restart-case (misc-test-postscript test)
         (:skip ()
           :report (lambda (stream) (format stream "Skip ~A" (misc-test-name test)))))))

(define-misc-test "Empty Records 1" (stream)                                          
    "Tests the effect of empty output records on their parent's bounding rectangle. If successful, you will see a circle enclosed in a square. The square should tightly fit the circle. If the rectangle extends all the way to the top/left edges of the pane, McCLIM is not handling this correctly. This specifically exercises addition of empty children in recompute-extent-for-new-child."
  (surrounding-output-with-border (stream :shape :rectangle)
    (draw-circle* stream 200 200 40)
    (with-new-output-record (stream))))


(define-misc-test "Empty Records 2" (stream)
 "Tests the effect of empty output records on their parent's bounding rectangle. If successful, you will see a circle enclosed in a square. The square should fit the circle within a few pixels distance. If the rectangle extends all the way to the top/left edges of the pane, McCLIM is not handling this correctly. This specifically tests addition and deletion of an empty child, and failure may point to recompute-extent-for-new-child or recompute-extent-for-changed-child."
  (surrounding-output-with-border (stream :shape :rectangle)
    (draw-circle* stream 200 200 40)
    (let ((record (with-new-output-record (stream))))
      (delete-output-record record (output-record-parent record)))))

(define-misc-test "Empty Records 3" (stream)
  "Tests the effect of empty output records on their parent's bounding rectangle. If successful, you will see a circle enclosed in a square. The square should fit the circle within a few pixels distance. If the rectangle extends all the way to the top/left edges of the pane, McCLIM is not handling this correctly. This test creates a new output record, fills it with content, then clears the record contents."
  (surrounding-output-with-border (stream :shape :rectangle)
    (draw-circle* stream 200 200 40)
    (let ((record (with-new-output-record (stream)
                    (draw-circle* stream 50 50 10))))
      (clear-output-record record))))
                                           
(define-misc-test "Empty Borders" (stream)
    "Tests handling of empty output records by surrounding-output-with-border. If successful, you will see twelve small circles arranged themselves in a larger circle. A likely failure mode will exhibit the circles piled on each other in the upper-left corner of the pane."
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
                                                    :outline-ink +gray40+))))))

(define-misc-test "Underlining" (stream)
    "Tests the underlining border style. You should see five lines of text, equally spaced, with the second and third lines having the phrase 'all live' underlined, first by a thick black line then by a thin dashed red line. If the lines are broken or the spacing is irregular, the :move-cursor nil key of surrounding-output-with-border may not have behaved as expected. "
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

(define-misc-test "Transparent Ink Test" (stream)
    "Drawing with transparent ink can be useful as a way of reserving space as padding around the visible part of a drawing. This test checks that the medium supports drawing in transparent ink, and that it is recorded with the expected bounding rectangle. It will draw two tables, which should format identically except for one square, which will be transparent in the first table and blue in the second. If the in absence of the blue square its row and column collapse to a small size, the bounding rectangle for the transparent squares is probably wrong. Light gray circles will be drawn in the background, and should show through the empty row/column of the table."
  (let ((table '((1 1 1 0 1)
                 (1 1 1 0 1)
                 (1 1 1 0 1)
                 (0 0 0 2 0)
                 (1 1 1 0 1)))
        (inks (list +transparent-ink+ +red+ +blue+))
        (records nil))
    ;; Draw some junk to make sure the transparent ink is really transparent,
    ;; and not just matching the background:
    (dotimes (i 400)
      (draw-circle* stream (- (random 600) 100) (- (random 600) 100) (1+ (* 40 (random 1.0) (random 1.0))) :ink +gray90+))
    ;; Draw two tables:
    (format-items '(0 2) :stream stream :printer
      (lambda (foo stream)
        ;; Why isn't there an :equalize-row-heights ?
        (surrounding-output-with-border (stream)
          (formatting-table (stream :equalize-column-widths nil)
            (dolist (row table)
              (formatting-row (stream)
                (dolist (cell row)
                  (formatting-cell (stream)
                    (push
                     (with-new-output-record (stream)
                       (draw-rectangle* stream 0 0 32 32
                                        :ink (elt inks (if (eql cell 2)
                                                           foo
                                                           cell))))
                     records)))))))))
    ;; Make sure the bounding rectangles are the same:
    (unless (reduce
             (lambda (a b)
               (and a
                    (> 1 (abs (- (bounding-rectangle-width a)
                                 (bounding-rectangle-width b))))
                    (> 1 (abs (- (bounding-rectangle-height a)
                                 (bounding-rectangle-height b))))
                    b))
             records)
      (format stream "~&The bounding rectangles don't look right..~%"))))

(define-misc-test "Arrows" (stream)
    "Tests scaling and rotation of arrow heads, and the handling of the case where the heads become sufficiently large that they would overlap and should join in the middle. The line thickness and arrowhead width is increased from thin to thick, counterclockwise. The tips of the arrows should always fall on the green and red points."
  (let ((scale 1.2)
        (from-head t)
        (to-head t))
  (with-room-for-graphics (stream :first-quadrant nil)
   (with-scaling (stream scale scale)
     (loop for theta from 0.0 below (* 2 pi) by (/ (* 2 pi) 17) do
          (progn (let* ((x2 (* 250 (sin theta)))
                        (y2 (* 250 (cos theta)))
                        (x1 (* 0.2 x2))
                        (y1 (* 0.2 y2)))
                   (draw-arrow* stream x1 y1 x2 y2
                                :line-thickness (1+ (* 8 theta))
                                :head-width (* 5 (1+ theta))
                                :to-head to-head
                                :from-head from-head
                                :head-length (* 10 (1+ theta)) )
                   (draw-point* stream x1 y1 :ink +red+ :line-thickness 5)
                   (draw-point* stream x2 y2 :ink +green+ :line-thickness 5))))))))

(define-misc-test "Gadget Output Records" (stream)
    "This tests integration of gadget output records. They should have correct bounding rectangles, and moving the output record should move the gadget. Adding/removing the output record from the history should add/remove the gadget as expected. If these things are true, gadget outputs records should work in almost any situation normal CLIM drawing would (excluding inside incremental redisplay, at present (?)), including graph layout and table formatting. This test uses format-graph-from-roots to create graph whose nodes are push-buttons."
  (let ((tree '((peter peter pumpkin eater)
                (had (a wife) (but (couldnt (keep (her)))))
                (he (put her (in (a pumpkin shell))))
                (and there he (kept her (very well))))))
    (format-graph-from-roots tree
      (lambda (obj stream)
        (let ((obj (typecase obj (list (first obj)) (t obj))))
          (let ((fm (frame-manager *application-frame*)))
            (with-look-and-feel-realization (fm *application-frame*)
              (with-output-as-gadget (stream)
                (make-pane 'push-button
                           :activate-callback
                           (lambda (&rest args)
                             (declare (ignore args))
                             (notify-user *application-frame* "You clicked a button."))
                           :label (string-downcase
                                   (princ-to-string obj))))))))
      (lambda (obj)
        (if (listp obj) (rest obj) nil))
      :stream stream)))

(define-misc-test "Line Widths" (stream)
    "Hi there."
  (formatting-table (stream)
    (loop for scale-expt from 0 upto 2
          as scale = (expt 2 scale-expt) do
         (with-scaling (stream scale)
           (formatting-row (stream)
             (loop for thickness from 1 upto 25 by 5
                   with width = 40
                   with width/2 = (/ width 2) do
                  (formatting-cell (stream)
                    (draw-rectangle* stream 0 (- width/2) width width/2 :line-thickness thickness :filled nil :ink +red+ :line-unit :coordinate)
                    (draw-circle* stream width/2 0 width/2 :line-thickness thickness :filled nil :ink +blue+ :line-unit :coordinate)
                    (draw-line* stream 0 0 width 0
                                :line-thickness thickness
                                :line-cap-shape :round
                                :line-unit :coordinate)
                    #+NIL
                    (draw-rectangle* stream 0 (- width/2) width width/2 :filled nil :ink +white+))))))))


    
