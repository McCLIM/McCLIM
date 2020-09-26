;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2000 Iban Hatchondo <hatchond@emi.u-bordeaux.fr>
;;;  (c) copyright 2000 Julien Boninfante <boninfan@emi.u-bordeaux.fr>
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; A simple RGB color chooser example.
;;;

(defpackage #:clim-demo.colorslider
  (:use
   #:clim-lisp
   #:clim)
  (:export
   #:colorslider))
(in-package #:clim-demo.colorslider)

;;; Example gadget definition.

(defclass abstract-colored-gadget (basic-gadget) ())
(defclass generic-colored-gadget (abstract-colored-gadget)
  ((color :initform +black+ :accessor colored-gadget-color)))

(defmethod handle-repaint ((gadget generic-colored-gadget) region)
  (declare (ignore region))
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region gadget)
    (draw-rectangle* gadget x1 y1 x2 y2 :ink (colored-gadget-color gadget))))

;;; Slider callback and macro.

(defun make-slider (label writer)
  (flet ((update (gadget value)
           (let ((frame (gadget-client gadget)))
             (funcall writer value frame))))
    (make-pane :slider :label label
                       :min-value 0
                       :max-value 1
                       :value 0
                       :show-value-p t :decimal-places 2
                       :orientation :horizontal
                       :drag-callback #'update
                       :value-changed-callback #'update)))

(define-application-frame colorslider ()
  ((%red :accessor red :initform 0)
   (%green :accessor green :initform 0)
   (%blue :accessor blue :initform 0))
  (:menu-bar nil)
  (:panes
   (combined generic-colored-gadget :min-width 40)
   (red generic-colored-gadget :min-width 20)
   (green generic-colored-gadget :min-width 20)
   (blue generic-colored-gadget :min-width 20)
   (slider-red (make-slider "Red" #'(setf red)))
   (slider-green (make-slider "Green" #'(setf green)))
   (slider-blue (make-slider "Blue" #'(setf blue))))
  (:layouts
   (default
    (spacing (:thickness 4)
      (horizontally ()
        (labelling (:label "Combined")
          combined)
        (:fill (labelling (:label "Components")
                 (vertically (:y-spacing 8)
                   (horizontally (:x-spacing 4)
                     red (:fill slider-red))
                   (horizontally (:x-spacing 4)
                     green (:fill slider-green))
                   (horizontally (:x-spacing 4)
                     blue (:fill slider-blue)))))))))
  (:default-initargs
   :width 500 :height 250))

(defun update (frame)
  (let ((red (red frame))
        (green (green frame))
        (blue (blue frame)))
    (flet ((update-gadget (gadget-name color)
             (let ((colored (find-pane-named *application-frame* gadget-name)))
               (setf (colored-gadget-color colored) color)
               (repaint-sheet colored +everywhere+))))
      (update-gadget 'combined (make-rgb-color red green blue))
      (update-gadget 'red (make-rgb-color red 0 0))
      (update-gadget 'green (make-rgb-color 0 green 0))
      (update-gadget 'blue (make-rgb-color 0 0 blue)))))

(defmethod (setf red) :after (new-value (frame colorslider))
  (update frame))

(defmethod (setf green) :after (new-value (frame colorslider))
  (update frame))

(defmethod (setf blue) :after (new-value (frame colorslider))
  (update frame))

;;; Test function.

(defun colorslider ()
  (run-frame-top-level (make-application-frame 'colorslider)))
