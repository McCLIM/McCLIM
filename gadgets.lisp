;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2000 by 
;;; Arthur Lemmens (lemmens@simplex.nl),
;;; Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;; and Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2001 by
;;; Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2001 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>

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

(in-package :CLIM-INTERNALS)


;;;
;;; Gadget
;;;

(defclass gadget (permanent-medium-sheet-output-mixin immediate-sheet-input-mixin immediate-repainting-mixin pane)
  ((id :initarg :id
       :initform (gensym "GADGET")
       :accessor gadget-id)
   (client :initarg :client
           :initform *application-frame*
           :accessor gadget-client)
   (armed-callback :initarg :armed-callback
                   :initform nil
                   :reader gadget-armed-callback)
   (disarmed-callback :initarg :disarmed-callback
                      :initform nil
                      :reader gadget-disarmed-callback)
   (active-p;; [Arthur] I'm not so sure about the value for :initform.
    ;; Maybe T is better? Or maybe we should call
    ;; ACTIVATE-GADGET after creating a gadget?
    :initform t
    :reader gadget-active-p)
   (armed :initform nil)))

(defun gadgetp (object)
  (typep object 'gadget))

;;
;; gadget's colors
;;

(defclass gadget-color-mixin ()
  ((normal :type color
	   :initform +gray80+
	   :initarg :normal
	   :accessor gadget-normal-color)
   (highlighted :type color
		:initform +gray85+
		:initarg :highlighted
		:accessor gadget-highlighted-color)
   (pushed-and-highlighted :type color
			   :initform +gray75+
			   :initarg :pushed-and-highlighted
			   :accessor gadget-pushed-and-highlighted-color)
   (current-color :type color
		  :accessor gadget-current-color))
  (:documentation "This class define the gadgets colors."))

(defmethod initialize-instance :after ((gadget gadget-color-mixin) &rest args)
  (declare (ignore args))
  (setf (slot-value gadget 'current-color) (gadget-normal-color gadget)))

(defmethod (setf gadget-current-color) :after (color (gadget gadget-color-mixin))
  (declare (ignore color))
  (dispatch-repaint gadget (sheet-region gadget)))
  

;;;
;;; gadgets look
;;;

(defun display-gadget-background (gadget color x1 y1 x2 y2)
  (draw-rectangle* gadget x1 y1 x2 y2 :ink color :filled t))

(defun draw-edges-lines* (pane x1 y1 x2 y2)
  (draw-line* pane x1 y1 x2 y1 :ink +white+)
  (draw-line* pane x1 y1 x1 y2 :ink +white+)
  (draw-line* pane x1 y2 x2 y2 :ink +black+)
  (draw-line* pane x2 y1 x2 y2 :ink +black+))

;;;;
;;;; 3D-BORDER-MIXIN Class
;;;;

;; 3D-BORDER-MIXIN class can be used to add a 3D-ish border to
;; panes. There are three new options:
;;
;;  :border-width       The width of the border
;;  :border-style       The border's style one of :inset, :outset, :groove, :ridge, :solid,
;;                      :double, :dotted, :dashed
;;                      [:dotted and :dashed are not yet implemented]
;;
;;  :border-color       The border's color
;;                      [Not implemented yet]
;;
;; [These options are modelled after CSS].
;;
;; When using 3D-BORDER-MIXIN, one should query the pane's inner
;; region, where drawing should take place, by PANE-INNER-REGION.
;;
;; --GB

#||
;; Motif-ish
(defparameter *3d-dark-color*   (make-gray-color .45))
(defparameter *3d-normal-color* (make-gray-color .75))
(defparameter *3d-light-color*  (make-gray-color .92))
(defparameter *3d-inner-color*  (make-gray-color .65))
||#

;; Gtk-ish

(defparameter *3d-dark-color*   (make-gray-color .59))
(defparameter *3d-normal-color* (make-gray-color .84))
(defparameter *3d-light-color*  (make-gray-color 1.0))
(defparameter *3d-inner-color*  (make-gray-color .75))

(defclass 3D-border-mixin ()
  ((border-width :initarg :border-width :initform 2)
   (border-style :initarg :border-style :initform :outset)
   (border-color :initarg :border-color :initform "???")))

(defmethod pane-inner-region ((pane 3D-border-mixin))
  (with-slots (border-width) pane
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
      (make-rectangle* (+ x1 border-width) (+ y1 border-width)
                       (- x2 border-width) (- y2 border-width)))))

(defmethod repaint-sheet :after ((pane 3D-border-mixin) region)
  (declare (ignore region))
  (with-slots (border-width border-style) pane
    (draw-bordered-polygon pane (polygon-points (bounding-rectangle (sheet-region pane)))
                           :border-width border-width
                           :style border-style)))

;;;;
;;;; 3D-ish Look
;;;;

;; DRAW-BORDERED-POLYGON medium point-seq &key border-width style
;;
;; -GB

(labels ((line-hnf (x1 y1 x2 y2)
           (values (- y2 y1) (- x1 x2) (- (* x1 y2) (* y1 x2))))
         
         (line-line-intersection (x1 y1 x2 y2 x3 y3 x4 y4)
           (multiple-value-bind (a1 b1 c1) (line-hnf x1 y1 x2 y2)
             (multiple-value-bind (a2 b2 c2) (line-hnf x3 y3 x4 y4)
               (let ((d (- (* a1 b2) (* b1 a2))))
                 (cond ((< (abs d) 1e-6)
                        nil)
                       (t
                        (values (/ (- (* b2 c1) (* b1 c2)) d)
                                (/ (- (* a1 c2) (* a2 c1)) d))))))))
         
         (polygon-orientation (point-seq)
           "Determines the polygon's orientation.
            Returns:  +1 = counter-clock-wise 
                      -1 = clock-wise

            The polygon should be clean from duplicate points or co-linear points.
            If the polygon self intersects, the orientation may not be defined, this
            function does not try to detect this situation and happily returns some
            value."
           ;;
           (let ((n (length point-seq)))
             (let* ((min-i 0)
                    (min-val (point-x (elt point-seq min-i))))
               ;;
               (loop for i from 1 below n do
                     (when (< (point-x (elt point-seq i)) min-val)
                       (setf min-val (point-x (elt point-seq i))
                             min-i i)))
               ;;
               (let ((p0 (elt point-seq (mod (+ min-i -1) n)))
                     (p1 (elt point-seq (mod (+ min-i 0) n)))
                     (p2 (elt point-seq (mod (+ min-i +1) n))))
                 (signum (- (* (- (point-x p2) (point-x p0)) (- (point-y p1) (point-y p0)))
                            (* (- (point-x p1) (point-x p0)) (- (point-y p2) (point-y p0)))))))))
         
         (clean-polygon (point-seq)
           "Cleans a polygon from duplicate points and co-linear points. Furthermore
            tries to bring it into counter-clock-wise orientation."
           ;; first step: remove duplicates
           (setf point-seq
                 (let ((n (length point-seq)))
                   (loop for i from 0 below n 
                         for p0 = (elt point-seq (mod (+ i -1) n))
                         for p1 = (elt point-seq (mod (+ i 0) n))
                         unless (and (< (abs (- (point-x p0) (point-x p1))) 10e-8)
                                     (< (abs (- (point-y p0) (point-y p1))) 10e-8))
                         collect p1)))
           ;; second step: remove colinear points
           (setf point-seq
                 (let ((n (length point-seq)))
                   (loop for i from 0 below n
                         for p0 = (elt point-seq (mod (+ i -1) n))
                         for p1 = (elt point-seq (mod (+ i 0) n))
                         for p2 = (elt point-seq (mod (+ i +1) n))
                         unless (< (abs (- (* (- (point-x p1) (point-x p0)) (- (point-y p2) (point-y p0)))
                                           (* (- (point-x p2) (point-x p0)) (- (point-y p1) (point-y p0)))))
                                   10e-8)
                         collect p1)))
           ;; third step: care for the orientation
           (if (and (not (null point-seq))
                    (minusp (polygon-orientation point-seq)))
               (reverse point-seq)
               point-seq) ))
  
  (defun shrink-polygon (point-seq width)
    (let ((point-seq (clean-polygon point-seq)))
      (let ((n (length point-seq)))
        (values
         point-seq
         (loop for i from 0 below n
               for p0 = (elt point-seq (mod (+ i -1) n))
               for p1 = (elt point-seq (mod (+ i  0) n))
               for p2 = (elt point-seq (mod (+ i +1) n))
               collect
               (let* ((dx1 (- (point-x p1) (point-x p0))) (dy1 (- (point-y p1) (point-y p0)))
                      (dx2 (- (point-x p2) (point-x p1))) (dy2 (- (point-y p2) (point-y p1)))
                      ;;
                      (m1  (/ width (sqrt (+ (* dx1 dx1) (* dy1 dy1)))))
                      (m2  (/ width (sqrt (+ (* dx2 dx2) (* dy2 dy2)))))
                      ;;
                      (q0  (make-point (+ (point-x p0) (* m1 dy1)) (- (point-y p0) (* m1 dx1))))
                      (q1  (make-point (+ (point-x p1) (* m1 dy1)) (- (point-y p1) (* m1 dx1))))
                      (q2  (make-point (+ (point-x p1) (* m2 dy2)) (- (point-y p1) (* m2 dx2))))
                      (q3  (make-point (+ (point-x p2) (* m2 dy2)) (- (point-y p2) (* m2 dx2)))) )
                 ;;
                 (multiple-value-bind (x y)
                     (multiple-value-call #'line-line-intersection
                       (point-position q0) (point-position q1)
                       (point-position q2) (point-position q3))
                   (if x
                       (make-point x y)
                       (make-point 0 0)))))))))

  (defun draw-bordered-polygon (medium point-seq
                                       &key (border-width 2)
                                       (style :inset))
    (labels ((draw-pieces (outer-points inner-points dark light)
               (let ((n (length outer-points)))
                 (dotimes (i n)
                   (let* ((p1 (elt outer-points (mod (+ i  0) n)))
                          (p2 (elt outer-points (mod (+ i +1) n)))
                          (q1 (elt inner-points (mod (+ i  0) n)))
                          (q2 (elt inner-points (mod (+ i +1) n)))
                          (p1* (transform-region +identity-transformation+  p1))
                          (p2* (transform-region +identity-transformation+  p2))
                          (a (mod (atan (- (point-y p2*) (point-y p1*))
                                        (- (point-x p2*) (point-x p1*)))
                                  (* 2 pi))))
                     (draw-polygon medium (list p1 q1 q2 p2)
                                   :ink
                                   (if (<= (* 1/4 pi) a (* 5/4 pi))
                                       dark light)))))))
      (let ((light  *3d-light-color*)
            (dark   *3d-dark-color*))
      ;;
      (ecase style
        (:solid
         (multiple-value-call #'draw-pieces (shrink-polygon point-seq border-width)
                              +black+ +black+))
        (:inset
         (multiple-value-call #'draw-pieces (shrink-polygon point-seq border-width)
                              dark light))
        (:outset
         (multiple-value-call #'draw-pieces (shrink-polygon point-seq border-width)
                              light dark))
        ;;
        ;; Mickey Mouse is the trademark of the Walt Disney Company.
        ;;
        (:mickey-mouse-outset
         (multiple-value-bind (outer-points inner-points) (shrink-polygon point-seq border-width)
           (declare (ignore outer-points))
           (multiple-value-bind (outer-points middle-points) (shrink-polygon point-seq (/ border-width 2))
             (draw-pieces outer-points middle-points +white+ +black+)
             (draw-pieces middle-points inner-points light dark))))
        (:mickey-mouse-inset
         (multiple-value-bind (outer-points inner-points) (shrink-polygon point-seq border-width)
           (declare (ignore outer-points))
           (multiple-value-bind (outer-points middle-points) (shrink-polygon point-seq (/ border-width 2))
             (draw-pieces outer-points middle-points dark light)
             (draw-pieces middle-points inner-points +black+ +white+))))
        ;;
        (:ridge
         (multiple-value-bind (outer-points inner-points) (shrink-polygon point-seq border-width)
           (declare (ignore outer-points))
           (multiple-value-bind (outer-points middle-points) (shrink-polygon point-seq (/ border-width 2))
             (draw-pieces outer-points middle-points light dark)
             (draw-pieces middle-points inner-points dark light))))
        (:groove
         (multiple-value-bind (outer-points inner-points) (shrink-polygon point-seq border-width)
           (declare (ignore outer-points))
           (multiple-value-bind (outer-points middle-points) (shrink-polygon point-seq (/ border-width 2))
             (draw-pieces outer-points middle-points dark light)
             (draw-pieces middle-points inner-points light dark))))
        (:double
         (multiple-value-bind (outer-points inner-points) (shrink-polygon point-seq border-width)
           (declare (ignore outer-points))
           (multiple-value-bind (outer-points imiddle-points) (shrink-polygon point-seq (* 2/3 border-width))
             (declare (ignore outer-points))
             (multiple-value-bind (outer-points omiddle-points) (shrink-polygon point-seq (* 1/3 border-width))
               (draw-pieces outer-points omiddle-points +black+ +black+)
               (draw-pieces imiddle-points inner-points +black+ +black+))))))))) )

;;
;; gadget sub-classes
;;

(defclass basic-gadget (sheet-parent-mixin sheet-leaf-mixin mirrored-sheet-mixin gadget-color-mixin gadget)
  ;; Half-baked attempt to be compatible with Lispworks.
  ())

(defclass standard-gadget (basic-gadget)
  ())

(defgeneric armed-callback (gadget client gadget-id))
(defgeneric disarmed-callback (gadget client gadget-id))

;; "The default methods (on standard-gadget) call the function stored
;; in gadget-armed-callback or gadget-disarmed-callback with one argument,
;; the gadget."

(defmethod armed-callback ((gadget standard-gadget) client gadget-id)
  (declare (ignore client gadget-id))
  (let ((callback (gadget-armed-callback gadget)))
    (when callback
      (funcall callback gadget))))

(defmethod disarmed-callback ((gadget standard-gadget) client gadget-id)
  (declare (ignore client gadget-id))
  (let ((callback (gadget-disarmed-callback gadget)))
    (when callback
      (funcall callback gadget))))


;; Client warning

; Warning the gadget's client is useful (and made) when a changement has come
(defgeneric warn-client (gadget client))

; Default methods does nothing
; Now, the default client of a gadget is its application-frame (here *application-frame*)
(defmethod warn-client (gadget (frame application-frame))
  (declare (ignorable gadget frame))
  (values))

(defmethod warn-client (gadget (sheet sheet))
  (declare (ignorable gadget sheet))
  (values))

; The moment to warn the client
; [Julien] I'm not sure about this moment, but it works well...
(defmethod handle-event :after ((gadget standard-gadget) (event pointer-button-press-event))
  (when (gadget-client gadget)
    (warn-client gadget (gadget-client gadget))))


;;;
;;; Activation
;;;

(defgeneric activate-gadget (gadget))
(defgeneric deactivate-gadget (gadget))
(defgeneric note-gadget-activated (client gadget))
(defgeneric note-gadget-deactivated (client gadget))

(defmethod activate-gadget ((gadget gadget))
  (with-slots (active-p) gadget
    (unless active-p
      (setf active-p t)
      (note-gadget-activated (gadget-client gadget) gadget))))

(defmethod deactivate-gadget ((gadget gadget))
  (with-slots (active-p) gadget
    (when active-p
      (setf active-p nil)
      (note-gadget-deactivated (gadget-client gadget) gadget))))

(defmethod note-gadget-activated (client (gadget gadget))
  (declare (ignore client))
  ;; Default: do nothing  
  )

(defmethod note-gadget-deactivated (client (gadget gadget))
  (declare (ignore client))
  ;; Default: do nothing
  )

;;;
;;; Value-gadget
;;;

(defclass value-gadget (standard-gadget)
  ((value :initarg :value
          :reader gadget-value)
   (value-changed-callback :initarg :value-changed-callback
                           :initform nil
                           :reader gadget-value-changed-callback)))

(defgeneric (setf gadget-value) (value gadget &key invoke-callback))

(defmethod (setf gadget-value) 
  (value (gadget value-gadget) &key invoke-callback)
  (setf (slot-value gadget 'value) value)
  (when invoke-callback
    (value-changed-callback gadget 
                            (gadget-client gadget) 
                            (gadget-id gadget)
                            value)))

(defgeneric value-changed-callback (value-gadget client gadget-id value))

(defmethod value-changed-callback ((gadget value-gadget) client gadget-id value)
  (declare (ignore client gadget-id))
  (let ((callback (gadget-value-changed-callback gadget)))
    (when callback
      (funcall callback gadget value))))

  
;;;
;;; Action-gadget
;;;

(defclass action-gadget (standard-gadget)
  ((activate-callback :initarg :activate-callback
                      :initform nil
                      :reader gadget-activate-callback)))

(defgeneric activate-callback (action-gadget client gadget-id))

(defmethod activate-callback ((gadget action-gadget) client gadget-id)
  (declare (ignore client gadget-id))
  (let ((callback (gadget-activate-callback gadget)))
    (when callback
      (funcall callback gadget))))

;;;
;;; Oriented-gadget, labelled-gadget, range-gadget
;;;

;; Oriented-gadget

(defclass oriented-gadget ()
  ((orientation :type (member '(:vertical :horizontal))
		:initarg :orientation
                :reader gadget-orientation)))

(defclass oriented-gadget-mixin (oriented-gadget)
  ;; Try to be compatible with Lispworks' CLIM.
  ())


;; Labelled-gadget

(defgeneric draw-label (gadget label x y))

(defclass labelled-gadget ()
  ((label :initarg :label
	  :initform "No label"
          :accessor gadget-label)
   (align-x :initarg :align-x
            :accessor gadget-label-align-x)
   (align-y :initarg :align-y
            :accessor gadget-label-align-y)
   (label-text-style :initform *default-text-style*
		     :initarg :label-text-style
                     :accessor gadget-label-text-style)))

(defmethod compose-space ((pane labelled-gadget))
  (compose-space-aux pane (gadget-label pane)))

(defmethod compose-space-aux ((pane labelled-gadget) (label string))
  (with-sheet-medium (medium pane)
    (multiple-value-bind (width height)
	(text-size medium (gadget-label pane)
		   :text-style (gadget-label-text-style pane))
      ;; FIXME remove explicit values
      ;; instead use spacer pane in derived classes
      (let ((tw (* 1.3 width))
	    (th (* 2.5 height)))
	(make-space-requirement :width tw :height th
				:max-width 400 :max-height 400
				:min-width tw :min-height th)))))

(defmethod draw-label ((pane labelled-gadget) (label string) x y)
  (draw-text* pane label
	      x y
	      :align-x (gadget-label-align-x pane)
	      :align-y (gadget-label-align-y pane)
	      :text-style (gadget-label-text-style pane)))

(defclass labelled-gadget-mixin (labelled-gadget)
  ;; Try to be compatible with Lispworks' CLIM.
  ())

;; LATER: Implement the following: "Changing the label of a gadget
;; may result in invoking the layout protocol on the gadget and its
;; ancestor sheets." (And similarly for changing the alignment or
;; the label text style of a gadget.)


;; Range-gadget

(defclass range-gadget ()
  ((min-value :initarg :min-value
              :accessor gadget-min-value)
   (max-value :initarg :max-value
              :accessor gadget-max-value)))

(defclass range-gadget-mixin (range-gadget)
  ;; Try to be compatible with Lispworks' CLIM.
  ())

(defgeneric gadget-range (range-gadget)
  (:documentation
   "Returns the difference of the maximum and minimum value of RANGE-GADGET."))

(defmethod gadget-range ((gadget range-gadget))
  (- (gadget-max-value gadget)
     (gadget-min-value gadget)))

(defgeneric gadget-range* (range-gadget)
  (:documentation 
   "Returns the minimum and maximum value of RANGE-GADGET as two values."))

(defmethod gadget-range* ((gadget range-gadget))
  (values (gadget-min-value gadget)
          (gadget-max-value gadget)))


;;
;; PUSH-BUTTON gadget
;;

(defclass push-button (labelled-gadget-mixin action-gadget) ())
  
(defclass push-button-pane  (push-button)
  ((show-as-default-p :type boolean
		      :initform nil
		      :initarg :show-as-default-p
		      :accessor push-button-show-as-default-p)))

(defmethod initialize-instance :before ((pane push-button-pane) &rest rest)
  (declare (ignore rest))
  (setf (gadget-label-align-x pane) :center
	(gadget-label-align-y pane) :center))

(defmethod handle-event ((pane push-button-pane) (event pointer-enter-event))
  (with-slots (armed) pane
    (unless armed
      (setf armed t
	    (gadget-current-color pane) (gadget-highlighted-color pane))
      (armed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane push-button-pane) (event pointer-exit-event))
  (with-slots (armed) pane
    (when armed
      (setf armed nil
	    (gadget-current-color pane) (gadget-normal-color pane))
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane push-button-pane) (event pointer-button-press-event))
  (with-slots (armed) pane
    (unless armed
      (armed-callback pane (gadget-client pane) (gadget-id pane)))
    (setf armed ':button-press
	  (gadget-current-color pane) (gadget-pushed-and-highlighted-color pane))))    

(defmethod handle-event ((pane push-button-pane) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when (eql armed ':button-press)
      (activate-callback pane (gadget-client pane) (gadget-id pane))
      (setf armed t
	    (gadget-current-color pane) (gadget-highlighted-color pane))
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane push-button-pane) (event window-repaint-event))
  (dispatch-repaint pane (sheet-region pane)))

(defmethod repaint-sheet ((pane push-button-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
      (declare (type coordinate x1 y1 x2 y2))
      (let ((w (- x2 x1)) 
	    (h (- y2 y1)))
	(display-gadget-background pane (gadget-current-color pane) 0 0 w h)
	(if (eq (slot-value pane 'armed) ':button-press)
	    (progn 
	      (when (push-button-show-as-default-p pane)
		(draw-edges-lines* pane (2- w) (2- h) 0 0))
	      (draw-edges-lines* pane (1- w) (1- h) 0 0))
	    (progn
	      (when (push-button-show-as-default-p pane)
		(draw-edges-lines* pane 0 0 (2- w) (2- h)))
	      (draw-edges-lines* pane 0 0 (1- w) (1- h))))
	(draw-label pane (gadget-label pane) (round w 2) (round h 2))))))


;;
;; TOGGLE-BUTTON gadget
;;

(defclass toggle-button (labelled-gadget-mixin value-gadget) ()
  (:documentation "The value is either t either nil"))

(defclass toggle-button-pane (toggle-button)
  ((indicator-type :type (member '(:one-of :some-of))
		   :initarg :indicator-type
		   :reader toggle-button-indicator-type)))
; We don't have implemented the difference of appearence whether the 
; indicator-type is :one-of or :some-of

(defmethod initialize-instance :before ((pane toggle-button-pane) &rest rest)
  (declare (ignore rest))
  (setf (gadget-label-align-x pane) :center
	(gadget-label-align-y pane) :center))

(defmethod initialize-instance :after ((pane toggle-button-pane) &rest rest)
  (declare (ignore rest))
  (when (gadget-value pane)
    (setf (slot-value pane 'current-color)
	  (gadget-pushed-and-highlighted-color pane))))

(defmethod handle-event ((pane toggle-button-pane) (event pointer-enter-event))
  (with-slots (armed) pane
    (unless armed
      (setf armed t)
      (unless (gadget-value pane)
	(setf (gadget-current-color pane) (gadget-highlighted-color pane)))
      (armed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane toggle-button-pane) (event pointer-exit-event))
  (with-slots (armed) pane
    (when armed
      (setf armed nil)
      (if (gadget-value pane)
	  (setf (gadget-current-color pane) (gadget-pushed-and-highlighted-color pane))
	  (setf (gadget-current-color pane) (gadget-normal-color pane)))
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane toggle-button-pane) (event pointer-button-press-event))
  (with-slots (armed) pane
    (unless armed
      (armed-callback pane (gadget-client pane) (gadget-id pane)))
    (setf armed ':button-press
	  (gadget-current-color pane) (gadget-pushed-and-highlighted-color pane))))
      
(defmethod handle-event ((pane toggle-button-pane) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when (eql armed ':button-press)
      (setf armed t
	    (gadget-value pane :invoke-callback t) (not (gadget-value pane)))
      (unless (gadget-value pane)
	(setf (gadget-current-color pane) (gadget-highlighted-color pane)))
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane toggle-button-pane) (event window-repaint-event))
  (dispatch-repaint pane (sheet-region pane)))

(defmethod repaint-sheet ((pane toggle-button-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let ((region (sheet-region pane))
	  (armed (slot-value pane 'armed)))
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
	(display-gadget-background pane (gadget-current-color pane) x1 y1 x2 y2)
	(if (or (gadget-value pane) (eql armed ':button-press))
	    (draw-edges-lines* pane (1- x2) (1- y2) x1 y1)
	    (draw-edges-lines* pane x1 y1 (1- x2) (1- y2)))
	(draw-label pane (gadget-label pane) (round (- x2 x1) 2) (round (- y2 y1) 2))))))


;;
;; MENU-BUTTON gadget
;;

(defclass menu-button (labelled-gadget-mixin value-gadget)
  ()
  (:documentation "The value is a button"))

(defclass menu-button-pane (menu-button) ())

(defmethod initialize-instance :before ((pane menu-button-pane) &rest rest)
  (declare (ignore rest))
  (setf (gadget-label-align-x pane) :center
	(gadget-label-align-y pane) :center))

(defmethod repaint-sheet ((pane menu-button-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let ((region (sheet-region pane)))
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
	(let ((w (- x2 x1))
	      (h (- y2 y1)))
	  (cond ((slot-value pane 'armed)
		 (draw-rectangle* pane 0 0 w h
				  :ink (gadget-highlighted-color pane)
				  :filled t)
		 (draw-edges-lines* pane (- w 2) (- h 2) 1 1))
		(t
		 (draw-rectangle* pane 0 0 w h
				  :ink (gadget-normal-color pane)
				  :filled t)))
	  (draw-label pane (gadget-label pane) (round w 2) (round h 2)))))))


;;;;
;;;; SCROLL-BAR gadget
;;;;

(defgeneric drag-callback (pane client gadget-id value))
(defgeneric scroll-to-top-callback (scroll-bar client gadget-id))
(defgeneric scroll-to-bottom-callback (scroll-bar client gadget-id))
(defgeneric scroll-up-line-callback (scroll-bar client gadget-id))
(defgeneric scroll-up-page-callback (scroll-bar client gadget-id))
(defgeneric scroll-down-line-callback (scroll-bar client gadget-id))
(defgeneric scroll-down-page-callback (scroll-bar client gadget-id))

(defclass scroll-bar (value-gadget oriented-gadget-mixin range-gadget-mixin)
  ((drag-callback :initarg :drag-callback
		  :initform nil
		  :reader scroll-bar-drag-callback)
   (scroll-to-bottom-callback :initarg :scroll-to-bottom-callback
			      :initform nil
			      :reader scroll-bar-scroll-to-bottom-callback)
   (scroll-to-top-callback :initarg :scroll-to-top-callback
			   :initform nil
			   :reader scroll-bar-scroll-to-top-callback)
   (scroll-down-line-callback :initarg :scroll-down-line-callback
                              :initform nil
                              :reader scroll-bar-scroll-down-line-callback)
   (scroll-up-line-callback :initarg :scroll-up-line-callback
                            :initform nil
                            :reader scroll-bar-scroll-up-line-callback)
   (scroll-down-page-callback :initarg :scroll-down-page-callback
                              :initform nil
                              :reader scroll-bar-scroll-down-page-callback)
   (scroll-up-page-callback :initarg :scroll-up-page-callback
                            :initform nil
                            :reader scroll-bar-scroll-up-page-callback)
   
   (thumb-size :initarg :thumb-size :initform 1/4
               :reader scroll-bar-thumb-size) ))

(defmethod drag-callback ((pane scroll-bar) client gadget-id value)
  (declare (ignore client gadget-id))
  (when (scroll-bar-drag-callback pane)
    (funcall (scroll-bar-drag-callback pane) pane value)))

(defmacro invoke-callbacks (pane callback)
  (let ((call (gensym)))
    `(let ((,call (funcall (symbol-function ,callback) ,pane)))
       (when ,call
	 (funcall ,call ,pane)))))

(defmethod scroll-to-top-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callbacks pane 'scroll-bar-scroll-to-top-callback))

(defmethod scroll-to-bottom-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callbacks pane 'scroll-bar-scroll-to-bottom-callback))

(defmethod scroll-up-line-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callbacks pane 'scroll-bar-scroll-up-line-callback))

(defmethod scroll-up-page-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callbacks pane 'scroll-bar-scroll-up-page-callback))

(defmethod scroll-down-line-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callbacks pane 'scroll-bar-scroll-down-line-callback))

(defmethod scroll-down-page-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callbacks pane 'scroll-bar-scroll-down-page-callback))

;;;;
;;;; SCROLL-BAR-PANE
;;;;

(defclass scroll-bar-pane (basic-pane scroll-bar 3D-border-mixin)
  ((event-state :initform nil)
   (drag-dy :initform nil))
  (:default-initargs :value 0
                     :min-value 0
                     :max-value 1
                     :orientation :vertical
                     :border-width 2
                     :border-style :inset))

(defmethod compose-space ((sb scroll-bar-pane))
  (if (eq (gadget-orientation sb) :vertical)
      (make-space-requirement :min-width 1
			      :width *scrollbar-thickness*
			      :min-height (* 3 *scrollbar-thickness*)
			      :height (* 4 *scrollbar-thickness*))
      (make-space-requirement :min-height 1
			      :height *scrollbar-thickness*
			      :min-width (* 3 *scrollbar-thickness*)
			      :width (* 4 *scrollbar-thickness*))))
;;; Utilities

;; We think all scroll bars as vertically oriented, therefore we have
;; SCROLL-BAR-TRANSFORMATION, which should make every scroll bar
;; look like being vertically oriented -- simplifies much code.

(defmethod scroll-bar-transformation ((sb scroll-bar))
  (ecase (gadget-orientation sb)
    (:vertical   +identity-transformation+)
    (:horizontal (make-transformation 0 1 1 0 0 0))))

(defun translate-range-value (a mina maxa mino maxo)
  "When \arg{a} is some value in the range from \arg{mina} to \arg{maxa},
   proportionally translate the value into the range \arg{mino} to \arg{maxo}."
  (+ mino (* (/ (- a mina) (- maxa mina)) (- maxo mino))))

;;; Scroll-bar's sub-regions

(defmethod scroll-bar-up-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (pane-inner-region sb))
    (declare (ignore maxy))
    (make-rectangle* minx miny
                     maxx (+ miny (- maxx minx)))))

(defmethod scroll-bar-down-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (pane-inner-region sb))
    (declare (ignore miny))
    (make-rectangle* minx (- maxy (- maxx minx))
                     maxx maxy)))

(defmethod scroll-bar-thumb-bed-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (pane-inner-region sb))
    (make-rectangle* minx (+ miny (- maxx minx) 1)
                     maxx (- maxy (- maxx minx) 1))))

(defmethod scroll-bar-thumb-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-thumb-bed-region sb)
    (multiple-value-bind (minv maxv) (gadget-range* sb)
      (multiple-value-bind (v) (gadget-value sb)
        (let ((ts (* 1/3 (- maxv minv))))
          (let ((ya (translate-range-value v minv (+ maxv ts) y1 y2))
                (yb (translate-range-value (+ v ts) minv (+ maxv ts) y1 y2)))
            (make-rectangle* x1 ya x2 yb)))))))

#||
;; alternative:

(defmethod scroll-bar-up-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (sheet-region sb))
    (make-rectangle* (+ minx 2) (- (- maxy (* 2 (- maxx minx))) 2)
                     (- maxx 2) (- (- maxy (- maxx minx)) 2))))

(defmethod scroll-bar-down-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (sheet-region sb))
    (make-rectangle* (+ minx 2) (+ (- maxy (- maxx minx)) 2)
                     (- maxx 2) (-  maxy 2))))

(defmethod scroll-bar-thumb-bed-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (sheet-region sb))
    (make-rectangle* (+ minx 2) (+ miny 2 )
                     (- maxx 2) (- maxy 2 (* 2 (- maxx minx)) 2))))

(defmethod scroll-bar-thumb-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-thumb-bed-region sb)
    (multiple-value-bind (minv maxv) (gadget-range* sb)
      (multiple-value-bind (v) (gadget-value sb)
        (let ((ts (* 1/3 (- maxv minv))))
          (let ((ya (translate-range-value v minv (+ maxv ts) y1 y2))
                (yb (translate-range-value (+ v ts) minv (+ maxv ts) y1 y2)))
            (make-rectangle* x1 ya x2 yb)))))))
||#


;;; Event handlers

(defmethod handle-event ((sb scroll-bar-pane) (event window-repaint-event))
  (dispatch-repaint sb (sheet-region sb)))

#||
(defmethod handle-event ((sb scroll-bar-pane) (event pointer-enter-event))
  (declare (ignorable event))
  (with-slots (armed) sb
     (unless armed
       (setf armed t)
       (armed-callback sb (gadget-client sb) (gadget-id sb)))))

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-exit-event))
  (declare (ignorable event))
  (with-slots (armed) sb
     (when armed
       (setf armed nil)
       (disarmed-callback sb (gadget-client sb) (gadget-id sb)))))
||#

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-button-press-event))
  (multiple-value-bind (x y) (transform-position (scroll-bar-transformation sb)
                                                 (pointer-event-x event) (pointer-event-y event))
    (with-slots (event-state drag-dy) sb
      (cond ((region-contains-position-p (scroll-bar-up-region sb) x y)
             (scroll-up-line-callback sb (gadget-client sb) (gadget-id sb))
             (setf event-state :up-armed)
             (repaint-sheet sb +everywhere+))
            ((region-contains-position-p (scroll-bar-down-region sb) x y)
             (scroll-down-line-callback sb (gadget-client sb) (gadget-id sb))
             (setf event-state :dn-armed)
             (repaint-sheet sb +everywhere+))
            ((region-contains-position-p (scroll-bar-thumb-region sb) x y)
             (setf event-state :dragging
                   drag-dy (- y (bounding-rectangle-min-y (scroll-bar-thumb-region sb)))))
            ((region-contains-position-p (scroll-bar-thumb-bed-region sb) x y)
             (if (< y (bounding-rectangle-min-y (scroll-bar-thumb-bed-region sb)))
                 (scroll-up-page-callback sb (gadget-client sb) (gadget-id sb))
                 (scroll-down-page-callback sb (gadget-client sb) (gadget-id sb))))
            (t
             nil)))))

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-button-release-event))
  (with-slots (event-state) sb
    (case event-state
      (:up-armed (setf event-state nil))
      (:dn-armed (setf event-state nil))
      (otherwise
       (setf event-state nil) )))
  (repaint-sheet sb +everywhere+) )

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-motion-event))
  (multiple-value-bind (x y) (transform-position (scroll-bar-transformation sb)
                                                 (pointer-event-x event) (pointer-event-y event))
    (declare (ignore x))
    (with-slots (event-state drag-dy) sb
      (case event-state
        (:dragging
         (let* ((y-new-thumb-top (- y drag-dy))
                (ts (* 1/3 (- (gadget-max-value sb) (gadget-min-value sb))))
                (new-value (translate-range-value y-new-thumb-top
                                                  (bounding-rectangle-min-y (scroll-bar-thumb-bed-region sb))
                                                  (bounding-rectangle-max-y (scroll-bar-thumb-bed-region sb))
                                                  (gadget-min-value sb)
                                                  (+ (gadget-max-value sb) ts))))
           (drag-callback sb (gadget-client sb) (gadget-id sb)
                          (min (gadget-max-value sb)
                               (max (gadget-min-value sb)
                                    new-value))) ))))))

;;; Repaint

(defmethod repaint-sheet ((sb scroll-bar-pane) region)
  (declare (ignore region))
  (with-special-choices (sb)
    (let ((tr (scroll-bar-transformation sb)))
      (with-bounding-rectangle* (minx miny maxx maxy) (transform-region tr (sheet-region sb))
        (with-drawing-options (sb :transformation tr)
          (draw-rectangle* sb minx miny maxx maxy :filled t
                           :ink *3d-normal-color*)
          ;; draw up arrow
          (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-up-region sb)
            (let ((pg (list (make-point (/ (+ x1 x2) 2) y1)
                            (make-point x1 y2)
                            (make-point x2 y2))))
              (case (slot-value sb  'event-state)
                (:up-armed
                 (draw-polygon sb pg :ink *3d-inner-color*)
                 (draw-bordered-polygon sb pg :style :inset :border-width 2))
                (otherwise
                 (draw-polygon sb pg :ink *3d-normal-color*)
                 (draw-bordered-polygon sb pg :style :outset :border-width 2) ))))

          ;; draw down arrow
          (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-down-region sb)
            (let ((pg (list (make-point (/ (+ x1 x2) 2) y2)
                            (make-point x1 y1)
                            (make-point x2 y1))))
              (case (slot-value sb 'event-state)
                (:dn-armed
                 (draw-polygon sb pg :ink *3d-inner-color*)
                 (draw-bordered-polygon sb pg :style :inset :border-width 2))
                (otherwise
                 (draw-polygon sb pg :ink *3d-normal-color*)
                 (draw-bordered-polygon sb pg :style :outset :border-width 2)))))

          ;; draw thumb
          (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-thumb-region sb)
            (draw-rectangle* sb x1 y1 x2 y2 :ink *3d-normal-color*)
            (draw-bordered-polygon sb
                                   (polygon-points (make-rectangle* x1 y1 x2 y2))
                                   :style :outset
                                   :border-width 2)
            (let ((y (/ (+ y1 y2) 2)))
              (draw-bordered-polygon sb
                                     (polygon-points (make-rectangle* (+ x1 3) (- y 1) (- x2 3) (+ y 1)))
                                     :style :inset
                                     :border-width 1)
              (draw-bordered-polygon sb
                                     (polygon-points (make-rectangle* (+ x1 3) (- y 4) (- x2 3) (- y 2)))
                                     :style :inset
                                     :border-width 1)
              (draw-bordered-polygon sb
                                     (polygon-points (make-rectangle* (+ x1 3) (+ y 4) (- x2 3) (+ y 2)))
                                     :style :inset
                                     :border-width 1))) )))))

;;
;; SLIDER gadget
;;

;; ----------------------------------------------------------
;; What should be done for having a better look for sliders
;;
;; We should find a way to draw the value, when show-value-p
;; is true, in a good position, or to dedicate a particular
;; sheet for this drawing (this sheet would be inside the
;; slider's sheet, probably his child).
;; ----------------------------------------------------------

(defclass slider-gadget (labelled-gadget-mixin
			 value-gadget
			 oriented-gadget-mixin
			 range-gadget-mixin
			 gadget-color-mixin)
  ()
  (:documentation "The value is a real number, and default value for orientation is :vertical, and must never be nil."))
  
(defclass slider-pane (slider-gadget)
  ((drag-callback :initform nil
		  :initarg :drag-callback
		  :reader slider-drag-callback)
   (show-value-p :type boolean
		 :initform nil
		 :initarg :show-value-p
		 :accessor gadget-show-value-p)
   (decimal-places :initform 0
                   :initarg :decimal-places
                   :reader slider-decimal-places)))

;; This values should be changeable by user. That's
;; why they are parameters, and not constants.
(defparameter slider-button-long-dim 30)
(defparameter slider-button-short-dim 10)

(defmethod initialize-instance :before ((pane slider-pane) &rest rest)
  (declare (ignore rest))
  (setf (slot-value pane 'orientation) :vertical))

(defmethod drag-callback ((pane slider-pane) client gadget-id value)
  (declare (ignore client gadget-id))
  (when (slider-drag-callback pane)
    (funcall (slider-drag-callback pane) pane value)))

(defmethod handle-event ((pane slider-pane) (event pointer-enter-event))
  (with-slots (armed) pane
    (unless armed
      (setf armed t))
    (armed-callback pane (gadget-client pane) (gadget-id pane))))

(defmethod handle-event ((pane slider-pane) (event pointer-exit-event))
  (with-slots (armed) pane
    (when armed
      (setf armed nil))
    (disarmed-callback pane (gadget-client pane) (gadget-id pane))))

(defmethod handle-event ((pane slider-pane) (event pointer-button-press-event))
  (with-slots (armed) pane
     (when armed
       (setf armed ':button-press))))       

(defmethod handle-event ((pane slider-pane) (event pointer-motion-event))
  (with-slots (armed) pane
    (when (eq armed ':button-press)
      (let ((value (convert-position-to-value pane
					      (if (eq (gadget-orientation pane) :vertical)
						  (pointer-event-y event)
						  (pointer-event-x event)))))
	(setf (gadget-value pane :invoke-callback nil) value)
	(drag-callback pane (gadget-client pane) (gadget-id pane) value)
	(dispatch-repaint pane (sheet-region pane))))))

(defmethod handle-event ((pane slider-pane) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when armed
      (setf armed t
	    (gadget-value pane :invoke-callback t)
	    (convert-position-to-value pane
				       (if (eq (gadget-orientation pane) :vertical)
					   (pointer-event-y event)
					   (pointer-event-x event))))
      (dispatch-repaint pane (sheet-region pane)))))

(defmethod handle-event ((pane slider-pane) (event window-repaint-event))
  (dispatch-repaint pane (sheet-region pane)))

(defmethod convert-position-to-value ((pane slider-pane) dim)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
    (multiple-value-bind (good-dim1 good-dim2)
	(if (eq (gadget-orientation pane) :vertical)
	    ; vertical orientation
	    (values (+ y1 (ash slider-button-short-dim -1))
		    (- y2 (ash slider-button-short-dim -1)))
	    ; horizontal orientation
	    (values (+ x1 (ash slider-button-short-dim -1))
		    (- x2 (ash slider-button-short-dim -1))))
      (+ (gadget-min-value pane)
	 (/ (* (gadget-range pane) (- (max good-dim1 (min dim good-dim2)) good-dim1))
	    (- good-dim2 good-dim1))))))

(defun format-value (value decimal-places)
  (if (<= decimal-places 0)
      (format nil "~D" (round value))
      (let ((control-string (format nil "~~,~DF" decimal-places)))
        (format nil control-string value))))

(defmethod repaint-sheet ((pane slider-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let ((position (convert-value-to-position pane))
	  (slider-button-half-short-dim (ash slider-button-short-dim -1))
	  (slider-button-half-long-dim (ash slider-button-long-dim -1)))
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
	(display-gadget-background pane (gadget-current-color pane) 0 0 (- x2 x1) (- y2 y1))
	(if (eq (gadget-orientation pane) :vertical)
	    ; vertical case
	    (let ((middle (round (- x2 x1) 2)))
	      (draw-line* pane
			  middle (+ y1 slider-button-half-short-dim)
			  middle (- y2 slider-button-half-short-dim)
			  :ink +black+)
	      (draw-rectangle* pane
			       (- middle slider-button-half-long-dim) (- position slider-button-half-short-dim)
			       (+ middle slider-button-half-long-dim) (+ position slider-button-half-short-dim)
			       :ink +gray85+ :filled t)
	      (draw-edges-lines* pane
				 (- middle slider-button-half-long-dim) (- position slider-button-half-short-dim)
				 (+ middle slider-button-half-long-dim) (+ position slider-button-half-short-dim))
	      (when (gadget-show-value-p pane)
		(draw-text* pane (format-value (gadget-value pane)
                                               (slider-decimal-places pane))
			    5 ;(- middle slider-button-half-short-dim)
			    10))) ;(- position slider-button-half-long-dim))))
	    ; horizontal case
	    (let ((middle (round (- y2 y1) 2)))
	      (draw-line* pane
			  (+ x1 slider-button-half-short-dim) middle
			  (- x2 slider-button-half-short-dim) middle
			  :ink +black+)
	      (draw-rectangle* pane
			       (- position slider-button-half-short-dim) (- middle slider-button-half-long-dim)
			       (+ position slider-button-half-short-dim) (+ middle slider-button-half-long-dim)
			       :ink +gray85+ :filled t)
	      (draw-edges-lines* pane
				 (- position slider-button-half-short-dim) (- middle slider-button-half-long-dim)
				 (+ position slider-button-half-short-dim) (+ middle slider-button-half-long-dim))
	      (when (gadget-show-value-p pane)
		(draw-text* pane (format-value (gadget-value pane)
                                               (slider-decimal-places pane))
			    5 ;(- position slider-button-half-short-dim)
			    (- middle slider-button-half-long-dim)))))))))

(defmethod convert-value-to-position ((pane slider-pane))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
    (multiple-value-bind (good-dim1 good-dim2)
	(if (eq (gadget-orientation pane) :vertical)
	    ; vertical orientation
	    (values (+ y1 (ash slider-button-short-dim -1))
		    (- y2 (ash slider-button-short-dim -1)))
	    ; horizontal orientation
	    (values (+ x1 (ash slider-button-short-dim -1))
		    (- x2 (ash slider-button-short-dim -1))))
      (+ good-dim1 (/ (* (- (gadget-value pane) (gadget-min-value pane))
			 (- good-dim2 good-dim1))
		      (gadget-range pane))))))


;;
;; RADIO-BOX gadget
;;

(defclass radio-box (value-gadget oriented-gadget-mixin) ()
  (:documentation "The value is a button"))
  
(defclass radio-box-pane (radio-box)
  ((current-selection :type 'toggle-button
		      :initarg :current-selection
		      :accessor radio-box-current-selection)))

(defmethod (setf radio-box-current-selection) :before (button (pane radio-box-pane))
  (declare (ignore button))
  (let ((old-button (radio-box-current-selection pane)))
    (setf (gadget-value old-button :invoke-callback t) nil)
    (dispatch-repaint old-button (sheet-region old-button))))

(defmethod warn-client ((gadget toggle-button-pane) (client radio-box-pane))
  (setf (radio-box-current-selection client) gadget))

(defun find-current (&rest l)
    (let (current)
      (loop for element in l
	    do (when (equal (first element) 'radio-box-current-selection)
		 (return (setf current element))))
      (or current (first l))))

(defmacro with-radio-box ((&rest options) &body body)
  (let* ((current (apply #'find-current body))
	 (other-body (remove current body)))
    (when (eql (first current) 'radio-box-current-selection)
      (setf current (second current))) ; first case of the or in find-current
    `(let* ((current-button ,current)
	    (radio-box (make-pane-1 (slot-value *application-frame* 'manager)
				    *application-frame*
				    'radio-box-pane
				    ,@options
				    :current-selection current-button)))
       (mapcar #'(lambda (gadget) (setf (gadget-client gadget) radio-box))
	       (list ,@other-body))
       (mapcar #'(lambda (gadget) (setf (gadget-value gadget :invoke-callback t) nil))
	       (list ,@other-body))
       (setf (gadget-client current-button) radio-box
	     (gadget-value current-button :invoke-callback t) t)
       radio-box)))


;;
;; TEXT-FIELD gadget
;;

(defclass text-field (value-gadget action-gadget) ()
  (:documentation "The value is a string"))

(defclass text-field-pane (text-field) ())

(defmethod initialize-instance :after ((gadget text-field) &rest rest)
  (unless (getf rest :normal)
    (setf (slot-value gadget 'current-color) +white+
	  (slot-value gadget 'normal) +white+)))

(defmethod handle-event ((pane text-field-pane) (event window-repaint-event))
  (dispatch-repaint pane (sheet-region pane)))

(defmethod repaint-sheet ((pane text-field-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
      (display-gadget-background pane (gadget-current-color pane) 0 0 (- x2 x1) (- y2 y1))
      (draw-text* pane (gadget-value pane)
		  (round (- x2 x1))
		  (round (- y2 y1) 2)
		  :align-x :right
		  :align-y :center))))

(defmethod (setf gadget-value) :after (value (pane text-field-pane) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (dispatch-repaint pane (sheet-region pane)))

(defmethod compose-space ((pane text-field-pane))
  (with-sheet-medium (medium pane)
    (multiple-value-bind (width height)
	(text-size medium (gadget-value pane))
      (make-space-requirement :width width :height height
			      :max-width width :max-height height
			      :min-width width :min-height height))))

;;
;; TEXT-EDITOR gadget
;;

(defclass text-editor (text-field) ()
  (:documentation "The value is a string"))

(defclass text-editor-pane (text-editor)
  ((width :type integer
	  :initarg :width
	  :initform 300
	  :reader text-editor-width)
   (height :type integer
	   :initarg :height
	   :initform 300
	   :reader text-editor-height)))

(defmethod compose-space ((pane text-editor-pane))
  (let ((width (text-editor-width pane))
	(height (text-editor-height pane)))
  (make-space-requirement :width width
			  :min-width width
			  :max-width width
			  :height height
			  :min-height height
			  :max-height height)))


;;
;; GADGET-OUTPUT-RECORD
;;

(defclass gadget-output-record (output-record) ())

(defmacro with-output-as-gadget (stream &body body)
  (declare (type symbol stream))
  (when (eq stream t)
    (setq stream '*standard-output*))
  (let ((gadget (gensym))
	(gadget-output-record (gensym)))
    `(let* ((,gadget (progn ,@body))
	    (,gadget-output-record (make-instance 'gadget-output-record 
				    :children (list ,gadget))))
       (stream-add-output-record ,stream ,gadget-output-record)
       ,gadget)))
