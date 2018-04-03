;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2001,2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2001 by
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;  (c) copyright 2002, 2003 by
;;;           Gilbert Baumann <unk6@rz.uni-karlsruhe.de>

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

;;; $Id: panes.lisp,v 1.197 2009/08/01 22:11:06 gbaumann Exp $

(in-package :clim-internals)

;;;;
;;;; Ambiguities and Obmissions
;;;;

;; This is a scratch pad, were we can document, what the spec doesn't
;; tells us about CLIM. Reason: While coding, one sees were the spec
;; is vague or wrong; later when the task to update the spec is due,
;; things might be forgotten. --GB

;;
;; - Default of :equalize-width / :equalize-height is T
;;
;; - LAYOUT-PANE is mentioned in the spec's example, but not in the
;;   text.
;;
;; - Behaviour of :align-x, :align-y is uncertain.
;;   (Should it be specifed on the childs? on the parents?)
;;
;; - BORDER-PANE is not in the spec and just a different name of
;;   OUTLINED-PANE, where is it from? --GB
;;
;; - RAISED-PANE, where form? --GB
;;

;; - In XBOX-PANE: I would like to also allow for (1 <pane>) being a
;;   proportional content.


;;;; TODO

;; - VBOX/HBOX/VRACK/HRACK:
;;   . should align its children
;;     Q: Should we cope with proportional content differently?
;;   . test units for spacing and fixed width
;;     Q: When to resolve?
;;   . adopt/disown/enable/disable
;;
;; - TABLE-PANE
;;   . test units
;;   . adopt/disown/enable/disable
;;   . allow for partially filled rows/cols?
;;
;; - GRID-PANE
;;   . align children
;;   . test units
;;   . adopt/disown/enable/disable
;;
;; - SPACING-PANE
;;   . align child
;;     Or: expand them as we did?
;;   . adopt/disown/enable/disable
;;
;; - RESTRAINING-PANE
;;   . ???
;;
;; - LABEL-PANE
;;   . test units
;;   . adopt/disown/enable/disable
;;   . expand child? leave it?
;;
;; - SCROLLER-PANE
;;   . much!
;;
;; - we still need to think about what should happen when children
;;   get disabled or adopted or disowned.
;;
;; - adjust class names.
;;
;; - advertise layout-child et al
;;
;; - reuse single-child-composite-pane
;;
;; - MAKE-SPACE-REQUIREMENT right?
;;   . default arguments in the spec are different
;;   . DUIM's default for maxima is not +fill+ but the dimension
;;
;; - what are the appropriate default values for align?
;;

;; - for layout purposes the list of children should be considered in
;;   reverse: The first element of children should come last.

;;--GB 2002-02-27

;;;; CLIM Layout Protocol for Dummies

;; Here is how I interpret the relevant sections of the specification:
;;
;; COMPOSE-SPACE
;;
;;   This is called by CLIM, when it wants to find out what the pane
;;   thinks are its space requirements. The result of COMPOSE-SPACE is
;;   cached by CLIM.
;;
;; ALLOCATE-SPACE
;;
;;   This method is called by CLIM when a pane is allocate space. It
;;   should layout its possible children.
;;
;; CHANGE-SPACE-REQUIREMENTS
;;
;;   This is called by the application programmer to a) indicate that
;;   COMPOSE-SPACE may now return something different from previous
;;   invocations and/or b) to update the user space requirements
;;   options (the :width, :height etc keywords as upon pane creation).
;;
;; NOTE-SPACE-REQUIREMENTS-CHANGED
;;
;;   Called by CLIM when the space requirements of a pane have
;;   changed. Not called to layout a pane; This is only a kind of signal.
;;
;; LAYOUT-FRAME
;;
;;   Maybe called by both CLIM and the application programmer to
;;   "invoke the space allocation protocol", that is CLIM calls
;;   ALLOCATE-SPACE on the top level sheet. This in turn will probably
;;   call COMPOSE-SPACE on its children and layout then accordingly by
;;   calling ALLOCATE-SPACE again.
;;
;;   The effect is that ALLOCATE-SPACE propagate down the sheet
;;   hierarchy.
;;
;; --GB 2003-08-06

;; For each of the builtin CLIM gadgets there is an abstract gadget class
;; and at least one "concrete" subclass which can be chosen by the
;; frame manager. The CLIM 2.0 spec names one concrete class for each
;; abstract class. Frame managers need a mechanism to look up these
;; concrete classes. The current practice of the CLX backend is to
;; search for classes of various names based on the name of the abstract
;; class. This mostly works as all but two of the specified concrete
;; class names can be produced by appending "-PANE" to the abstract class
;; name. The classes GENERIC-LIST-PANE and GENERIC-OPTION-PANE break this
;; convention.

;; I've extended the CLX frame manager to additionally search the property
;; list of the pane class name when searching for a concrete pane class. The
;; function below can be used where needed to place the concrete class name
;; where it needs to go.

;; This could be easily extended to allow mappings for specific backends..

(defun define-abstract-pane-mapping (abstract-class-name concrete-class-name)
  (setf (get abstract-class-name 'concrete-pane-class-name)
        concrete-class-name))



;;; Gadget "Feel"

(defparameter *double-click-delay* 0.25
  "Maximum time in seconds between clicks in order to produce a double-click")

(defparameter *double-click-max-travel* 7
  "Maximum distance in device units that the cursor may move between clicks in 
order to produce a double-click")

;;;
;;; gadgets look
;;;

;; Only used by some gadgets, I suggest using my more flexible and
;; general DRAW-BORDERED-POLYGON.

(defun display-gadget-background (gadget color x1 y1 x2 y2)
  (draw-rectangle* gadget x1 y1 x2 y2 :ink color :filled t))

(defun draw-edges-lines* (pane ink1 x1 y1 ink2 x2 y2)
  (draw-line* pane x1 y1 x2 y1 :ink ink1)
  (draw-line* pane x1 y1 x1 y2 :ink ink1)
  (draw-line* pane x1 y2 x2 y2 :ink ink2)
  (draw-line* pane x2 y1 x2 y2 :ink ink2))


;;; Space Requirements

(defconstant +fill+ (expt 10 (floor (log most-positive-fixnum 10))))

(defclass space-requirement () ())

(defclass standard-space-requirement (space-requirement)
  ((width      :initform 1
	       :initarg :width
	       :reader space-requirement-width)
   (max-width  :initform 1
	       :initarg :max-width
	       :reader space-requirement-max-width)
   (min-width  :initform 1
	       :initarg :min-width
	       :reader space-requirement-min-width)
   (height     :initform 1
	       :initarg :height
	       :reader space-requirement-height)
   (max-height :initform 1
	       :initarg :max-height
	       :reader space-requirement-max-height)
   (min-height :initform 1
	       :initarg :min-height
	       :reader space-requirement-min-height) ) )

(defmethod print-object ((space standard-space-requirement) stream)
  (with-slots (width height min-width max-width min-height max-height) space
    (print-unreadable-object (space stream :type t :identity nil)
      (format stream "width: ~S [~S,~S] height: ~S [~S,~S]"
              width
              min-width
              max-width
              height
              min-height
              max-height))))

(defun make-space-requirement (&key (min-width 0) (min-height 0)
                                    (width min-width) (height min-height)
				    (max-width +fill+) (max-height +fill+))
  (assert (<= 0 min-width width max-width) (min-width width max-width))
  (assert (<= 0 min-height height max-height) (min-height height max-height))
  (make-instance 'standard-space-requirement
    :width width
    :max-width max-width
    :min-width min-width
    :height height
    :max-height max-height
    :min-height min-height))

(defmethod space-requirement-components ((space-req standard-space-requirement))
  (with-slots (width min-width max-width height min-height max-height) space-req
    (values width min-width max-width height min-height max-height)))

(defun space-requirement-combine* (function sr1 &key (width 0) (min-width 0) (max-width 0)
                                                (height 0) (min-height 0) (max-height 0))
  (apply #'make-space-requirement
         (mapcan #'(lambda (c1 c2 keyword)
                     (list keyword (funcall function c1 c2)))
                 (multiple-value-list (space-requirement-components sr1))
                 (list width min-width max-width height min-height max-height)
                 '(:width :min-width :max-width :height :min-height :max-height))))

(defun space-requirement-combine (function sr1 sr2)
  (multiple-value-bind (width min-width max-width height min-height max-height)
      (space-requirement-components sr2)
    (space-requirement-combine* function sr1
                                :width      width
                                :min-width  min-width
                                :max-width  max-width
                                :height     height
                                :min-height min-height
                                :max-height max-height)))

(defun space-requirement+ (sr1 sr2)
  (space-requirement-combine #'+ sr1 sr2))

(defun space-requirement+* (space-req &key (width 0) (min-width 0) (max-width 0)
                                           (height 0) (min-height 0) (max-height 0))
  (space-requirement-combine* #'+ space-req
                              :width      width
                              :min-width  min-width
                              :max-width  max-width
                              :height     height
                              :min-height min-height
                              :max-height max-height))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun spacing-value-p (x)
  (or (and (realp x) (>= x 0))
      (and (consp x)
           (realp (car x))
           (consp (cdr x))
           (member (cadr x) '(:point :pixel :mm :character :line))
           (null (cddr x)))
      ;; For clim-stream-pane
      (eq x :compute)))
)

(deftype spacing-value ()
  ;; just for documentation
  `(satisfies spacing-value-p))

;;; PANES

;; Macros for quick access to space-requirement slots.
(defmacro sr-width (pane)
  `(space-requirement-width (pane-space-requirement ,pane)))
(defmacro sr-height (pane)
  `(space-requirement-height (pane-space-requirement ,pane)))
(defmacro sr-max-width (pane)
  `(space-requirement-max-width (pane-space-requirement ,pane)))
(defmacro sr-max-height (pane)
  `(space-requirement-max-height (pane-space-requirement ,pane)))
(defmacro sr-min-width (pane)
  `(space-requirement-min-width (pane-space-requirement ,pane)))
(defmacro sr-min-height (pane)
  `(space-requirement-min-height (pane-space-requirement ,pane)))

(defclass layout-protocol-mixin ()
  ((space-requirement :accessor pane-space-requirement
                      :initform nil
                      :documentation "The cache of the space requirements of the pane. NIL means: need to recompute.")
   (current-width     :accessor pane-current-width
                      :initform nil)
   (current-height    :accessor pane-current-height
                      :initform nil) ))

;;; XXX Move to protocol-classes.lisp. Should this really have all these
;;; superclasses? 
(define-protocol-class pane (clim-repainting-mixin
			     clim-sheet-input-mixin
			     sheet-transformation-mixin
			     layout-protocol-mixin
			     basic-sheet)
  (
   (text-style :initarg :text-style :initform nil :reader pane-text-style)
   (name :initarg :name :initform "(Unnamed Pane)" :reader pane-name)
   (manager :initarg :manager)
   (port :initarg :port)
   (frame :initarg :frame :initform *application-frame* :reader pane-frame)
   (enabledp :initform nil :initarg :enabledp :accessor pane-enabledp)
   (space-requirement :initform nil :accessor pane-space-requirement)
   ;; New sizes, for allocating protocol
   (new-width :initform nil)
   (new-height :initform nil)
   (redisplay-needed :accessor pane-redisplay-needed
		     :initarg :redisplay-needed :initform nil))
  (:documentation ""))

(defmethod print-object ((pane pane) sink)
  (print-unreadable-object (pane sink :type t :identity t)
    (prin1 (pane-name pane) sink)))

(defun make-pane (type &rest args)
  (when (eql (symbol-package type)
             (symbol-package :foo))
    (setf type (or (find-symbol (symbol-name type) (find-package :clim))
                   type)))
  (apply #'make-pane-1 (or *pane-realizer*
			   (frame-manager *application-frame*))
	 *application-frame* type args))

(defmethod medium-foreground ((pane pane))
  (medium-foreground (sheet-medium pane)))

(defmethod (setf medium-foreground) (ink (pane pane))
  (setf (medium-foreground (sheet-medium pane)) ink))

(defmethod medium-background ((pane pane))
  (medium-background (sheet-medium pane)))

(defmethod (setf medium-background) (ink (pane pane))
  (setf (medium-background (sheet-medium pane)) ink))

(defmethod compose-space ((pane pane) &key width height)
  (make-space-requirement :width (or width 200)
			  :height (or height 200)))

(defmethod allocate-space ((pane pane) width height)
  (declare (ignorable pane width height))
  )

(defmethod pane-needs-redisplay ((pane pane))
  (let ((do-redisplay (pane-redisplay-needed pane)))
    (values do-redisplay
	    (and do-redisplay (not (eq do-redisplay :no-clear))))))

(defmethod (setf pane-needs-redisplay) (value (pane pane))
  (setf (pane-redisplay-needed pane) value))

(defmethod window-clear ((pane pane))
  nil)


;;;
;;; Utilities 
;;;

;; Since, I hate to duplicate code for HBOX and VBOX, I define this
;; evil macro:

(defmacro dada ((&rest substs) &body body)
  "This is an evil macro."
  (setf substs (sort substs #'> :key (lambda (s) (length (symbol-name (first s))))))
  `(progn
     ,@(loop for k from 1 below (length (first substs)) collect
             (labels ((subst-one (new old sym)
                        (let ((p (search (symbol-name old) (symbol-name sym))))
                          (cond ((not (null p))
                                 (let ((pack (if (eq (symbol-package sym)
                                                     (find-package :keyword))
                                                 (symbol-package sym)
                                               *package*)))
                                   (intern (concatenate 'string
                                             (subseq (symbol-name sym) 0 p)
                                             (symbol-name new)
                                             (subseq (symbol-name sym)
                                                     (+ p (length (symbol-name old)))))
                                           pack)))
                                (t
                                 sym))))
                      (walk (x)
                        (cond ((symbolp x)
                               (dolist (subst substs)
                                 (setf x (subst-one (elt subst k) (first subst) x)))
                               x)
                              ((atom x) x)
                              ((consp x)
                               (cons (walk (car x)) (walk (cdr x)))))))
               `(locally
                  ,@(walk body))))))

;;;; Layout Utilities

(defun layout-child (child align-x align-y x y width height)
  "Allocates space to a child of a pane. 
   x, y, width, height designate the area of available space.
   align-x, align-y name the desired child alignment.
   If the child does not have enough strechability to cover all of the
   given area, it is aligned within that area according to the given
   options.

   As a special option we allow align-x or align-y be :expand, which
   means that the child wouldn't be aligned in that direction but its
   size would be forced."
  (let* ((sr           (compose-space child))
         ;; The child's dimension is clamped within its min/max space requirement
         (child-width  (if (eql :expand align-x)
                           width
                           (clamp width
                                  (space-requirement-min-width sr)
                                  (space-requirement-max-width sr))))
         (child-height (if (eql :expand align-y)
                           height
                           (clamp height
                                  (space-requirement-min-height sr)
                                  (space-requirement-max-height sr))))
         ;; Align the child within the available area
         (child-x      (ecase align-x
                         ((:left)   x)
                         ((:center) (+ x (/ (- width child-width) 2)))
                         ((:right)  (+ x (- width child-width)))
                         ((:expand)  x) ))
         (child-y      (ecase align-y
                         ((:top)    y)
                         ((:center) (+ y (/ (- height child-height) 2)))
                         ((:bottom) (+ y (- height child-height)))
                         ((:expand)  y) )))
    ;; Actually layout the child
    (move-sheet child child-x child-y)
    (resize-sheet child child-width child-height)
    (allocate-space child child-width child-height)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User Space Requirements

(defclass space-requirement-options-mixin ()
  ((user-width       
    :initarg  :width
    :initform nil
    :reader   pane-user-width
    :type     (or null spacing-value))
   (user-min-width 
    :initarg :min-width
    :initform nil
    :reader   pane-user-min-width
    :type     (or null spacing-value))
   (user-max-width
    :initarg :max-width
    :initform nil
    :reader   pane-user-max-width
    :type     (or null spacing-value))
   (user-height
    :initarg :height
    :initform nil
    :reader   pane-user-height
    :type     (or null spacing-value))
   (user-min-height
    :initarg :min-height
    :initform nil
    :reader   pane-user-min-height
    :type     (or null spacing-value))
   (user-max-height
    :initarg :max-height
    :initform nil
    :reader   pane-user-max-height
    :type     (or null spacing-value))
   (x-spacing
    :initarg :x-spacing
    :initform 0
    :reader   pane-x-spacing
    :type     (or null spacing-value))
   (y-spacing
    :initarg :y-spacing
    :initform 0
    :reader   pane-y-spacing
    :type     (or null spacing-value)))
  (:documentation
   "Mixin class for panes which offer the standard user space requirements options."))

(defclass standard-space-requirement-options-mixin (space-requirement-options-mixin)
  ())

(defgeneric spacing-value-to-device-units (pane x))

(defun merge-one-option
    (pane foo user-foo user-min-foo user-max-foo min-foo max-foo)
  

  ;; NOTE: The defaulting for :min-foo and :max-foo is different from MAKE-SPACE-REQUIREMENT.
  ;;       MAKE-SPACE-REQUIREMENT has kind of &key foo (min-foo 0) (max-foo +fill+)
  ;;       While user space requirements has &key foo (min-foo foo) (max-foo foo).
  ;;       I as a user would pretty much expect the same behavior, therefore I'll take the
  ;;       following route:
  ;;       When the :foo option is given, I'll let MAKE-SPACE-REQUIREMENT decide.
  ;;   
  ;; old code:
  ;;
  ;; ;; Then we resolve defaulting. sec 29.3.1 says:
  ;; ;; | If either of the :max-width or :min-width options is not
  ;; ;; | supplied, it defaults to the value of the :width option. If
  ;; ;; | either of the :max-height or :min-height options is not
  ;; ;; | supplied, it defaults to the value of the :height option.
  ;; (setf user-max-foo  (or user-max-foo user-foo)
  ;;       user-min-foo  (or user-min-foo user-foo))
  ;;       --GB 2003-01-23

  (when (and (null user-max-foo) (not (null user-foo)))
    (setf user-max-foo (space-requirement-max-width
			(make-space-requirement
			 :width (spacing-value-to-device-units pane foo)))))
  (when (and (null user-min-foo) (not (null user-foo)))
    (setf user-min-foo (space-requirement-min-width
			(make-space-requirement
			 :width (spacing-value-to-device-units pane foo)))))
	    
  ;; when the user has no idea about the preferred size just take the
  ;; panes preferred size.
  (setf user-foo (or user-foo foo))
  (setf user-foo (spacing-value-to-device-units pane user-foo))

  ;; dito for min/max
  (setf user-min-foo (or user-min-foo min-foo)
	user-max-foo (or user-max-foo max-foo))
	    
  ;; | :max-width, :min-width, :max-height, and :min-height can
  ;; | also be specified as a relative size by supplying a list of
  ;; | the form (number :relative). In this case, the number
  ;; | indicates the number of device units that the pane is
  ;; | willing to stretch or shrink.
  (labels ((resolve-relative (dimension sign base)
	     (if (and (consp dimension) (eq (car dimension) :relative))
		 (+ base (* sign (cadr dimension)))
		 (spacing-value-to-device-units pane dimension))))
    (setf user-min-foo (and user-min-foo
			    (resolve-relative user-min-foo  -1 user-foo))
	  user-max-foo (and user-max-foo
			    (resolve-relative user-max-foo  +1 user-foo))))
	    
  ;; Now we have two space requirements which need to be 'merged'.
  (setf min-foo (clamp user-min-foo min-foo max-foo)
	max-foo (clamp user-max-foo min-foo max-foo)
	foo     (clamp user-foo min-foo max-foo))
  (values foo min-foo max-foo))

(defgeneric merge-user-specified-options (pane sr))

(defmethod merge-user-specified-options ((pane space-requirement-options-mixin)
					 sr)
  ;; ### I want proper error checking and in case there is an error we 
  ;;     should just emit a warning and move on. CLIM should not die from
  ;;     garbage passed in here.
  (multiple-value-bind (width min-width max-width height min-height max-height)
		       (space-requirement-components sr)
    (multiple-value-bind (new-width new-min-width new-max-width)
	(merge-one-option pane
			  width
			  (pane-user-width pane)
			  (pane-user-min-width pane)
			  (pane-user-max-width pane)
			  min-width
			  max-width)
      (multiple-value-bind (new-height new-min-height new-max-height)
	  (merge-one-option pane
			    height
			    (pane-user-height pane)
			    (pane-user-min-height pane)
			    (pane-user-max-height pane)
			    min-height
			    max-height)
	(make-space-requirement
	 :width      new-width
	 :min-width  new-min-width
	 :max-width  new-max-width
	 :height     new-height
	 :min-height new-min-height
	 :max-height new-max-height)))))


(defmethod compose-space :around ((pane space-requirement-options-mixin)
                                  &key width height)
  (declare (ignore width height))
  ;; merge user specified options.
  (let ((sr (call-next-method)))
    (unless sr
      (warn "~S has no idea about its space-requirements." pane)
      (setf sr (make-space-requirement :width 100 :height 100)))
    (merge-user-specified-options pane sr)))

(defmethod change-space-requirements :before ((pane space-requirement-options-mixin)
                                              &key (width :nochange) (min-width :nochange) (max-width :nochange)
                                                   (height :nochange) (min-height :nochange) (max-height :nochange)
                                                   (x-spacing :nochange) (y-spacing :nochange)
                                              &allow-other-keys)
  (with-slots (user-width user-min-width user-max-width
               user-height user-min-height user-max-height
               (user-x-spacing x-spacing)
               (user-y-spacing y-spacing))
      pane
    (unless (eq width      :nochange) (setf user-width      width))
    (unless (eq min-width  :nochange) (setf user-min-width  min-width))
    (unless (eq max-width  :nochange) (setf user-max-width  max-width))
    (unless (eq height     :nochange) (setf user-height     height))
    (unless (eq min-height :nochange) (setf user-min-height min-height))
    (unless (eq max-height :nochange) (setf user-max-height max-height))
    (unless (eq x-spacing  :nochange) (setf user-x-spacing  x-spacing))
    (unless (eq y-spacing  :nochange) (setf user-y-spacing  y-spacing)) ))

;;;; LAYOUT-PROTOCOL-MIXIN

;;; Note

;;; This is how I read the relevant section of the specification:
;;;
;;; - space is only allocated / composed when the space allocation
;;;   protocol is invoked, that is when layout-frame is called.
;;;
;;; - CHANGE-SPACE-REQUIREMENTS is only for
;;;   . reparsing the user space options
;;;   . flushing the space requirement cache of that pane.
;;;
;;; - when within CHANGING-SPACE-REQUIREMENTS, the method for
;;;   CHANGING-SPACE-REQUIREMENTS on the top level sheet should not
;;;   invoke the layout protocol but remember that the SR of the frame
;;;   LAYOUT-FRAME then is then called when leaving
;;;   CHANGING-SPACE-REQUIREMENTS.
;;;
;;; - NOTE-SPACE-REQUIREMENTS-CHANGED is solely for the user.
;;;
;;; --GB 2003-03-16

(defmethod allocate-space :around ((pane layout-protocol-mixin) width height)
  (setf (pane-current-width pane) width
	(pane-current-height pane) height)
  (unless (top-level-sheet-pane-p pane)
    (resize-sheet pane width height))
  (call-next-method))

(defmethod compose-space :around ((pane layout-protocol-mixin) &key width height)
  (declare (ignore width height))
  (or (pane-space-requirement pane)
      (setf (pane-space-requirement pane)
            (call-next-method))))

;;; changing space requirements

;;; Here is what we do:
;;;
;;; change-space-requirements (pane) :=
;;;   clear space requirements cache
;;;   call change-space-requirements on parent pane
;;;   call note-space-requirements-changed
;;;
;;; This is split into :before, primary and :after method to allow for
;;; easy overriding of change-space-requirements without needing to
;;; know the details of the space requirement cache and the
;;; note-space-requirements-changed notifications.
;;;
;;; The calls to change-space-requirements travel all the way up to
;;; the top-level-sheet-pane which then invokes the layout protocol
;;; calling layout-frame.
;;;
;;; In case this happens within changing-space-requirements layout
;;; frame is not called but simply recorded and then called when
;;; changing-space-requirements is left.
;;;
;;; No action is taken in note-space-requirements-changed. We leave
;;; that to the user.

(defvar *changing-space-requirements* nil
  "Bound to non-NIL while within the execution of CHANGING-SPACE-REQUIREMENTS.")

(defvar *changed-space-requirements* nil
  "A list of (frame pane resize-frame) tuples recording frames and their panes
which changed during the current execution of CHANGING-SPACE-REQUIREMENTS.
[This is expected to change]")

(defmethod change-space-requirements :before ((pane layout-protocol-mixin)
                                              &rest space-req-keys
                                              &key resize-frame &allow-other-keys)
  (declare (ignore resize-frame space-req-keys))
  ;; Clear the space requirements cache
  (setf (pane-space-requirement pane) nil)
  (setf (pane-current-width pane) nil)
  (setf (pane-current-height pane) nil) )

(defmethod change-space-requirements ((pane layout-protocol-mixin)
                                      &key resize-frame &allow-other-keys)
  (when (sheet-parent pane)
    (change-space-requirements (sheet-parent pane)
			       :resize-frame resize-frame)))

(defmethod change-space-requirements :after ((pane layout-protocol-mixin)
                                             &key resize-frame &allow-other-keys)
  (declare (ignore resize-frame))
  (note-space-requirements-changed (sheet-parent pane) pane))

(defmethod note-space-requirements-changed (pane client)
  "Just a no-op fallback method."
  nil)

;;; CHANGING-SPACE-REQUIREMENTS macro

(defmacro changing-space-requirements ((&key resize-frame layout) &body body)
  `(invoke-with-changing-space-requirements (lambda () ,@body) :resize-frame ,resize-frame :layout ,layout))

(defun invoke-with-changing-space-requirements (continuation
						&key resize-frame layout)
  (cond (*changed-space-requirements*
         ;; We are already within changing-space-requirements, so just
         ;; call the body. This might however lead to surprising
         ;; behavior in case the outer changing-space-requirements has
         ;; resize-frame = NIL while the inner has resize-frame = T.
         (funcall continuation))
        (t
         (let ((*changed-space-requirements* nil))
           (let ((*changing-space-requirements* t))
             (funcall continuation))
           ;;
           ;; Note: That 'resize-frame' and especially 'layout' are
           ;; options to this strongly suggests that the authors of
           ;; the clim specification may have meant that
           ;; changing-space-requirements records space requirements
           ;; of the *application-frame* only.
           ;;
           ;; We solve this by recording all frames but applying
           ;; resize-frame and layout only to *application-frame*.
           ;;
           (dolist (q *changed-space-requirements*)
             (destructuring-bind (frame pane resize-frame-2) q
               (cond ((eq frame *application-frame*)
                      (when layout
                        (setf (frame-current-layout frame) layout))
                      (cond (resize-frame
                             (layout-frame frame))
                            (t
                             (layout-frame frame
                                           (bounding-rectangle-width pane)
                                           (bounding-rectangle-height pane)))))
                     (t
                      (cond (resize-frame-2
                             (layout-frame frame))
                            (t
                             (layout-frame frame
                                           (bounding-rectangle-width pane)
                                           (bounding-rectangle-height pane))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BASIC PANE

(defclass basic-pane (standard-space-requirement-options-mixin
                      sheet-parent-mixin ;mirrored-sheet-mixin
                      ;; UX mixins
                      cut-and-paste-mixin
                      mouse-wheel-scroll-mixin
                      ;; protocol class with million mixins goes last
                      pane)
  ((foreground       :initarg :foreground
                     :reader pane-foreground)
   (background       :initarg :background
                     :reader pane-background)
   (text-style       :initarg :text-style
                     :reader pane-text-style)
   (align-x          :initarg :align-x
                     :type (member :left :center :right)
                     :reader pane-align-x)
   (align-y          :initarg :align-y
                     :type (member :top :center :bottom)
                     :reader pane-align-y))
  (:default-initargs
   :foreground +black+
   :background *3d-normal-color*
   :text-style *default-text-style*
   :align-x :left
   :align-y :top))

(defmethod initialize-instance :after ((obj basic-pane) &key text-style)
  (when (consp text-style)
    (setf (slot-value obj 'text-style) (apply #'make-text-style text-style))))

(defmethod engraft-medium :after (medium port (pane basic-pane))
  (declare (ignore port))
  ;; implements 29.2.2, last sentence.
  (setf (medium-foreground medium) (pane-foreground pane)
        (medium-background medium) (pane-background pane)
        (medium-text-style medium) (pane-text-style pane)))

;;;;
;;;; Composite Panes
;;;;

(defclass composite-pane (sheet-multiple-child-mixin
			  basic-pane)
  ()
  (:documentation "protocol class"))

(defmethod spacing-value-to-device-units (pane x)
  (cond ((realp x) x)
        ((consp x)
         (ecase (cadr x)
           (:pixels (car x))
           (:point  (* (car x) (graft-pixels-per-inch (graft pane)) 1/72))
           (:mm     (* (car x) (graft-pixels-per-millimeter (graft pane))))
           (:character (* (car x) (text-style-character-width (pane-text-style pane)
                                                               (sheet-medium pane)
                                                               #\m)))
           (:line  (* (car x)
                      (stream-line-height pane)))))))

;;; SINGLE-CHILD-COMPOSITE PANE

(defclass single-child-composite-pane (sheet-single-child-mixin basic-pane) ())


(defmethod initialize-instance :after ((pane single-child-composite-pane)
				       &rest args
				       &key contents
				       &allow-other-keys)
  (declare (ignore args))
  (when contents
    (when (cdr contents)
      (error 'sheet-supports-only-one-child :sheet pane))
    (sheet-adopt-child pane (first contents))))

(defmethod compose-space ((pane single-child-composite-pane) &key width height)
  (if (sheet-child pane)
      (compose-space (sheet-child pane)
                     :width width :height height)
      (make-space-requirement)))

(defmethod allocate-space ((pane single-child-composite-pane) width height)
  (when (sheet-child pane)
    (allocate-space (sheet-child pane) width height)))

;;; TOP-LEVEL-SHEET

(defclass top-level-sheet-pane (;;permanent-medium-sheet-output-mixin
				;;mirrored-sheet-mixin
                                single-child-composite-pane)
  ()
  (:documentation "For the first pane in the architecture"))

(defun top-level-sheet-pane-p (pane)
  (typep pane 'top-level-sheet-pane))

(defmethod change-space-requirements ((pane top-level-sheet-pane)
                                      &rest space-req-keys
                                      &key resize-frame &allow-other-keys)
  (declare (ignore space-req-keys))
  (cond (*changing-space-requirements*
         ;; Record changed space requirements.
         ;; What happens if we change the requirements successively
         ;; with different values? Only the first takes effect?
         ;; -Hefner
         (unless (find pane *changed-space-requirements* :key #'second)
           (push (list (pane-frame pane) pane resize-frame)
                 *changed-space-requirements*)))
        (t
         (let ((frame (pane-frame pane)))
           ;; ### we miss the :resize-frame option
           (cond (resize-frame
                  (layout-frame frame))
                 (t
                  (layout-frame frame
                                (bounding-rectangle-width pane)
                                (bounding-rectangle-height pane))))))))

(defmethod compose-space ((pane top-level-sheet-pane) &key width height)
  (declare (ignore width height))
  (compose-space (sheet-child pane)))

(defmethod allocate-space ((pane top-level-sheet-pane) width height)
  (unless (pane-space-requirement pane)
    (setf (pane-space-requirement pane)
	  (compose-space pane)))
  (alexandria:when-let ((child (sheet-child pane)))
    (allocate-space child
		    (clamp width  (sr-min-width pane)  (sr-max-width pane))
		    (clamp height (sr-min-height pane) (sr-max-height pane)))))

(defmethod note-sheet-region-changed :after ((pane top-level-sheet-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
    (allocate-space pane (- x2 x1) (- y2 y1))))

(defmethod handle-event ((sheet top-level-sheet-pane)
			 (event window-configuration-event))
  (let ((x (window-configuration-event-x event))
	(y (window-configuration-event-y event))
	(width (window-configuration-event-width event))
        (height (window-configuration-event-height event)))
    (let ((*configuration-event-p* sheet))
      (%set-sheet-region-and-transformation
       sheet
       (make-bounding-rectangle 0 0 width height)
       (make-translation-transformation x y)))))

(defmethod handle-event ((pane top-level-sheet-pane)
			 (event window-manager-delete-event))
  (frame-exit (pane-frame (event-sheet event))))

;;; UNMANAGED-TOP-LEVEL-SHEET PANE

(defclass unmanaged-top-level-sheet-pane (top-level-sheet-pane)
  ()
  (:documentation "Top-level sheet without window manager intervention"))

(defmethod sheet-native-transformation ((sheet top-level-sheet-pane))
  (with-slots (native-transformation) sheet
    	native-transformation))

(defmethod change-space-requirements ((pane unmanaged-top-level-sheet-pane)
                                      &rest space-req-keys
                                      &key resize-frame &allow-other-keys)
  ;; Special variant for unmanaged-top-level-sheet-pane. Since the
  ;; pane is unmanaged there is no window manager which can offer the
  ;; user options to resize this top level pane.
  ;;
  ;; This should however be changed by turning on the :resize-frame
  ;; option of the frame of the unmanaged-top-level-sheet-pane and
  ;; handle it in the method on top-level-sheet.
  ;;
  ;; This is currently not done, since:
  ;; . we obviously lack the :resize-frame option
  ;; . of some reason the frame of e.g. a command-menu is the
  ;;   application-frame. I am not sure if this is totally right.
  ;;
  ;; --GB 2003-03-16
  (declare (ignore space-req-keys resize-frame))
  (let ((w (space-requirement-width (compose-space pane)))
        (h (space-requirement-height (compose-space pane))))
    (resize-sheet pane w h)
    (allocate-space pane w h) ))

;;; Now each child (client) of a box-layout pane is described by the
;;; following class:

(defclass box-client ()
  ((fillp
    :initarg       :fillp
    :initform      nil
    :reader        box-client-fillp
    :documentation "Whether this child can stretch infinitly.")
   (fixed-size
    :initarg       :fixed-size
    :initform      nil
    :reader        box-client-fixed-size
    :documentation "Possible fixed size of a child.")
   (proportion
    :initarg       :proportion
    :initform      nil
    :reader        box-client-proportion
    :documentation "Proportion child should get of excess space.")
   (pane
    :initarg       :pane
    :reader        box-client-pane
    :documentation "Either the child pane or NIL.")))

(defclass box-layout-mixin ()
  ((box-layout-orientation
    :initarg :box-layout-orientation
    :initform :vertical
    :type     (member :vertical :horizontal)
    :accessor box-layout-orientation)
   (clients
    :accessor box-layout-mixin-clients
    :initform nil) )
  (:documentation
   "Mixin class for layout panes, which want to behave like a HBOX/VBOX."))

;;; First we need to make sure that the list of clients and the list
;;; of children agree with each other.

(defmethod sheet-adopt-child :after ((sheet box-layout-mixin) child)
  ;; When the child is already known in the client list we add no new
  ;; client object.
  (unless (find child (box-layout-mixin-clients sheet) :key #'box-client-pane)
    (setf (box-layout-mixin-clients sheet)
          (append (box-layout-mixin-clients sheet)
                  (list (make-instance 'box-client
                                       :pane child))))
    (when (and (sheet-enabled-p sheet)
               (sheet-parent sheet))
      (change-space-requirements sheet))))

(defmethod sheet-disown-child :after ((sheet box-layout-mixin) (child sheet) &key errorp)
  (declare (ignore errorp))
  (setf (box-layout-mixin-clients sheet)
        (remove-if (lambda (client)
                     (eq (box-client-pane client) child))
                   (box-layout-mixin-clients sheet)))
  (when (and (sheet-enabled-p sheet)
             (sheet-parent sheet))
    (change-space-requirements sheet)))


(defclass rack-layout-mixin (box-layout-mixin)
  ((box-layout-orientation
    :initarg :box-layout-orientation
    :initform :vertical
    :type (member :vertical :horizontal)
    :accessor box-layout-orientation))
  (:documentation
   "Mixin class for layout panes, which want to behave like a HRACK/VRACK."))

(defmethod compose-space ((pane box-layout-mixin) &key width height)
  (declare (ignore width height))
  (if (eq (box-layout-orientation pane) :vertical)
      (box-layout-mixin/vertically-compose-space pane)
      (box-layout-mixin/horizontally-compose-space pane)))

(defmethod allocate-space ((pane box-layout-mixin) width height)
  (if (eq (box-layout-orientation pane) :vertical)
      (box-layout-mixin/vertically-allocate-space pane width height)
      (box-layout-mixin/horizontally-allocate-space pane width height)))

(defvar *dump-allocate-space* nil)

(dada
 ((major   width        height)
  (minor   height       width)
  (xbox    hbox         vbox)
  (xrack   hrack        vrack)
  (xically horizontally vertically)
  (major-spacing x-spacing y-spacing)
  (minor-spacing x-spacing y-spacing)  )

 (defgeneric xically-content-sr** (pane client))

 (defmethod xically-content-sr** ((pane box-layout-mixin) client)
   (let (p)
     (let ((sr (if (box-client-pane client)
                   (compose-space (box-client-pane client))
                   (make-space-requirement :width 0 :min-width 0 :max-width 0
                                           :height 0 :min-height 0 :max-height 0))))
       (cond ((box-client-fillp client)
              (make-space-requirement
               :major     (space-requirement-major sr)
               :min-major (space-requirement-min-major sr)
               :max-major +fill+
               :minor     (space-requirement-minor sr)
               :min-minor (space-requirement-min-minor sr)
               :max-minor (space-requirement-max-minor sr)))
             ((setq p (box-client-fixed-size client))
              (make-space-requirement
               :major     p
               :min-major p
               :max-major p
               :minor     (if sr (space-requirement-minor sr) 0)
               :min-minor (if sr (space-requirement-min-minor sr) 0)
               :max-minor (if sr (space-requirement-max-minor sr) 0)))
             (t
              sr) ))))

 (defgeneric xically-content-sr*** (pane client major))

 (defmethod xically-content-sr*** ((pane box-layout-mixin) client major)
   (let (p)
     (let ((sr (if (box-client-pane client)
                   (compose-space (box-client-pane client))
                   (make-space-requirement :width 0 :min-width 0 :max-width 0
                                           :height 0 :min-height 0 :max-height 0))))
       (cond ((box-client-fillp client)
              (make-space-requirement
               :major     (space-requirement-major sr)
               :min-major (space-requirement-min-major sr)
               :max-major +fill+
               :minor     (space-requirement-minor sr)
               :min-minor (space-requirement-min-minor sr)
               :max-minor (space-requirement-max-minor sr)))
             ((setq p (box-client-fixed-size client))
              (make-space-requirement
               :major     p
               :min-major p
               :max-major p
               :minor     (if sr (space-requirement-minor sr) 0)
               :min-minor (if sr (space-requirement-min-minor sr) 0)
               :max-minor (if sr (space-requirement-max-minor sr) 0)))
             ((setq p (box-client-proportion client))
              (make-space-requirement
               :major     (clamp (* p major)
                                 (space-requirement-min-major sr)
                                 (space-requirement-max-major sr))
               :min-major (space-requirement-min-major sr)
               :max-major (space-requirement-max-major sr)
               :minor     (if sr (space-requirement-minor sr) 0)
               :min-minor (if sr (space-requirement-min-minor sr) 0)
               :max-minor (if sr (space-requirement-max-minor sr) 0)))
             (t
              sr) ))))

 (defgeneric box-layout-mixin/xically-compose-space (pane))

 (defmethod box-layout-mixin/xically-compose-space ((pane box-layout-mixin))
   (let ((n (length (sheet-enabled-children pane))))
     (with-slots (major-spacing) pane
       (loop
           for client in (box-layout-mixin-clients pane)
           for sr = (xically-content-sr** pane client)
           sum (space-requirement-major sr) into major
           sum (space-requirement-min-major sr) into min-major
           sum (space-requirement-max-major sr) into max-major
           maximize (space-requirement-minor sr) into minor
           maximize (space-requirement-min-minor sr) into min-minor
           minimize (space-requirement-max-minor sr) into max-minor
           finally
             (return
               (space-requirement+*
                 (make-space-requirement
                   :major     major
                   :min-major (min min-major major)
                   :max-major (max max-major major)
                   :minor     minor
                   :min-minor (min min-minor minor)
                   :max-minor (max max-minor minor))
                 :min-major (* (1- n) major-spacing)
                 :max-major (* (1- n) major-spacing)
                 :major     (* (1- n) major-spacing)
                 :min-minor 0
                 :max-minor 0
                 :minor     0))))))

  (defgeneric box-layout-mixin/xically-allocate-space-aux* (box width height))

  (defmethod box-layout-mixin/xically-allocate-space-aux* ((box box-layout-mixin) width height)
   (declare (ignorable width height))
   (let ((children (reverse (sheet-enabled-children box))))
     (with-slots (major-spacing) box
       (let* ((content-srs (mapcar #'(lambda (c) (xically-content-sr*** box c major))
                                   (box-layout-mixin-clients box)))
              (allot       (mapcar #'space-requirement-major content-srs))
              (wanted      (reduce #'+ allot))
              (excess      (- major wanted
                              (* (1- (length children)) major-spacing))))
         (when *dump-allocate-space*
           (format *trace-output* "~&;; ~S ~S~%"
                   'box-layout-mixin/xically-allocate-space-aux* box)
           (format *trace-output* "~&;;   major = ~D, wanted = ~D, excess = ~D, allot = ~D.~%"
                   major wanted excess allot))

         (let ((qvector
                (mapcar
                 (lambda (c)
                   (cond
                     ((box-client-fillp c)
                      (vector 1 0 0))
                     (t
                      (vector 0 0
                              (abs (- (if (> excess 0)
                                          (space-requirement-max-major (xically-content-sr*** box c major))
                                          (space-requirement-min-major (xically-content-sr*** box c major)))
                                      (space-requirement-major (xically-content-sr*** box c major))))))))
                 (box-layout-mixin-clients box))))
           ;;
           (when *dump-allocate-space*
             (format *trace-output* "~&;;   old allotment = ~S.~%" allot)
             (format *trace-output* "~&;;   qvector = ~S.~%" qvector)
             (format *trace-output* "~&;;   qvector 0 = ~S.~%" (mapcar #'(lambda (x) (elt x 0)) qvector))
             (format *trace-output* "~&;;   qvector 1 = ~S.~%" (mapcar #'(lambda (x) (elt x 1)) qvector))
             (format *trace-output* "~&;;   qvector 2 = ~S.~%" (mapcar #'(lambda (x) (elt x 2)) qvector)))
           ;;
           (dotimes (j 3)
             (let ((sum (reduce #'+ (mapcar (lambda (x) (elt x j)) qvector))))
               (unless (zerop sum)
                 (setf allot
                       (mapcar (lambda (allot q)
                                 (let ((q (elt q j)))
                                   (let ((delta (if (zerop sum) 0 (/ (* excess q) sum))))
                                     (decf excess delta)
                                     (decf sum q)
                                     (+ allot delta))))
                               allot qvector))
		 (when *dump-allocate-space*
		   (format *trace-output* "~&;;   new excess = ~F, allotment = ~S.~%" excess allot)) )))
           ;;
	   (when *dump-allocate-space*
	     (format *trace-output* "~&;;   excess = ~F.~%" excess)
	     (format *trace-output* "~&;;   new allotment = ~S.~%" allot))

           (values allot
                   (mapcar #'space-requirement-minor content-srs))) ))))

 (defmethod box-layout-mixin/xically-allocate-space-aux* :around ((box rack-layout-mixin) width height)
   (declare (ignorable width height))
   (multiple-value-bind (majors minors) (call-next-method)
     (values majors
             (mapcar (lambda (x) x minor) minors))))

 ;; Now actually layout the children
 ;;
 ;; A rack pane would force the minor dimension of the child. A
 ;; box pane would just align the child according to the
 ;; alignment option. We do the same with the minor dimension.
 ;;
 
 (defgeneric box-layout-mixin/xically-allocate-space (pane real-width real-height))

 (defmethod box-layout-mixin/xically-allocate-space ((pane box-layout-mixin) real-width real-height)
   (with-slots (major-spacing) pane
     (multiple-value-bind (majors minors)
         (box-layout-mixin/xically-allocate-space-aux* pane real-width real-height)
       (let ((x 0))
         (loop
             for child in (box-layout-mixin-clients pane)
             for major in majors
             for minor in minors
             do
             (when (box-client-pane child)
               (layout-child (box-client-pane child)
                             (pane-align-x (box-client-pane child))
                             (pane-align-y (box-client-pane child))
                             ((lambda (major minor) height width) x 0)
                             ((lambda (major minor) width height) x 0)
                             ((lambda (major minor) height width) width real-width)
                             ((lambda (major minor) height width) real-height height) ))
             (incf x major)
             (incf x major-spacing))))))

 (defmethod box-layout-mixin/xically-allocate-space ((pane rack-layout-mixin) real-width real-height)
   (with-slots (major-spacing) pane
     (multiple-value-bind (majors minors)
         (box-layout-mixin/xically-allocate-space-aux* pane real-width real-height)
       (let ((x 0))
         (loop
             for child in (box-layout-mixin-clients pane)
             for major in majors
             for minor in minors
             do
             (when (box-client-pane child)
               (layout-child (box-client-pane child)
                             :expand
                             :expand
                             ((lambda (major minor) height width) x 0)
                             ((lambda (major minor) width height) x 0)
                             ((lambda (major minor) height width) width real-width)
                             ((lambda (major minor) height width) real-height height) ))
             (incf x major)
             (incf x major-spacing)))))))

(defmethod reorder-sheets :after ((pane box-layout-mixin) new-order)
  ;; Bring the order of the clients in sync with the new order of the
  ;; children.
  (setf new-order (reverse new-order))
  (let ((new-bcs
         (loop for bc in (box-layout-mixin-clients pane)
               collect
               (cond ((box-client-pane bc)
                      (find (pop new-order) (box-layout-mixin-clients pane) :key #'box-client-pane))
                     (t
                      bc)))))
    (assert (null (set-difference new-bcs (box-layout-mixin-clients pane))))
    (setf (box-layout-mixin-clients pane) new-bcs))
  ;; finally do a re-layout.
  (change-space-requirements pane) )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-box-macro-contents (contents)
    (loop
       for content in contents
       collect (if (and (consp content)
			(or (realp (car content))
			    (member (car content) '(+fill+ :fill))))
		   `(list ',(car content) ,(cadr content))
		   content))))

(macrolet ((frob (macro-name box rack equalize-arg)
	     (let ((equalize-key (make-keyword equalize-arg)))
	       `(defmacro ,macro-name ((&rest options
					      &key (,equalize-arg t)
					      &allow-other-keys)
				       &body contents)
		  (with-keywords-removed (options (,equalize-key))
		    `(make-pane (if ,,equalize-arg
				    ',',rack
				    ',',box)
				,@options
				:contents (list ,@(make-box-macro-contents
						   contents))))))))
  (frob horizontally hbox-pane hrack-pane equalize-height)
  (frob vertically vbox-pane vrack-pane equalize-width))

(defclass box-pane (box-layout-mixin
		    composite-pane
		    permanent-medium-sheet-output-mixin ;arg!
		    )
  ()
  (:documentation "Superclass for hbox-pane and vbox-pane that provides the
		    initialization common to both."))

(defmethod initialize-instance :after ((pane box-pane) &key contents)
  (setf (%pane-contents pane) contents))

(defgeneric %pane-contests (pane contents))

(defmethod (setf %pane-contents) (contents (pane box-pane))
  (labels ((parse-box-content (content)
	     "Parses a box/rack content and returns a BOX-CLIENT instance."
	     ;; ### we need to parse more
	     (cond
	       ;; <pane>
	       ((panep content)
		(make-instance 'box-client :pane content))
	       ;; +fill+
	       ((or (eql content +fill+)
		    (eql content '+fill+)
		    (eql content :fill))
		(make-instance 'box-client
			       :pane nil
			       :fillp t))
	       ;; (+fill+ <pane>)
	       ((and (consp content)
		     (or (member (car content) '(+fill+ :fill))
			 (eql (car content) +fill+)))
		(make-instance 'box-client
			       :pane (cadr content)
			       :fillp t))
	       ;; <n>
	       ;;
	       ;; what about something like (30 :mm) ?
	       ;;
	       ((and (realp content) (>= content 0))
		(make-instance 'box-client
			       :pane nil
			       :fixed-size content))

	       ;; (<n> pane)
	       ((and (consp content)
		     (realp (car content))
		     (>= (car content) 0)
		     (consp (cdr content))
		     (panep (cadr content))
		     (null (cddr content)))
		(let ((number (car content))
		      (child  (cadr content)))
		  (if (< number 1)
		      (make-instance 'box-client
				     :pane child
				     :proportion number)
		      (make-instance 'box-client
				     :pane child
				     :fixed-size number))))

	       (t
		(error "~S is not a valid element in the ~S option of ~S."
		       content :contents pane)) )))
    ;; remove old children, if any
    (dolist (child (sheet-children pane))
      (sheet-disown-child pane child))

    (let* ((clients  (mapcar #'parse-box-content contents))
	   (children (remove nil (mapcar #'box-client-pane clients))))
      (setf (box-layout-mixin-clients pane) clients)
      (mapc (curry #'sheet-adopt-child pane) children))))

(defclass hbox-pane (box-pane)
   ()
  (:default-initargs :box-layout-orientation :horizontal))

(defclass vbox-pane (box-pane)
  ()
  (:default-initargs :box-layout-orientation :vertical))

(defclass hrack-pane (rack-layout-mixin hbox-pane)
   ()
   (:default-initargs :box-layout-orientation :horizontal))

(defclass vrack-pane (rack-layout-mixin vbox-pane)
   ()
   (:default-initargs :box-layout-orientation :vertical))

;;; TABLE PANE

;;; TODO: The table and grid panes should respect the :x-spacing,
;;; :y-spacing, and :spacing initargs.

(defclass table-pane (composite-pane)
  ((array
    :documentation "Two-dimensional array holding the child panes as they are to be arranged."))
  ;;
  (:documentation
   "The table layout implies that each colums has the same width
    and each lines has the same height - same rules for max and min -") )

(defmethod initialize-instance :after ((pane table-pane) &key contents &allow-other-keys)
  ;; check the format: contents should be list of lists of panes
  (unless (and (listp contents)
               (every (lambda (x)
                        (and (listp x)
                             (every #'panep x)))
                      contents))
    (error "~S option to ~S has bad format; should be a list of lists of panes.~%But its value is ~S."
           :contents pane contents))
  ;; shovel child panes into the array and adopt them
  (let ((nrows (length contents))
        (ncols (reduce #'max (mapcar #'length contents)
                       :initial-value 0)))
    (with-slots (array) pane
      (setf array (make-array (list nrows ncols)
                              :initial-element nil))
      (loop for row in contents
            for i from 0 do
         (loop for cell in row
               for j from 0 do
            (setf (aref array i j) cell)
            (sheet-adopt-child pane cell))))))

(dada ((xically horizontally vertically)
       (major   width height)
       (minor   height  width))
      ;;
      (defun stack-space-requirements-xically (srs)
        (loop
            for sr in srs
            sum (space-requirement-major sr) into major
            sum (space-requirement-min-major sr) into min-major
            sum (space-requirement-max-major sr) into max-major
            maximize (space-requirement-minor sr) into minor
            maximize (space-requirement-min-minor sr) into min-minor
            minimize (space-requirement-max-minor sr) into max-minor
            finally
              (return
                (make-space-requirement
                 :major major
                 :min-major (min min-major major)
                 :max-major (max max-major major)
                 :minor minor
                 :min-minor (min min-minor minor)
                 :max-minor (max max-minor minor)))))

      (defun allot-space-xically (srs major)
        (let* ((allot  (mapcar #'space-requirement-major srs))
               (wanted (reduce #'+ allot))
               (excess (- major wanted))
               (qs
                (mapcar (lambda (sr)
                          (abs (- (if (> excess 0)
                                      (space-requirement-max-major sr)
                                      (space-requirement-min-major sr))
                                  (space-requirement-major sr))))
                        srs)))
          (let ((sum (reduce #'+ qs)))
            (cond ((zerop sum)
                   (let ((n (length qs)))
                     (setf allot
                       (mapcar (lambda (allot q)
                                 (let ((delta (/ excess n)))
                                   (decf n)
                                   (decf excess delta)
                                   (decf sum q)
                                   (+ allot delta)))
                               allot qs))))
                  (t
                   (setf allot
                     (mapcar (lambda (allot q)
                               (let ((delta (if (zerop sum) 0 (/ (* excess q) sum))))
                                 (decf excess delta)
                                 (decf sum q)
                                 (+ allot delta)))
                             allot qs)))))
          allot)) )

(defgeneric table-pane-row-space-requirement (pane i))

(defmethod table-pane-row-space-requirement ((pane table-pane) i)
  (with-slots (array) pane
    (stack-space-requirements-horizontally
     (loop for j from 0 below (array-dimension array 1)
         collect (compose-space (aref array i j))))))

(defgeneric table-pane-col-space-requirement (pane j))

(defmethod table-pane-col-space-requirement ((pane table-pane) j)
  (with-slots (array) pane
    (stack-space-requirements-vertically
     (loop for i from 0 below (array-dimension array 0)
         collect (compose-space (aref array i j))))))

(defmethod compose-space ((pane table-pane) &key width height)
  (declare (ignore width height))
  (with-slots (array x-spacing y-spacing) pane
    ;; ---v our problem is here.
    ;; Which problem? --GB
    (let ((rsrs (loop for i from 0 below (array-dimension array 0) 
                    collect (table-pane-row-space-requirement pane i)))
          (csrs (loop for j from 0 below (array-dimension array 1) 
                    collect (table-pane-col-space-requirement pane j)))
          (xs (* x-spacing (1- (array-dimension array 1))))
          (ys (* y-spacing (1- (array-dimension array 0)))))
      (let ((r (stack-space-requirements-vertically rsrs))
            (c (stack-space-requirements-horizontally csrs)))
        (let ((res
               (make-space-requirement
                :width      (+ (space-requirement-width r) xs)
                :min-width  (+ (space-requirement-min-width r) xs)
                :max-width  (+ (space-requirement-max-width r) xs)
                :height     (+ (space-requirement-height c) ys)
                :min-height (+ (space-requirement-min-height c) ys)
                :max-height (+ (space-requirement-max-height c) ys))))
          res)))))

(defmethod allocate-space ((pane table-pane) width height)
  (let (rsrs csrs)
    (declare (ignorable rsrs csrs))
    (with-slots (array x-spacing y-spacing) pane
      ;; allot rows
      (let* ((xs (* x-spacing (1- (array-dimension array 1))))
             (ys (* y-spacing (1- (array-dimension array 0))))
             (rows (allot-space-vertically
                    (setq rsrs (loop for i from 0 below (array-dimension array 0)
                                     collect (table-pane-row-space-requirement pane i)))
                    (- height ys)))
             (cols (allot-space-horizontally
                    (setq csrs (loop for j from 0 below (array-dimension array 1)
                                     collect (table-pane-col-space-requirement pane j)))
                    (- width xs))))
        ;; now finally layout each child
        (loop
            for y = 0 then (+ y h y-spacing)
            for h in rows
            for i from 0
            do (loop
                   for x = 0 then (+ x w x-spacing)
                   for w in cols
                   for j from 0
                   do (let ((child (aref array i j)))
                        (layout-child child
                                      (pane-align-x child)
                                      (pane-align-y child)
                                      x y w h))))))))

(defun table-pane-p (pane)
  (typep pane 'table-pane))

(defmacro tabling ((&rest options &key (grid nil) &allow-other-keys) &body contents)
  (if grid
      `(make-pane 'grid-pane  ,@options :contents (list ,@contents))
      `(make-pane 'table-pane ,@options :contents (list ,@contents))))

;;; GRID PANE

(defclass grid-pane (table-pane)
  ()
  (:documentation
   "Be careful : each cells has the same size in the two dimentions.
 In other words : if the cell sizes are width, height then
  width  = grid-width / number of children per line
  height = grid-height / number of children per column.
=====> this is for all cells."))

(defun grid-p (pane)
  (typep pane 'grid-pane))

(defmethod compose-space ((grid grid-pane) &key width height)
  (declare (ignore width height))
  (mapc #'compose-space (sheet-children grid))
  (with-slots (array) grid
    (loop with nb-children-pl = (array-dimension array 1) ;(table-pane-number grid)
          with nb-children-pc = (array-dimension array 0) ;(/ (length (sheet-children grid)) nb-children-pl)
          for child in (sheet-children grid)
          and width = 0 then (max width (sr-width child))
          and height = 0 then (max height (sr-height child))
          and max-width = 5000000 then (min max-width (sr-min-width child))
          and max-height = 5000000 then (min max-height (sr-max-height child))
          and min-width = 0 then (max min-width (sr-min-width child))
          and min-height = 0 then (max min-height (sr-min-height child))
          finally (return
                    (make-space-requirement
                     :width (* width nb-children-pl)
                     :height (* height nb-children-pc)
                     :max-width (* width nb-children-pl)
                     :max-height (* max-height nb-children-pc)
                     :min-width (* min-width nb-children-pl)
                     :min-height (* min-height nb-children-pc))))))

(defmethod allocate-space ((grid grid-pane) width height)
  (with-slots (array) grid
    (loop with nb-kids-p-l = (array-dimension array 1) ;(table-pane-number grid)
          with nb-kids-p-c = (array-dimension array 0) ;(/ (length (sheet-children grid)) nb-kids-p-l)
          for c from nb-kids-p-c downto 1
          for row-index from 0 by 1
          for tmp-height = height then (decf tmp-height new-height)
          for new-height = (/ tmp-height c)
          for y = 0 then (+ y new-height)
          do (loop
                for col-index from 0 by 1
                for l from nb-kids-p-l downto 1                  
                for child = (aref array row-index col-index)
                for tmp-width = width then (decf tmp-width new-width)
                for new-width = (/ tmp-width l)
                for x = 0 then (+ x new-width)
                do (move-sheet child x y)
		  (allocate-space child new-width new-height)))))

;;; SPACING PANE

(defclass spacing-pane (;;standard-space-requirement-options-mixin
			single-child-composite-pane
                        permanent-medium-sheet-output-mixin)
  ((border-width :initarg :thickness
                 :initform 1))
  (:documentation "Never trust a random documentation string."))

(defmacro spacing ((&rest options) &body contents)
  `(make-pane 'spacing-pane ,@options :contents (list ,@contents)))

(defun spacing-p (pane)
  (typep pane 'spacing-pane))

(defmethod initialize-instance :after ((spacing spacing-pane) &key thickness contents &allow-other-keys)
  (declare (ignorable thickness contents))
  (with-slots (user-width user-min-width user-max-width
               user-height user-min-height user-max-height)
      spacing))

(defmethod compose-space ((pane spacing-pane) &key width height)
  (declare (ignore width height))
  (with-slots (border-width) pane
    (let ((sr (call-next-method)))
      (make-space-requirement
       :width (+ (* 2 border-width) (space-requirement-width sr))
       :height (+ (* 2 border-width) (space-requirement-height sr))
       :min-width (+ (* 2 border-width) (space-requirement-min-width sr))
       :min-height (+ (* 2 border-width) (space-requirement-min-height sr))
       :max-width (+ (* 2 border-width) (space-requirement-max-width sr))
       :max-height (+ (* 2 border-width) (space-requirement-max-height sr))))))

(defmethod allocate-space ((pane spacing-pane) width height)
  (with-slots (border-width) pane
    (let ((child (sheet-child pane))
	  (new-width  (- width border-width border-width))
	  (new-height (- height border-width border-width)))
      (layout-child child (pane-align-x pane) (pane-align-y pane)
                    border-width border-width
		    new-width new-height))))

;;; OUTLINED PANE

;; same as SPACING-PANE but a different default background.

(defclass outlined-pane (spacing-pane)
  ()
  (:default-initargs :background +black+))

(defmacro outlining ((&rest options) &body contents)
  `(make-pane 'outlined-pane ,@options :contents (list ,@contents)))

;;; BORDER PANE

;; same as outlined-pane, but thickness is now called border-width.

(defclass border-pane (outlined-pane)
  ((border-width :initarg :border-width
                 :initform 1
                 :reader border-pane-width))
  (:documentation ""))

(defmacro bordering ((&rest options) &body contents)
  `(make-pane 'border-pane ,@options :contents (list ,@contents)))

;;; This generic function appears to be nowhere used. 
(defgeneric pane-border (pane))

(defmethod pane-border ((pane basic-pane))
  (let ((parent (sheet-parent pane)))
    (when (and parent (typep parent 'border-pane))
	parent)))

;;; RAISED PANE

(defclass raised-pane (border-pane permanent-medium-sheet-output-mixin)
  ()
  (:default-initargs
   :border-width 2))

(defmacro raising ((&rest options) &body contents)
  `(make-pane 'raised-pane ,@options :contents (list ,@contents)))

(defmethod handle-repaint ((pane raised-pane) region)
  (declare (ignore region))
  (with-slots (border-width) pane
    (multiple-value-call #'draw-bordered-rectangle* pane (bounding-rectangle* (sheet-region pane))
                         :style :outset
                         :border-width border-width)))

;;; LOWERED PANE

(defclass lowered-pane (border-pane permanent-medium-sheet-output-mixin)
  ()
  (:default-initargs
   :border-width 2))

(defmacro lowering ((&rest options) &body contents)
  `(make-pane 'lowered-pane ,@options :contents (list ,@contents)))

(defmethod handle-repaint ((pane lowered-pane) region)
  (declare (ignore region))
  (with-slots (border-width) pane
    (multiple-value-call #'draw-bordered-rectangle* pane (bounding-rectangle* (sheet-region pane))
                         :style :inset
                         :border-width border-width)))

;;; RESTRAINING PANE

(defclass restraining-pane (single-child-composite-pane) ())

(defun restraining-pane-p (pane)
  (typep pane 'restraining-pane))

(defmacro restraining ((&rest options) &body contents)
  `(make-pane 'restraining-pane ,@options :contents (list ,@contents)))

(defmethod note-space-requirements-changed ((pane restraining-pane) child)
  (declare (ignore pane child)))

;;; BBOARD PANE

(defclass bboard-pane (composite-pane) ())

(defmethod compose-space ((bboard bboard-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :width 300 :height 300))

;;; VIEWPORT

(defclass viewport-pane (single-child-composite-pane) ())

(defmethod compose-space ((pane viewport-pane) &key width height)
  (declare (ignorable width height))
  ;; I _think_ this is right, it certainly shouldn't be the
  ;; requirements of the child, apart from the max sizes. If the child
  ;; does not want to go bigger than a specific size, we should not
  ;; force it to do so.
  (let ((child-sr (compose-space (sheet-child pane))))
    (if child-sr
        (make-space-requirement :max-width (space-requirement-max-width child-sr)
                                :max-height (space-requirement-max-height child-sr))
        (make-space-requirement))))

(defmethod allocate-space ((pane viewport-pane) width height)
  (with-slots (hscrollbar vscrollbar) (sheet-parent pane)
    (let* ((child            (sheet-child pane))
           (child-space      (compose-space child))
           (child-width      (space-requirement-width child-space))
           (child-min-width  (space-requirement-min-width child-space))
           (child-height     (space-requirement-height child-space))
           (child-min-height (space-requirement-min-height child-space)))
      (move-and-resize-sheet child
                             (if hscrollbar (- (gadget-value hscrollbar)) 0)
                             (if vscrollbar (- (gadget-value vscrollbar)) 0)
                             (max child-width  width)
                             (max child-height height))
        ; move-and-resize-sheet does not allocate space for the sheet...
        ; so we do it manually for this case, which may be wrong - CHECKME
        ; if this is the right place, reusing the above calculation might be a good idea
      (allocate-space child
                      (max child-min-width child-width  width)
                      (max child-min-height child-height height)))))

(defmethod note-input-focus-changed ((pane viewport-pane) state)
  (note-input-focus-changed (sheet-child pane) state))

;;; This method ensures that when the child changes size, the viewport
;;; will move its focus so that it will not display a region outside
;;; of `child' (if at all possible, this ideal can be circumvented by
;;; creating a child sheet that is smaller than the viewport). I do
;;; not believe having a viewport look at "empty" space is ever
;;; useful.
(defmethod note-space-requirements-changed ((pane viewport-pane) child)
  (let ((viewport-width (bounding-rectangle-width pane))
        (viewport-height (bounding-rectangle-height pane))
        (child-width (bounding-rectangle-width child))
        (child-height (bounding-rectangle-height child)))
    (destructuring-bind (horizontal-scroll vertical-scroll)
        (mapcar #'- (multiple-value-list
                     (transform-position (sheet-transformation child) 0 0)))
      ;; XXX: We cannot use `scroll-extent', because McCLIM ignores it
      ;; unless the scrollee happens to be drawing. Very weird, should
      ;; be fixed.

      ;; It's not a bug, it's a feature. This requires further thought. -Hefner
      (move-sheet child
		  (- (if (> (+ horizontal-scroll viewport-width)
			    child-width)
			 (- child-width viewport-width)
			 horizontal-scroll))
                  (- (if (> (+ vertical-scroll viewport-height)
			    child-height)
			 (- child-height viewport-height)
			 vertical-scroll)))
      (scroller-pane/update-scroll-bars (sheet-parent pane)))))

;;; Accounting for changed space requirements

(defmethod change-space-requirements ((pane viewport-pane) &rest options)
  (declare (ignore options))
  (let* ((client (sheet-child pane))
	 (sr (compose-space client))
	 (width  (max (bounding-rectangle-width pane)
		      (space-requirement-width sr)))
	 (height (max (bounding-rectangle-height pane)
		      (space-requirement-height sr))))
    (resize-sheet client width height)
    (allocate-space client width height)
    (scroller-pane/update-scroll-bars (sheet-parent pane))))


;;; SCROLLER PANE

;;; How scrolling is done

;;; The scroll-pane has a child window called the 'viewport', which
;;; itself has the scrolled client pane as child. To scroll the client
;;; pane is to move it [to possibly negative coordinates].
;;;
;;; So the viewport is just a kind of hole, where some part of the
;;; scrolled window shows through.

;;; How the scroll bars are set up

;;; The scroll-bar's min/max values match the min/max arguments to
;;; scroll-extent. The thumb-size is then calculated accordingly.

(defparameter *scrollbar-thickness* 17)

(defvar clim-extensions:*default-vertical-scroll-bar-position*
  :right
  "Default for the :VERTICAL-SCROLL-BAR-POSITION init arg of a
SCROLLER-PANE. Set it to :LEFT to have the vertical scroll bar of a
SCROLLER-PANE appear on the ergonomic left hand side, or leave set to
:RIGHT to have it on the distant right hand side of the scroller.")

(defclass scroller-pane (composite-pane)
  ((scroll-bar :type scroll-bar-spec ; (member t :vertical :horizontal nil)
               ;; ### Note: I added NIL here, so that the application
               ;; programmer can switch off scroll bars alltogether. 
               ;; The spec though has it neither in the description of
               ;; SCROLLER-PANE, nor in the description of
               ;; MAKE-CLIM-STREAM-PANE, but in OPEN-WINDOW-STREAM.
               ;;
               ;; One might argue that in case of no scroll-bars the
               ;; application programmer can just skip the scroller
               ;; pane altogether. But I think that the then needed
               ;; special casing on having a scroller pane or a bare
               ;; viewport at hand is an extra burden, that can be
               ;; avoided.
               ;; --GB 2005-11-29
               :initform t
               :initarg :scroll-bar
               :accessor scroller-pane-scroll-bar)
   (viewport   :initform nil)
   (vscrollbar :initform nil)
   (hscrollbar :initform nil)
   (suggested-width  :initform 300 :initarg :suggested-width)
   (suggested-height :initform 300 :initarg :suggested-height)
   (vertical-scroll-bar-position
    :initform clim-extensions:*default-vertical-scroll-bar-position*
    :initarg :vertical-scroll-bar-position
    :type (member :left :right)
    :documentation "Whether to put the vertical scroll bar on the left hand or
                    right hand side of the scroller pane."))
  (:default-initargs
   :x-spacing 0
   :y-spacing 0))

(defgeneric scroll-bar-values (scroll-bar)
  (:documentation "Returns the min value, max value, thumb size, and value of a
  scroll bar. When Setf-ed, updates the scroll bar graphics"))

(defgeneric* (setf scroll-bar-values) (min-value max-value thumb-size value scroll-bar))

(defmacro scrolling ((&rest options) &body contents)
  `(let ((viewport (make-pane 'viewport-pane :contents (list ,@contents))))
     (make-pane 'scroller-pane ,@options :contents (list viewport))))

;;; Layout

(defmethod compose-space ((pane scroller-pane) &key width height)
  (declare (ignore width height))
  (with-slots (viewport vscrollbar hscrollbar suggested-width suggested-height
                        x-spacing y-spacing scroll-bar)
      pane
    (if viewport
        (let ((req
               ;; v-- where does this requirement come from?
               ;;     a: just an arbitrary default
               (make-space-requirement
                :width suggested-width :height suggested-height :max-width +fill+ :max-height +fill+
                :min-width  (max (* 2 x-spacing) (if (null scroll-bar) 0 30))
                :min-height (max (* 2 y-spacing) (if (null scroll-bar) 0 30))))
              (viewport-child (sheet-child viewport)))
          (when vscrollbar
            (setq req (space-requirement+*
                       (space-requirement-combine #'max
                                                  req
                                                  (compose-space vscrollbar))
                       :height     *scrollbar-thickness*
                       :min-height *scrollbar-thickness*
                       :max-height *scrollbar-thickness*)))
          (when hscrollbar
            (setq req (space-requirement+*
                       (space-requirement-combine
                        #'max req (compose-space hscrollbar))
                       :width     *scrollbar-thickness*
                       :min-width *scrollbar-thickness*
                       :max-width *scrollbar-thickness*)))
          (let* ((viewport-sr (compose-space viewport
                               :width suggested-width
                               :height suggested-height))
                 (max-width (+ (space-requirement-max-width viewport-sr)
                               (if vscrollbar *scrollbar-thickness* 0)
                               ;; I don't know why this is necessary.
                               (if (extended-output-stream-p viewport-child)
                                   (* 4 (stream-vertical-spacing viewport-child))
                                   0)))
                 (max-height (+ (space-requirement-max-height viewport-sr)
                                (if hscrollbar *scrollbar-thickness* 0)
                                ;; I don't know why this is necessary.
                                (if (extended-output-stream-p viewport-child)
                                    (* 4 (stream-vertical-spacing viewport-child))
                                    0))))
            (setq req (make-space-requirement
                       :width (min (space-requirement-width req)
                                   max-width)
                       :height (min (space-requirement-height req)
                                    max-height)
                       :min-width (min (space-requirement-min-width req)
                                       max-width)
                       :min-height (min (space-requirement-min-height req)
                                        max-height)
                       :max-width max-width
                       :max-height max-height)))
          
          req)
        (make-space-requirement))))

(defmethod allocate-space ((pane scroller-pane) width height)
  (with-slots (viewport vscrollbar hscrollbar x-spacing y-spacing vertical-scroll-bar-position) pane
    (let* ((vsbar-width (if vscrollbar (space-requirement-width (compose-space vscrollbar)) 0))
           (hsbar-height (if hscrollbar (space-requirement-height (compose-space hscrollbar)) 0))
           (viewport-width  (- width vsbar-width))
           (viewport-height (- height hsbar-height)))
      (when vscrollbar
        (move-sheet vscrollbar
                    (ecase vertical-scroll-bar-position
                      (:left 0)
                      (:right (- width vsbar-width)))
                    0)
        (allocate-space vscrollbar
                        vsbar-width
                        (- height hsbar-height)))
      (when hscrollbar
        (move-sheet hscrollbar
                    (ecase vertical-scroll-bar-position
                      (:left vsbar-width)
                      (:right 0))
                    (- height hsbar-height))
        (allocate-space hscrollbar
                        (- width vsbar-width)
                        hsbar-height))
      ;;
      ;; Recalculate the gadget-values of the scrollbars
      ;;
      (when vscrollbar
        (let* ((scrollee (sheet-child viewport))
               (min 0)
               (max (- (max (space-requirement-height (compose-space scrollee))
                            viewport-height)
                       viewport-height))
               (ts  viewport-height)
               (val (if (zerop (gadget-max-value vscrollbar))
                        0
                        (* (/ (gadget-value vscrollbar) (gadget-max-value vscrollbar))
                           max))))
	  (setf (scroll-bar-values vscrollbar) (values min max ts val))))
      (when hscrollbar
        (let* ((scrollee (sheet-child viewport))
               (min 0)
               (max (- (max (space-requirement-width (compose-space scrollee))
                            viewport-width)
                       viewport-width))
               (ts  viewport-width)
               (val (if (zerop (gadget-max-value hscrollbar))
                        0
                        (* (/ (gadget-value hscrollbar) (gadget-max-value hscrollbar))
                           max))))
	  (setf (scroll-bar-values hscrollbar) (values min max ts val))))
      (when viewport
        (move-sheet viewport
                    (+ x-spacing
                       (ecase vertical-scroll-bar-position
                         (:left vsbar-width)
                         (:right 0)))
                    (+ y-spacing 0))
        (allocate-space viewport
                        (- viewport-width (* 2 x-spacing))
                        (- viewport-height (* 2 y-spacing))))
      (scroller-pane/update-scroll-bars pane))))

;;; Initialization

(defun scroller-pane/vertical-drag-callback (pane new-value)
  "Callback for the vertical scroll-bar of a scroller-pane."
  (with-slots (viewport hscrollbar vscrollbar) pane
    (let ((scrollee (sheet-child viewport)))
      (when (pane-viewport scrollee)
	(move-sheet scrollee
		    (if hscrollbar
			(- (gadget-value hscrollbar))
			0)
		    (- new-value))))))

(defun scroller-pane/horizontal-drag-callback (pane new-value)
  "Callback for the horizontal scroll-bar of a scroller-pane."
  (with-slots (viewport hscrollbar vscrollbar) pane
    (let ((scrollee (sheet-child viewport)))
      (when (pane-viewport scrollee)
	(move-sheet scrollee
		    (- new-value)
		    (if vscrollbar
			(- (gadget-value vscrollbar))
			0))))))
    
(defun scroller-pane/update-scroll-bars (pane)
  (check-type pane scroller-pane)
  (with-slots (viewport hscrollbar vscrollbar) pane
    (let* ((scrollee (sheet-child viewport))
           (scrollee-sr (sheet-region scrollee))
           (viewport-sr (sheet-region viewport)))
      ;;
      (when hscrollbar
        (let* ((min-value (bounding-rectangle-min-x scrollee-sr))
               (max-value (max (- (bounding-rectangle-max-x scrollee-sr)
                                  (bounding-rectangle-width viewport-sr))
                               (bounding-rectangle-min-x scrollee-sr)))
               (thumb-size (bounding-rectangle-width viewport-sr))
               (value (min (- (nth-value 0 (transform-position
                                            (sheet-transformation scrollee) 0 0)))
                           max-value)))
          (setf (scroll-bar-values hscrollbar)
                (values min-value max-value thumb-size value))))
      ;;
      (when vscrollbar
        (let* ((min-value (bounding-rectangle-min-y scrollee-sr))
               (max-value (max (- (bounding-rectangle-max-y scrollee-sr)
                                  (bounding-rectangle-height viewport-sr))
                               (bounding-rectangle-min-y scrollee-sr)))
               (thumb-size (bounding-rectangle-height viewport-sr))
               (value (min (- (nth-value 1 (transform-position
                                            (sheet-transformation scrollee) 0 0)))
                           max-value)))
          (setf (scroll-bar-values vscrollbar)
                (values min-value max-value thumb-size value)))))))

(defmethod initialize-instance :after ((pane scroller-pane) &key contents &allow-other-keys)
  (sheet-adopt-child pane (first contents))
  (with-slots (scroll-bar viewport vscrollbar hscrollbar) pane
    (setq viewport (first (sheet-children pane)))
    ;; make the background of the viewport match the background of the
    ;; things scrolled.
    ;; This doesn't appear to work, hence the "gray space" bugs. Actually
    ;; handy for observing when the space requirements get messed up.. -Hefner
    (alexandria:when-let ((child (sheet-child viewport)))
      (setf (slot-value pane 'background)  ;### hmm ...
            (pane-background child)))
    ;; make sure that we have ok options for the scroll-bar argument...
    (check-type scroll-bar scroll-bar-spec) ; (member :vertical :horizontal :both t nil))
    (when (member scroll-bar '(:vertical :both t))
      (setq vscrollbar
            (make-pane 'scroll-bar
                       :orientation :vertical
                       :client (sheet-child viewport)
                       :drag-callback (lambda (gadget new-value)
                                        (declare (ignore gadget))
                                        (scroller-pane/vertical-drag-callback pane new-value))
                       :scroll-up-page-callback
                       #'(lambda (scroll-bar)
                           (scroll-page-callback scroll-bar 1))
                       :scroll-down-page-callback
                       #'(lambda (scroll-bar)
                           (scroll-page-callback scroll-bar -1))
                       :scroll-up-line-callback
                       #'(lambda (scroll-bar)
                           (scroll-line-callback scroll-bar 1))
                       :scroll-down-line-callback
                       #'(lambda (scroll-bar)
                           (scroll-line-callback scroll-bar -1))
                       :value-changed-callback (lambda (gadget new-value)
                                                 (declare (ignore gadget))
                                                 (scroller-pane/vertical-drag-callback pane new-value))
                       :min-value 0
                       :max-value 1))
      (sheet-adopt-child pane vscrollbar))
    (when (member scroll-bar '(:horizontal :both t))
      (setq hscrollbar
            (make-pane 'scroll-bar
                       :orientation :horizontal
                       :client (sheet-child viewport)
                       :drag-callback (lambda (gadget new-value)
                                        (declare (ignore gadget))
                                        (scroller-pane/horizontal-drag-callback pane new-value))
                       :scroll-up-page-callback
                       #'(lambda (scroll-bar)
                           (scroll-page-callback scroll-bar 1))
                       :scroll-down-page-callback
                       #'(lambda (scroll-bar)
                           (scroll-page-callback scroll-bar -1))
                       :scroll-up-line-callback
                       #'(lambda (scroll-bar)
                           (scroll-line-callback scroll-bar 1))
                       :scroll-down-line-callback
                       #'(lambda (scroll-bar)
                           (scroll-line-callback scroll-bar -1))
                       :value-changed-callback
                       #'(lambda (gadget new-value)
                           (declare (ignore gadget))
                           (scroller-pane/horizontal-drag-callback pane new-value))
                       :min-value 0
                       :max-value 1))
      (sheet-adopt-child pane hscrollbar))))

;;; Scrolling itself

(defun scroll-page-callback (scroll-bar direction)
  (let ((client (gadget-client scroll-bar)))
    (setf (gadget-value scroll-bar :invoke-callback t)
          (clamp
           (- (gadget-value scroll-bar)
               (* direction
                  (funcall (if (eq (gadget-orientation scroll-bar) :vertical)
                               #'bounding-rectangle-height
                               #'bounding-rectangle-width)
                           (pane-viewport-region client))))
            (gadget-min-value scroll-bar)
            (gadget-max-value scroll-bar)))))

(defun scroll-line-callback (scroll-bar direction)
  (let ((client (gadget-client scroll-bar)))
    (setf (gadget-value scroll-bar :invoke-callback t)
          (clamp
           (- (gadget-value scroll-bar)
              (* direction
                 (if (extended-output-stream-p client)
                     (stream-line-height client)
                     10)))              ; picked an arbitrary number - BTS
           (gadget-min-value scroll-bar)
           (gadget-max-value scroll-bar)))))

(defmethod pane-viewport ((pane basic-pane))
  (let ((parent (sheet-parent pane)))
    (when (and parent (typep parent 'viewport-pane))
	parent)))

;;; Default for streams that aren't even panes.

(defmethod pane-viewport-region ((pane t))
  nil)

(defmethod pane-viewport-region ((pane basic-pane))
  (let ((viewport (pane-viewport pane)))
    (and viewport
         (untransform-region
          (sheet-delta-transformation pane viewport)
          (sheet-region viewport)))))

(defmethod pane-scroller ((pane basic-pane))
  (let ((viewport (pane-viewport pane)))
    (when viewport
      (sheet-parent viewport))))

(defmethod scroll-extent ((pane basic-pane) x y)
  (when (pane-viewport pane)
    (move-sheet pane (- x) (- y))
    (when (pane-scroller pane)
      (scroller-pane/update-scroll-bars (pane-scroller pane)))))


;;; LABEL PANE

(defclass label-pane (composite-pane  permanent-medium-sheet-output-mixin)
  ((label :type string
          :initarg :label
          :accessor label-pane-label
          :initform "")
   (alignment :type (member :bottom :top)
              :initform :top
              :initarg :label-alignment
              :reader label-pane-label-alignment)
   (background :initform *3d-normal-color*))
  (:default-initargs
   :align-y    :center
   :text-style (make-text-style :sans-serif nil nil))
  (:documentation ""))

(defmacro labelling ((&rest options) &body contents)
  `(make-pane 'label-pane ,@options :contents (list ,@contents)))

(defgeneric label-pane-margins (pane))

(defmethod label-pane-margins ((pane label-pane))
  (let ((m0 2)
        (a (text-style-ascent (pane-text-style pane) pane))
        (d (text-style-descent (pane-text-style pane) pane)))
    (values
     ;; margins of inner sheet region
     (+ a (* 2 m0))
     (+ a (if (eq (label-pane-label-alignment pane) :top) d 0) (* 2 m0))
     (+ a (* 2 m0))
     (+ a (if (eq (label-pane-label-alignment pane) :top) 0 d) (* 2 m0))
     ;; margins of surrounding border
     (+ m0 (/ a 2))
     (+ m0 (/ a 2))
     (+ m0 (/ a 2))
     (+ m0 (if (eq (label-pane-label-alignment pane) :top) 0 d) (/ a 2))
     ;; position of text
     (+ m0 (if (sheet-children pane)
               (+ a m0 m0 d)
             0))
     (+ m0 a))))

(defmethod compose-space ((pane label-pane) &key width height)
  (declare (ignore width height))
  (let* ((w (text-size pane (label-pane-label pane)))
         (a (text-style-ascent (pane-text-style pane) pane))
         (d (text-style-descent (pane-text-style pane) pane))
         (m0 2)
         (h (+ a d m0 m0)))
    (cond ((and (sheet-children pane)
                ;; ### this other test below seems to be neccessary since
                ;;     somebody decided that (NIL) is a valid return value
                ;;     from sheet-children. --GB 2002-11-10
                (first (sheet-children pane)))
           (let ((sr2 (compose-space (first (sheet-children pane)))))
             (multiple-value-bind (right top left bottom) (label-pane-margins pane)
               (make-space-requirement
                ;; label!
                :width      (+ left right (max (+ w m0 m0) (space-requirement-width sr2)))
                :min-width  (+ left right (max (+ w m0 m0) (space-requirement-min-width sr2)))
                :max-width  (+ left right (max (+ w m0 m0) (space-requirement-max-width sr2)))
                :height     (+ top bottom (space-requirement-height sr2))
                :min-height (+ top bottom (space-requirement-min-height sr2))
                :max-height (+ top bottom (space-requirement-max-height sr2))))))
          (t
           (incf w m0)
           (incf w m0)
           (let ((sr1 (make-space-requirement :width w :min-width w
                                              :height h :min-height h :max-height h)))
             (when (sheet-children pane)
               (let ((sr2 (compose-space (first (sheet-children pane)))))
                 (setf sr1
                   (make-space-requirement
                    :width      (max (space-requirement-width sr1) (space-requirement-width sr2))
                    :min-width  (max (space-requirement-min-width sr1) (space-requirement-min-width sr2))
                    :max-width  (max (space-requirement-max-width sr1) (space-requirement-max-width sr2))
                    :height     (+ (space-requirement-height sr1) (space-requirement-height sr2))
                    :min-height (+ (space-requirement-min-height sr1) (space-requirement-min-height sr2))
                    :max-height (+ (space-requirement-max-height sr1) (space-requirement-max-height sr2))))))
             sr1)))))

(defmethod allocate-space ((pane label-pane) width height)
  (multiple-value-bind (right top left bottom) (label-pane-margins pane)
    (alexandria:when-let ((child (first (sheet-children pane))))
      (multiple-value-bind (x1 y1 x2 y2) (values 0 0 width height)
        (move-sheet child (+ x1 left) (+ y1 top))
        (allocate-space child
                        (- (- x2 right) (+ x1 left))
                        (- (- y2 bottom) (+ y1 top)))))))

(defmethod handle-repaint ((pane label-pane) region)
  (declare (ignore region))
  (let ((m0 2)
        (a (text-style-ascent (pane-text-style pane) pane))
        (d (text-style-descent (pane-text-style pane) pane))
        (tw (text-size pane (label-pane-label pane))))
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
      (multiple-value-bind (iright itop ileft ibottom
                                   bright btop bleft bbottom)
          (label-pane-margins pane)
        (declare (ignorable iright itop ileft ibottom))
        (multiple-value-bind (tx ty)
            (values (ecase (pane-align-x pane)
                      (:left (+ x1 m0 (if (sheet-children pane)
                                          (+ a m0 m0 d)
                                          0)))
                      (:right (- x2 m0 (if (sheet-children pane)
                                           (+ a m0 m0 d)
                                           0)
                               tw))
                      (:center (- (/ (- x2 x1) 2) (/ tw 2))))
                    (ecase (label-pane-label-alignment pane)
                      (:top (+ y1 m0 a))
                      (:bottom (- y2 m0 d))))
          (draw-text* pane (label-pane-label pane)
                      tx ty)
          ;;;
          (when (sheet-children pane)
            (with-drawing-options (pane
                                   :clipping-region
                                   (region-difference
                                    (sheet-region pane)
                                    (make-rectangle* (- tx m0) (- ty a) (+ tx tw m0) (+ ty d))))
              (draw-bordered-rectangle* pane (+ x1 bleft) (+ y1 btop) (- x2 bright) (- y2 bbottom) 
                                        :style :groove))))))))


(defmethod initialize-instance :after ((pane label-pane) &key contents &allow-other-keys)
  (when contents
    (sheet-adopt-child pane (first contents))))

;;; GENERIC FUNCTIONS

(defgeneric* (setf window-viewport-position) (x y clim-stream-pane))

;;; Mixin for panes which want the mouse wheel to scroll vertically

(defclass mouse-wheel-scroll-mixin () ())

(defparameter *mouse-scroll-distance* 4
  "Number of lines by which to scroll the window in response to the scroll wheel")

(defmethod scroll-quantum (pane) 10)	; TODO: Connect this with the scroller-pane motion

(defun find-viewport-for-scroll (pane)
  "Find a viewport in the chain of parents which contains 'pane',
   returning this viewport and the sheet immediately contained within."
  (cond ((not (typep pane 'basic-pane))
         (values nil nil))
	((pane-viewport pane) (values (pane-viewport pane) pane))
	(t (find-viewport-for-scroll (sheet-parent pane)))))

(defun scroll-sheet (sheet horizontal vertical)
  (with-bounding-rectangle* (vx0 vy0 vx1 vy1) (pane-viewport-region sheet)
    (with-bounding-rectangle* (sx0 sy0 sx1 sy1) (sheet-region sheet)
      (let ((viewport-width  (- vx1 vx0))
            (viewport-height (- vy1 vy0))
            (delta (* *mouse-scroll-distance*
                      (scroll-quantum sheet))))
        ;; The coordinates (x,y) of the new upper-left corner of the viewport
        ;; must be "sx0 < x < sx1 - viewport-width"  and
        ;;         "sy0 < y < sy1 - viewport-height"
        (scroll-extent sheet
                       (max sx0 (min (- sx1 viewport-width)  (+ vx0 (* delta horizontal))))
                       (max sy0 (min (- sy1 viewport-height) (+ vy0 (* delta vertical)))))))))

(defmethod handle-event ((sheet mouse-wheel-scroll-mixin)
                         (event pointer-scroll-event))
  (multiple-value-bind (viewport sheet) (find-viewport-for-scroll sheet)
    (when viewport
      (scroll-sheet sheet
                    (pointer-event-delta-x event)
                    (pointer-event-delta-y event)))))


;;;
;;; 29.4 CLIM Stream Panes
;;;

;;; A class that implements the display function invocation. It's put
;;; in a super class of clim-stream-pane so that redisplay-frame-pane
;;; on updating-output-stream-mixin can override that method.

(defclass pane-display-mixin ()
  ((display-function :initform 'clim-stream-pane-default-display-function
		     :initarg :display-function
		     :accessor pane-display-function)))

(defmethod redisplay-frame-pane ((frame application-frame)
				 (pane pane-display-mixin)
				 &key force-p)
  (declare (ignore force-p))
  (invoke-display-function frame pane))

(defclass clim-stream-pane (updating-output-stream-mixin
			    pane-display-mixin
			    permanent-medium-sheet-output-mixin
                            #-clim-mp standard-repainting-mixin
                            standard-extended-input-stream
                            standard-extended-output-stream
                            standard-output-recording-stream
                            ;; sheet-leaf-mixin
                            sheet-multiple-child-mixin   ; needed for GADGET-OUTPUT-RECORD
                            basic-pane)
  ((redisplay-needed :initarg :display-time) 
   (scroll-bars :type scroll-bar-spec ; (member t :vertical :horizontal nil)
		:initform nil
		:initarg :scroll-bars
		:accessor pane-scroll-bars)
  
   ; Should inherit from label-pane for this one ??
   (label :type string
          :initform ""
	  :initarg :label
	  :reader pane-label)
   (text-margin :initarg :text-margin
		:reader pane-text-margin)
   (vertical-spacing :initarg :vertical-spacing
		     :reader pane-vertical-spacing)
   (end-of-line-action :initform :wrap
		       :initarg :end-of-line-action
		       :reader pane-end-of-line-action)
   (end-of-page-action :initform :scroll
		       :initarg :end-of-page-action
		       :reader pane-end-of-page-action)
   ;; Slots of space-requirement-options-mixin defined with private accessors for our
   ;; convenience; They are used by the :compute protocol.
   (user-width :accessor %pane-user-width)
   (user-min-width :accessor %pane-user-min-width)
   (user-max-width :accessor %pane-user-max-width)
   (user-height :accessor %pane-user-height)
   (user-min-height :accessor %pane-user-min-height)
   (user-max-height :accessor %pane-user-max-height)
   ;; size required by the stream
   (stream-width :initform 0 :accessor stream-width)
   (stream-height :initform 0 :accessor stream-height))
  (:documentation
   "This class implements a pane that supports the CLIM graphics,
    extended input and output, and output recording protocols."))

(defmethod interactive-stream-p ((stream clim-stream-pane))
  t)

(defmethod redisplay-frame-pane :after ((frame application-frame)
				 (pane clim-stream-pane)
				 &key force-p)
  (declare (ignore frame force-p))
  (unless (or (eql :compute (pane-user-width pane))
              (eql :compute (pane-user-min-width pane))
              (eql :compute (pane-user-max-width pane))
              (eql :compute (pane-user-height pane))
              (eql :compute (pane-user-min-height pane))
              (eql :compute (pane-user-max-height pane)))
    (change-space-requirements pane)))

(defun invoke-display-function (frame pane)
  (let ((display-function (pane-display-function pane)))
    (cond ((consp display-function)
	   (apply (car display-function)
		  frame pane (cdr display-function)))
	  (display-function
	   (funcall display-function frame pane))
	  (t nil))))

(defun change-stream-space-requirements (pane &key width height)
  (check-type pane clim-stream-pane)
  (when width
    (setf (stream-width pane) width))
  (when height
    (setf (stream-height pane) height))
  (change-space-requirements pane))

(defmethod compose-space :around ((pane clim-stream-pane) &key width height)
  (declare (ignore width height))
  (flet ((compute (val default)
	   (if (eq val :compute) default val)))
    (if (or (eql :compute (pane-user-width pane))
            (eql :compute (pane-user-min-width pane))
            (eql :compute (pane-user-max-width pane))
            (eql :compute (pane-user-height pane))
            (eql :compute (pane-user-min-height pane))
            (eql :compute (pane-user-max-height pane)))
	(multiple-value-bind (width height)
            (let ((record
                   (if (slot-value pane 'incremental-redisplay)
                       (stream-output-history pane)
                       (with-output-to-output-record (pane)
                         (invoke-display-function *application-frame* pane)))))
              (with-bounding-rectangle* (x1 y1 x2 y2) record
                (values (- x2 (min 0 x1)) (- y2 (min y1)))))
          (unless (> width 0) (setf width 1))
          (unless (> height 0) (setf height 1))
          (setf (stream-width pane) width)
          (setf (stream-height pane) height)
          ;; overwrite the user preferences which value is :compute
          (letf (((%pane-user-width pane)
                  (compute (pane-user-width pane) width))
                 ((%pane-user-min-width pane)
                  (compute (pane-user-min-width pane) width))
                 ((%pane-user-max-width pane)
                  (compute (pane-user-max-width pane) width))
                 ((%pane-user-height pane)
                  (compute (pane-user-height pane) height))
                 ((%pane-user-min-height pane)
                  (compute (pane-user-min-height pane) height))
                 ((%pane-user-max-height pane)
                  (compute (pane-user-max-height pane) height)))
            (call-next-method)))
        (call-next-method))))

;;; XXX if we decide to handle sheets starting from position different than
;;; [0,0] in the future we should take here bounding-rectangle-width/height and
;;; set sheet region to bounding-rectangle-min-x/y. Such approach may require
;;; change in more places.
(defmethod compose-space ((pane clim-stream-pane) &key width height)
  (declare (ignorable width height))
  (let* ((w (bounding-rectangle-max-x (stream-output-history pane)))
         (h (bounding-rectangle-max-y (stream-output-history pane)))
         (width (max w (stream-width pane)))
         (height (max h (stream-height pane))))
    (make-space-requirement
     :min-width (clamp w 0 width)
     :width width
     :max-width +fill+
     :min-height (clamp h 0 height)
     :height height
     :max-height +fill+)))

(defmethod window-clear ((pane clim-stream-pane))
  (stream-close-text-output-record pane)
  (let ((output-history (stream-output-history pane)))
    (with-bounding-rectangle* (left top right bottom) output-history
      (when (sheet-viewable-p pane)
	(medium-clear-area (sheet-medium pane) left top right bottom)))
    (clear-output-record output-history))
  (window-erase-viewport pane)
  (let ((cursor (stream-text-cursor pane)))
    (when cursor
      (setf (cursor-position cursor) (values 0 0))))
  (setf (stream-width pane) 0)
  (setf (stream-height pane) 0)
  (scroll-extent pane 0 0)
  (change-space-requirements pane))

(defmethod window-refresh ((pane clim-stream-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)    
    (draw-rectangle* (sheet-medium pane) x1 y1 x2 y2 :ink +background-ink+))
  (stream-replay pane))

(defun clim-stream-pane-default-display-function (frame pane)
  (declare (ignore frame))
  (stream-replay pane))

(defmethod window-viewport ((pane clim-stream-pane))
  (or (pane-viewport-region pane)
      (sheet-region pane)))

(defmethod window-erase-viewport ((pane clim-stream-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (or (pane-viewport-region pane)
                                              (sheet-region pane))
    (draw-rectangle* (sheet-medium pane) x1 y1 x2 y2 :ink +background-ink+)))

(defmethod window-viewport-position ((pane clim-stream-pane))
  (multiple-value-bind (x y) (bounding-rectangle* (stream-output-history pane))
    (values x y)))

(defmethod* (setf window-viewport-position) (x y (pane clim-stream-pane))
  (scroll-extent pane x y)
  (values x y))

(defmethod stream-set-input-focus ((stream clim-stream-pane))
  (with-slots (port) stream
    (prog1 (port-keyboard-input-focus port)
      (setf (port-keyboard-input-focus port) stream))))

;;; output any buffered stuff before input

(defmethod stream-read-gesture :before ((stream clim-stream-pane)
					&key timeout peek-p
					input-wait-test
					input-wait-handler
					pointer-button-press-handler)
  (declare (ignore timeout peek-p input-wait-test input-wait-handler
		   pointer-button-press-handler))
  (force-output stream)
  ;; make the output visible
  (let ((w (bounding-rectangle-max-x (stream-output-history stream)))
        (h (bounding-rectangle-max-y (stream-output-history stream))))
    (unless (region-contains-region-p (sheet-region stream)
                                      (make-rectangle* 0 0 w h))
      (change-space-requirements stream)
      (redisplay-frame-pane *application-frame* stream))))

(defmethod redisplay-frame-pane ((frame application-frame)
				 (pane symbol)
				 &key force-p)
  (let ((actual-pane (get-frame-pane frame pane)))
    (when actual-pane
      (redisplay-frame-pane frame actual-pane :force-p force-p))))

(define-presentation-method presentation-type-history-for-stream
    ((type t) (stream clim-stream-pane))
  (funcall-presentation-generic-function presentation-type-history type))

(defmethod %note-stream-end-of-page ((stream clim-stream-pane) action new-height)
  (change-stream-space-requirements stream :height new-height)
  (unless (eq :allow (stream-end-of-page-action stream))
    (scroll-extent stream 0 (max 0 (-  new-height
                                       (bounding-rectangle-height
                                        (or (pane-viewport stream)
                                            stream)))))))

;;; INTERACTOR PANES

(defclass interactor-pane (clim-stream-pane)
  ()
  (:default-initargs :display-time nil
                     :end-of-line-action :scroll
                     :scroll-bars :vertical
                     :incremental-redisplay t))

(defmethod initialize-instance :after ((pane interactor-pane) &rest args)
  (declare (ignore args)))

;;; KLUDGE: this is a hack to get keyboard focus (click-to-focus)
;;; roughly working for interactor panes.  It's a hack somewhat
;;; analogous to the mouse-wheel / select-and-paste handling in
;;; DISPATCH-EVENT, just in a slightly different place.
(defmethod frame-input-context-button-press-handler :before
    ((frame standard-application-frame) 
     (stream interactor-pane) 
     button-press-event)
  (let ((previous (stream-set-input-focus stream)))
    (when (and previous (typep previous 'gadget))
      (let ((client (gadget-client previous))
            (id (gadget-id previous)))
      (disarmed-callback previous client id)))))

;;; APPLICATION PANES

(defclass application-pane (clim-stream-pane)
  ()
  (:default-initargs :display-time :command-loop
                     :scroll-bars t))

;;; COMMAND-MENU PANE

(defclass command-menu-pane (clim-stream-pane)
  ()
  (:default-initargs :display-time :command-loop
                     :incremental-redisplay t
                     :scroll-bars t
                     :display-function 'display-command-menu))

;;; TITLE PANE

(defclass title-pane (clim-stream-pane)
  ()
  (:default-initargs :display-time t
                     :scroll-bars nil
                     :display-function 'display-title))

;;; Pointer Documentation Pane

(defparameter *default-pointer-documentation-background* +black+)
(defparameter *default-pointer-documentation-foreground* +white+)

(defclass pointer-documentation-pane (clim-stream-pane)
  ((background-message :initform nil
                       :accessor background-message
                       :documentation "An output record, or NIL, that will
be shown when there is no pointer documentation to show.")
   (background-message-time :initform 0
                            :accessor background-message-time
                            :documentation "The universal time at which the
current background message was set."))
  (:default-initargs 
   :display-time nil
   :scroll-bars nil
   :default-view +pointer-documentation-view+
   :height     '(2 :line)
   :min-height '(2 :line)
   :max-height '(2 :line)
   :text-style (make-text-style :sans-serif :roman :normal)
   :foreground *default-pointer-documentation-foreground*
   :background *default-pointer-documentation-background*
   :end-of-line-action :allow
   :end-of-page-action :allow))

(defmethod stream-accept :before ((stream pointer-documentation-pane) type
                                  &rest args)
  (declare (ignore args))
  (window-clear stream)
  (when (background-message stream)
    (setf (background-message stream) nil)
    (redisplay-frame-pane (pane-frame stream) stream)))

(defmethod stream-accept :around ((pane pointer-documentation-pane) type &rest args)
  (declare (ignore args))
  (unwind-protect (loop
                   (handler-case
                       (with-input-focus (pane)
                         (return (call-next-method)))
                     (parse-error () nil)))
    (window-clear pane)))


;;;
;;; CONSTRUCTORS
;;;

(defun make-clim-stream-pane (&rest options
                              &key (type 'clim-stream-pane)
                                (scroll-bars :vertical)
                                (borders t)
                                &allow-other-keys)
  (with-keywords-removed (options (:type :scroll-bars :borders))
    ;; The user space requirement options belong to the scroller ..
    (let* ((space-keys '(:width :height :max-width :max-height
			 :min-width :min-height))
	   (user-sr nil)
	   (pane-options nil))
      (loop  for (key value) on options by #'cddr
         if (and (member key space-keys :test #'eq)
                 (not (eq value :compute)))
         nconc (list key value) into space-options
         else
         nconc (list key value) into other-options
         end
         finally (progn
                   (setq user-sr space-options)
                   (setq pane-options other-options)))
      (let* ((pane (apply #'make-pane type (append pane-options
						   (unless (or scroll-bars
							       borders)
						     user-sr))))
	     (stream pane))
	(when scroll-bars
	  (setq pane (apply #'make-pane 'scroller-pane
			    :scroll-bar scroll-bars
			    :contents (list (make-pane 'viewport-pane
						       :contents (list pane)))
			    (unless borders
			      user-sr))))
	(when borders
	  (setq pane (apply #'make-pane 'border-pane
                            :border-width (if (not (numberp borders))
                                              1
                                              borders)
                            :contents (list pane)
                            user-sr)))
	(values pane stream)))))

(defun make-clim-interactor-pane (&rest options)
  (apply #'make-clim-stream-pane :type 'interactor-pane options))

(defun make-clim-application-pane (&rest options)
  (apply #'make-clim-stream-pane :type 'application-pane options))

(defun make-clim-pointer-documentation-pane (&rest options)
  (apply #'make-clim-stream-pane :type 'pointer-documentation-pane options))


;;;
;;; 29.4.5 Creating a Standalone CLIM Window
;;; WINDOW STREAM
;;;

(defclass window-stream (clim-stream-pane)
  ())

(define-application-frame a-window-stream (standard-encapsulating-stream
                                           standard-extended-input-stream
                                           fundamental-character-output-stream
                                           standard-application-frame)
  ((scroll-bars :initform :vertical
                :initarg :scroll-bars)
   stream
   pane)
  (:pane
   (with-slots (stream pane scroll-bars) *application-frame*
     (multiple-value-setq (pane stream)
       (make-clim-stream-pane
	:name 'a-window-stream-pane
	:display-time nil
	:type 'window-stream
	:scroll-bars scroll-bars
	:height 400 :width 700))
     pane)))

(defmethod close ((stream window-stream)
		  &key abort)
  (declare (ignore abort))
  (alexandria:when-let* ((frame (pane-frame stream))
			 (fm (frame-manager frame)))
    (disown-frame fm frame))
  (when (next-method-p)
    (call-next-method)))

(defun open-window-stream (&key port
                                left top right bottom width height
                                foreground background
                                text-style
                                (vertical-spacing 2)
                                end-of-line-action
                                end-of-page-action
                                output-record
                                (draw t)
                                (record t)
                                (initial-cursor-visibility :off)
                                text-margin
                                save-under
                                input-buffer
                                (scroll-bars :vertical)
                                borders
                                label)
  (declare (ignorable foreground background
                      text-style
                      vertical-spacing
                      end-of-line-action
                      end-of-page-action
                      output-record
                      draw
                      record
                      initial-cursor-visibility
                      text-margin
                      save-under
                      borders
                      label))
  (setf port (or port (find-port)))
  (let* ((fm (find-frame-manager :port port))
         (frame (make-application-frame 'a-window-stream
                                        :frame-event-queue input-buffer
                                        :frame-manager fm
                                        :pretty-name (or label "")
					:left left
					:top top
					:right right
					:bottom bottom
					:width width
					:height height
                                        :scroll-bars scroll-bars)))
    ;; Adopt and enable the pane
    (when (eq (frame-state frame) :disowned)
      (adopt-frame fm frame))
    (unless (or (eq (frame-state frame) :enabled)
		(eq (frame-state frame) :shrunk))
      (enable-frame frame))
    ;; Start a new thread to run the event loop, if necessary.
    (let ((*application-frame* frame))
      (stream-set-input-focus (encapsulating-stream-stream frame)))
    #+clim-mp
    (unless input-buffer
      (redisplay-frame-panes frame :force-p t)
      (clim-sys:make-process (lambda () (let ((*application-frame* frame))
                                          (standalone-event-loop)))))
    (encapsulating-stream-stream frame)))

(defun standalone-event-loop ()
  "An simple event loop for applications that want all events to be handled by
 handle-event methods, which also handles FRAME-EXIT."
  (let ((frame *application-frame*))
    (handler-case 
        (let ((queue (frame-event-queue frame)))
          (loop for event = (event-queue-read queue)
            ;; EVENT-QUEUE-READ in single-process mode calls PROCESS-NEXT-EVENT itself.
            do (handle-event (event-sheet event) event)))
      (frame-exit () (disown-frame (frame-manager frame) frame)))))


;;; These below were just hot fixes, are there still needed? Are even
;;; half-way correct? --GB
;;;
;;; These are needed, and are correct.  "Implementations should also
;;; provide a ``trampoline'' for this generic function for output sheets; the
;;; trampoline will simply call the method for the medium. -- moore
;;;
;;; Thanks! --GB
;;;
;;; Why are they placed here? -- APD

(defmethod text-size ((sheet sheet) string &rest more)
  (apply #'text-size (sheet-medium sheet) string more))

(defmethod text-style-ascent (ts (sheet sheet))
  (text-style-ascent ts (sheet-medium sheet)))

(defmethod text-style-descent (ts (sheet sheet))
  (text-style-descent ts (sheet-medium sheet)))

(defmethod text-style-height (ts (sheet sheet))
  (text-style-height ts (sheet-medium sheet)))

(defmethod text-style-width (ts (sheet sheet))
  (text-style-width ts (sheet-medium sheet)))

; timer-event convenience

(defmethod schedule-timer-event ((pane pane) token delay)
  (warn "Are you sure you want to use schedule-timer-event? It probably doesn't work.")
  (schedule-event pane (make-instance 'timer-event :token token :sheet pane) delay))
