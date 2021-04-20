;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2001 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2000 by Iban Hatchondo <hatchond@emi.u-bordeaux.fr>
;;;  (c) copyright 2000 by Julien Boninfante <boninfan@emi.u-bordeaux.fr>
;;;  (c) copyright 2001 by Lionel Salabartan <salabart@emi.u-bordeaux.fr>
;;;  (c) copyright 2001 by Arnaud Rouanet <rouanet@emi.u-bordeaux.fr>
;;;  (c) copyright 2001-2002, 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2002-2003 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of the 29.3 Composite and Layout Panes (layout protocol).
;;;

(in-package #:clim-internals)

;;; CLIM Layout Protocol for Dummies
;;;
;;; Here is how I interpret the relevant sections of the specification:
;;;
;;; COMPOSE-SPACE
;;;
;;;   This is called by CLIM, when it wants to find out what the pane
;;;   thinks are its space requirements. The result of COMPOSE-SPACE is
;;;   cached by CLIM.
;;;
;;; ALLOCATE-SPACE
;;;
;;;   This method is called by CLIM when a pane is allocate space. It
;;;   should layout its possible children.
;;;
;;; CHANGE-SPACE-REQUIREMENTS
;;;
;;;   This is called by the application programmer to a) indicate that
;;;   COMPOSE-SPACE may now return something different from previous
;;;   invocations and/or b) to update the user space requirements
;;;   options (the :width, :height etc keywords as upon pane creation).
;;;
;;; NOTE-SPACE-REQUIREMENTS-CHANGED
;;;
;;;   Called by CLIM when the space requirements of a pane have changed.
;;;
;;; LAYOUT-FRAME
;;;
;;;   May be called by both CLIM and the application programmer to "invoke the
;;;   space allocation protocol", that is CLIM calls ALLOCATE-SPACE on the top
;;;   level sheet. This in turn will probably call COMPOSE-SPACE on its
;;;   children and layout then accordingly by calling ALLOCATE-SPACE again.
;;;
;;;   The effect is that ALLOCATE-SPACE propagate down the sheet hierarchy.
;;;
;;; --GB 2003-08-06

(defconstant +fill+
  (expt 10 (floor (log most-positive-fixnum 10))))


;;; Space Requirements

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
  ;; Defensive programming. For instance SPACE-REQUIREMENT-+ may cause
  ;; max-{width,height} to be (+ +fill+ +fill+), what exceeds our biggest
  ;; allowed values. We fix that here.
  (clampf min-width 0 +fill+)
  (clampf max-width 0 +fill+)
  (clampf width min-width  max-width)
  (clampf min-height 0 +fill+)
  (clampf max-height 0 +fill+)
  (clampf height min-height max-height)
  (assert (<= min-width  max-width)  (min-width  max-width))
  (assert (<= min-height max-height) (min-height max-height))
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

(defmethod space-requirement-equal ((sr1 space-requirement) (sr2 space-requirement))
  (multiple-value-bind (width1 min-width1 max-width1 height1 min-height1 max-height1)
      (space-requirement-components sr1)
    (multiple-value-bind (width2 min-width2 max-width2 height2 min-height2 max-height2)
        (space-requirement-components sr2)
      (and (eql width1 width2) (eql min-width1 min-width2) (eql max-width1 max-width2)
           (eql height1 height2) (eql min-height1 min-height2) (eql max-height1 max-height2)))))

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
        (eq x :compute))))

(deftype spacing-value ()
  ;; just for documentation
  `(satisfies spacing-value-p))

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


;;; User space requirements
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
    :type     (or null spacing-value))
   (align-x
    :initarg :align-x
    :reader pane-align-x
    :type (member :left :center :right :expand))
   (align-y
    :initarg :align-y
    :reader pane-align-y
    :type (member :top :center :bottom :expand)))
  (:default-initargs
   :align-x :left
   :align-y :top)
  (:documentation
   "Mixin class for panes which offer the standard user space requirements options."))

(defmethod shared-initialize :after ((instance space-requirement-options-mixin)
                                     (slot-names t)
                                     &key
                                       (x-spacing nil x-spacing-p)
                                       (y-spacing nil y-spacing-p)
                                       (spacing nil spacing-p))
  (declare (ignore x-spacing y-spacing))
  (cond ((not spacing-p))
        (x-spacing-p
         (error #1="~@<The initargs ~S and ~S are mutually exclusive~@:>"
                :spacing :x-spacing))
        (y-spacing-p
         (error #1# :spacing :y-spacing))
        (t
         (setf (slot-value instance 'x-spacing) spacing
               (slot-value instance 'y-spacing) spacing))))

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

(defun merge-user-specified-options (pane sr)
  (check-type pane space-requirement-options-mixin)
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

(defmethod change-space-requirements :before
    ((pane space-requirement-options-mixin)
     &key
       (width :nochange) (min-width :nochange) (max-width :nochange)
       (height :nochange) (min-height :nochange) (max-height :nochange)
       (align-x :nochange) (align-y :nochange)
       (x-spacing :nochange) (y-spacing :nochange)
     &allow-other-keys)
  (macrolet ((update (parameter slot-name)
               `(unless (eq ,parameter :nochange)
                  (setf (slot-value pane ',slot-name) ,parameter))))
    (update width user-width)
    (update min-width user-min-width)
    (update max-width user-max-width)
    (update height user-height)
    (update min-height user-min-height)
    (update max-height user-max-height)
    (update align-x align-x)
    (update align-y align-y)
    (update x-spacing x-spacing)
    (update y-spacing y-spacing)))


;;; Layout protocol mixin

(defclass layout-protocol-mixin ()
  ((space-requirement
    :accessor pane-space-requirement
    :initform nil
    :documentation "The cache of the space requirements of the pane. NIL means: need to recompute.") ))

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
;;; --GB 2003-03-16

(defmethod compose-space :around ((pane layout-protocol-mixin) &key width height)
  (declare (ignore width height))
  (or (pane-space-requirement pane)
      (setf (pane-space-requirement pane)
            (call-next-method))))


;;; Changing space requirements

;;; Here is what we do:
;;;
;;; change-space-requirements (pane) :=
;;;   clear space requirements cache
;;;   call note-space-requirements-changed
;;;
;;; This is split into :before, primary and :after method to allow for
;;; easy overriding of change-space-requirements without needing to
;;; know the details of the space requirement cache and the
;;; note-space-requirements-changed notifications.
;;;
;;; If :resize-frame t the calls to change-space-requirements travel
;;; all the way up to the top-level-sheet-pane which then invokes the
;;; layout protocol calling layout-frame.
;;;
;;; In case this happens within changing-space-requirements layout
;;; frame is not called but simply recorded and then called when
;;; changing-space-requirements is left.

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
  ;; Clear the cached value
  (setf (pane-space-requirement pane) nil))

(defmethod change-space-requirements ((pane layout-protocol-mixin)
                                      &key resize-frame &allow-other-keys)
  (declare (ignore resize-frame))
  ;; do nothing here
  nil)

(defmethod change-space-requirements :after ((pane layout-protocol-mixin)
                                             &key resize-frame &allow-other-keys)
  (when-let ((parent (sheet-parent pane)))
    (if resize-frame
        ;; From Spec 29.3.4: "If resize-frame is true, then
        ;; layout-frame will be invoked on the frame". Here instead of
        ;; call directly LAYOUT-FRAME, we call
        ;; CHANGE-SPACE-REQUIREMENTS on the parent and it travels all
        ;; the way up to the top-level-sheet-pane which then invokes
        ;; the layout protocol calling LAYOUT-FRAME. The rationale of
        ;; this is:
        ;; 1. we can't call (LAYOUT-FRAME (PANE-FRAME pane)) on a
        ;;   menu because with the actual implementation of menu it
        ;;   will layout the main application and not the menu frame.
        ;; 2. we automatically clear the cached values of
        ;;    space-requirements for the involved panes.
        ;; -- admich 2020-08-11
        (if (top-level-sheet-pane-p pane)
            (note-space-requirements-changed parent pane)
            (change-space-requirements parent :resize-frame t))
        (note-space-requirements-changed parent pane))))

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
                             (if (frame-resize-frame frame)
                                 (layout-frame frame)
                                 (multiple-value-bind (width height)
                                     (bounding-rectangle-size pane)
                                   (layout-frame frame width height))))))
                     (t
                      (cond (resize-frame-2
                             (layout-frame frame))
                            (t
                             (if (frame-resize-frame frame)
                                 (layout-frame frame)
                                 (multiple-value-bind (width height)
                                     (bounding-rectangle-size pane)
                                   (layout-frame frame width height)))))))))))))
