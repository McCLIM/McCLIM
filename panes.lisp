;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2001 by
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2001 by
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)

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

;;; GENERIC FUNCTIONS

(defgeneric compose-space (pane))
(defgeneric allocate-space (pane width height))
(defgeneric change-space-requirements (pane &rest rest))
(defgeneric note-space-requirements-changed (sheet pane))

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

(defun make-space-requirement (&key (width 1) (height 1)
				    (min-width 0) (min-height 0)
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
    (space-requirement-combine* function
                                sr1
                                :width width :min-width min-width :max-width max-width
                                :height height :min-height min-height :max-height max-height)))

(defun space-requirement+ (sr1 sr2)
  (space-requirement-combine #'+ sr1 sr2))

(defun space-requirement+* (space-req &key (width 0) (min-width 0) (max-width 0)
                                      (height 0) (min-height 0) (max-height 0))
  (space-requirement-combine* #'+
                              space-req
                              :width width :min-width min-width :max-width max-width
                              :height height :min-height min-height :max-height max-height))

(deftype spacing-value ()
  ;; just for documentation
  `(satisfies spacing-value-p))

(defun spacing-value-p (x)
  (or (integerp x)
      (and (consp x)
           (realp (car x))
           (consp (cdr x))
           (member (cadr x) '(:point :pixel :mm :character :line))
           (null (cddr x)))))

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

(defclass pane (standard-sheet-input-mixin
		;;temporary-medium-sheet-output-mixin
		sheet-transformation-mixin
                layout-protocol-mixin
                basic-sheet
                )
  (
   (text-style :initarg :text-style :initform nil :reader pane-text-style)
   (name :initarg :name :initform "(Unnamed Pane)" :reader pane-name)
   (manager :initarg :manager)
   (port :initarg :port)
   (frame :initarg :frame :initform *application-frame* :reader pane-frame)
   (enabledp :initform nil :initarg :enabledp :accessor pane-enabledp)
   (sr-width :initform nil :initarg :width)
   (sr-height :initform nil :initarg :height)
   (sr-max-width :initform nil :initarg :max-width)
   (sr-max-height :initform nil :initarg :max-height)
   (sr-min-width :initform nil :initarg :min-width)
   (sr-min-height :initform nil :initarg :min-height)
   (space-requirement :initform nil :accessor pane-space-requirement)
   ;; New sizes, for allocating protocol
   (new-width :initform nil)
   (new-height :initform nil)
   )
  (:documentation ""))

(defmethod print-object ((pane pane) sink)
  (print-unreadable-object (pane sink :type t :identity t)
    (prin1 (pane-name pane) sink)))

(defun panep (x)
  (typep x 'pane))

(defun make-pane (type &rest args)
  (apply #'make-pane-1 *pane-realizer* *application-frame* type args))

(defmethod medium-foreground ((pane pane))
  (medium-foreground (sheet-medium pane)))

(defmethod (setf medium-foreground) (ink (pane pane))
  (setf (medium-foreground (sheet-medium pane)) ink))

(defmethod medium-background ((pane pane))
  (medium-background (sheet-medium pane)))

(defmethod (setf medium-background) (ink (pane pane))
  (setf (medium-background (sheet-medium pane)) ink))

(defmethod compose-space ((pane pane))
  (make-space-requirement :width 200
			  :height 200))

;;???
(defmethod allocate-space :before ((pane pane) width height)
  width height
  '(unless (typep 'pane 'top-level-sheet-pane)
    (resize-sheet pane width height)))

(defmethod allocate-space ((pane pane) width height)
  (declare (ignorable pane width height))
  )

;;???
(defmethod change-space-requirements ((pane pane) &rest rest)
  (declare (ignore rest))
  (values))

;;???
(defmethod note-space-requirements-changed (sheet (pane pane))
  (declare (ignore sheet))
  (setf (pane-space-requirement pane) nil)
  (compose-space pane)
  (if (or (top-level-sheet-pane-p pane) 
	  (restraining-pane-p pane)
	  (and (slot-value pane 'sr-width) 
	       (slot-value pane 'sr-height)))
      (allocate-space pane (sr-width pane) (sr-height pane))
      (note-space-requirements-changed (sheet-parent pane) pane)))

;;; WINDOW STREAM

;; ???
(defclass window-stream (standard-extended-output-stream 
			 standard-extended-input-stream)
  () )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Since, I hate to duplicate code for HBOX and VBOX, I define this
;; evil macro:

(defmacro dada ((&rest substs) &body body)
  "This is an evil macro."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;

;; All panes allow for user specified space requirement options, so we
;; define a mixin class to hold the corresponding slots:

(defclass space-requirement-options-mixin ()
  (;; These slots correspond to the options given by the user.
   (user-width       :reader pane-user-width
                     :type   spacing-value)
   (user-min-width   :reader pane-user-min-width
                     :type   spacing-value)
   (user-max-width   :reader pane-user-max-width
                     :type   spacing-value)
   (user-height      :reader pane-user-height
                     :type   spacing-value)
   (user-min-height  :reader pane-user-min-height
                     :type   spacing-value)
   (user-max-height  :reader pane-user-max-height
                     :type   spacing-value)
   (x-spacing        :reader pane-x-spacing
                     :type spacing-value)
   (y-spacing        :reader pane-y-spacing
                     :type spacing-value)))

;; The exact behaviour though is different:

(defclass standard-space-requirement-options-mixin (space-requirement-options-mixin) ())
(defclass border-space-requirement-options-mixin   (space-requirement-options-mixin) ())

;; 

(defmethod grok-user-space-requirement-options ((pane space-requirement-options-mixin)
                                                &key width max-width min-width
                                                     height max-height min-height
                                                     x-spacing y-spacing spacing
                                                &allow-other-keys)
  (setf (slot-value pane 'x-spacing) (spacing-value-to-device-units pane (or x-spacing spacing 0))
        (slot-value pane 'y-spacing) (spacing-value-to-device-units pane (or y-spacing spacing 0)) 
        (slot-value pane 'user-width) width
        (slot-value pane 'user-min-width) (or min-width width)
        (slot-value pane 'user-max-width) (or max-width width)
        (slot-value pane 'user-height) height
        (slot-value pane 'user-min-height) (or min-height height)
        (slot-value pane 'user-max-height) (or max-height height) ))

(defmethod initialize-instance :after ((pane space-requirement-options-mixin)
                                       &rest space-req-keys)
  (apply #'grok-user-space-requirement-options pane space-req-keys))

(defmethod compose-space :around ((pane standard-space-requirement-options-mixin))
  ;; merge user specified options.
  (let ((sr (call-next-method)))
    (unless sr
      (warn "~S has no idea about its speac-requirements." pane)
      (setf sr (make-space-requirement :width 100 :height 100)))
    (multiple-value-bind (width min-width max-width height min-height max-height)
        (space-requirement-components sr)
      (with-slots (user-width user-min-width user-max-width
                   user-height user-min-height user-max-height) pane

        (dada ((foo width height))
              ;; the user wins on the "primary" dimensions
              (when user-foo
                (setf foo (spacing-value-to-device-units pane user-foo)))
              ;; 
              (setf min-foo 
                (clamp
                 (cond ((and (consp user-min-foo) (eq (cadr user-min-foo) :relative))
                        (- foo (car user-min-foo)))
                       ((not (null user-min-foo))
                        (spacing-value-to-device-units pane user-min-foo))
                       (t
                        min-foo))
                 0 foo))
              ;;
              (setf max-foo 
                (max foo
                     (cond ((and (consp user-max-foo) (eq (cadr user-max-foo) :relative))
                            (+ foo (car user-max-foo)))
                           ((not (null user-max-foo))
                            (spacing-value-to-device-units pane user-max-foo))
                           (t
                            max-foo))))))
      (make-space-requirement
       :width width
       :min-width min-width
       :max-width max-width
       :height height
       :min-height min-height
       :max-height max-height) )))


;;

(defclass layout-protocol-mixin ()
  ((space-requirement :accessor pane-space-requirement
                      :initform nil)
   (current-width     :accessor pane-current-width
                      :initform nil)
   (current-height    :accessor pane-current-height
                      :initform nil) ))

(defmethod allocate-space :around ((pane layout-protocol-mixin) width height)
  (unless (and (eql (pane-current-width pane) width)
               (eql (pane-current-height pane) height))
    (setf (pane-current-width pane) width
          (pane-current-height pane) height)
    (unless (typep pane 'top-level-sheet-pane)
      (resize-sheet pane width height))
    (call-next-method)))

(defmethod compose-space :around ((pane layout-protocol-mixin))
  (or (pane-space-requirement pane)
      (setf (pane-space-requirement pane)
        (call-next-method))))

(defmethod change-space-requirements ((pane layout-protocol-mixin) &rest space-req-keys &key resize-frame &allow-other-keys)
  (apply #'grok-user-space-requirement-options pane space-req-keys)
  (note-space-requirements-changed (sheet-parent pane) pane))

(defmethod note-space-requirements-changed ((sheet layout-protocol-mixin) (pane layout-protocol-mixin))
  (setf (pane-space-requirement pane) (compose-space pane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BASIC-PANE

(defclass basic-pane (;; layout-protocol-mixin
                      standard-space-requirement-options-mixin
                      sheet-parent-mixin mirrored-sheet-mixin pane)
  ((foreground       :initarg :foreground
                     :initform +black+
                     :reader pane-foreground)
   (background       :initarg :background
                     :initform +white+
                     :reader pane-background)
   (text-style       :initarg :text-style
                     :initform *default-text-style*
                     :reader pane-text-style)
         
   (align-x          :initarg :align-x
                     :type (member :left :center :right)
                     :initform :left          ;??
                     :reader pane-align-x)
   (align-y          :initarg :align-y
                     :type (member :top :center :bottom)
                     :initform :top           ;??
                     :reader pane-align-y) ))

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
  ((contents :initform nil :initarg :contents))
  (:documentation "protocol class"))

(defclass content ()
  ((pane       :initarg :pane
               :initform nil
               :reader content-pane
               :type (or pane null)
               :documentation "The particular pane, this content specifiction belongs to."))
  (:documentation
   "This class represents one item of composite-pane's contents list."))

(defmethod initialize-instance :after ((pane composite-pane)
				       &rest args
				       &key contents
				       &allow-other-keys)
  (declare (ignore args))
  (setf (slot-value pane 'contents) (parse-contents pane contents))
  (dolist (content (slot-value pane 'contents))
    (when (content-pane content)
      (sheet-adopt-child pane (content-pane content)))))

(defmethod parse-contents ((pane pane) contents)
  (mapcar #'(lambda (content)
              (cond ((panep content)
                     (make-instance 'plain-content :pane content))
                    ((or (eql content +fill+) (eql content '+fill+))
                     (make-instance 'fill-content))
                    ((and (realp content) (>= content 0))
                     (make-instance 'constant-content :amount content))
                    ((and (consp content)
                          (realp (car content))
                          (>= (car content) 0)
                          (consp (cdr content))
                          (panep (cadr content))
                          (null (cddr content)))
                     (let ((number (car content))
                           (pane   (cadr content)))
                       (if (< number 1)
                           (make-instance 'proportional-content :pane pane :proportion number)
                         (make-instance 'constant-content :pane pane :amount number))))
                    (t
                     (error "~S is not a valid element in the ~S option of ~S."
                            content :contents pane))))
          contents))

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

(defmacro changing-space-requirement (&body body &key resize-frame)
  (declare (ignore resize-frame))
  `(progn
     ,@body))

(defmethod change-space-requirements ((pane composite-pane)
				      &key resize-frame
				      (width nil width-p)
				      (min-width nil min-width-p)
				      (max-width nil max-width-p)
				      (height nil height-p)
				      (min-height nil min-height-p)
				      (max-height nil max-height-p))
  (with-slots (sr-width sr-height sr-max-width sr-max-height
	       sr-min-width sr-min-height) pane
    (when width-p (setf sr-width width))
    (when min-width-p (setf sr-min-width min-width))
    (when max-width-p (setf sr-max-width max-width))
    (when height-p (setf sr-height height))
    (when min-height-p (setf sr-min-height min-height))
    (when max-height-p (setf sr-max-height max-height)))
  (if resize-frame
      ;; we didn't find the :resize-frame option in define-application-frame
      (layout-frame (pane-frame pane))
      (note-space-requirements-changed (sheet-parent pane) pane)))

;;; SINGLE-CHILD-COMPOSITE PANE

(defclass single-child-composite-pane (sheet-single-child-mixin
				       basic-pane)
  (
   )
  )


(defmethod initialize-instance :after ((pane single-child-composite-pane)
				       &rest args
				       &key contents
				       &allow-other-keys)
  (declare (ignore args))
  (when contents
    (sheet-adopt-child pane (first contents))))

(defmethod compose-space ((pane single-child-composite-pane))
  (if (sheet-children pane)
      (compose-space (first (sheet-children pane)))
      (make-space-requirement)))

(defmethod allocate-space ((pane single-child-composite-pane) width height)
  (when (first (sheet-children pane))
    (allocate-space (first (sheet-children pane)) width height)))

;;; TOP-LEVEL-SHEET

(defclass top-level-sheet-pane (composite-pane)
  ()
  (:documentation "For the first pane in the architecture"))

(defun top-level-sheet-pane-p (pane)
  (typep pane 'top-level-sheet-pane))

(defmethod compose-space ((pane top-level-sheet-pane))
  (compose-space (first (sheet-children pane))))

(defmethod allocate-space ((pane top-level-sheet-pane) width height)
  (unless (pane-space-requirement pane)
    (setf (pane-space-requirement pane)
      (compose-space pane)))
  (when (first (sheet-children pane))
    (allocate-space 
        (first (sheet-children pane))
	(clamp width (sr-min-width pane) (sr-max-width pane))
	(clamp height (sr-min-height pane) (sr-max-height pane)))))

(defmethod dispatch-event ((pane top-level-sheet-pane) event)
  (handle-event pane event))

(defmethod handle-event ((pane top-level-sheet-pane)
			 (event window-configuration-event))
  (let ((x (window-configuration-event-x event))
	(y (window-configuration-event-y event))
	(width (window-configuration-event-width event))
        (height (window-configuration-event-height event)))
    ;; avoid goint into an infinite loop by not using (setf sheet-transformation)
    (setf (slot-value pane 'transformation)
	  (make-translation-transformation x y))
    ;; avoid goint into an infinite loop by not using (setf sheet-region)
    (setf (slot-value pane 'region)
	  (make-bounding-rectangle 0 0 width height))
    (allocate-space pane width height)))

(defclass unmanaged-top-level-sheet-pane (top-level-sheet-pane)
  ()
  (:documentation "Top-level sheet without window manager intervention"))

(defmethod sheet-native-transformation ((sheet top-level-sheet-pane))
  +identity-transformation+)

;;; SHEET 

;; FIXME: Should it exists ???
(defmethod note-space-requirements-changed ((sheet sheet) (pane composite-pane))
  (values))

;;; Special content classes for VBOX/HBOX

(defclass fill-content (content) ())

(defclass constant-fill-content (content) 
  ((amount :initarg :amount 
           :reader constant-fill-content-amount)))

(defclass proportional-content (content)
  ((proportion :initarg :proportion
               :initform nil
               :type (or (real 0 1) null)
               :reader content-proportion
               :documentation "User specified proportion or NIL.")))

(defclass constant-content (content) 
  ((amount :initarg :amount 
           :reader constant-content-amount)))

(defclass plain-content (content) ())

(dada
 ((major   width        height)
  (minor   height       width)
  (xbox    hbox         vbox)
  (xrack   hrack        vrack)
  (xically horizontally vertically))

 (defmacro xically ((&rest options
                     &key equalize-minor
                     &allow-other-keys)
                    &body contents)
   (remf options :equalize-minor)
   `(make-pane ',(if equalize-minor
                     'xrack-pane
                   'xbox-pane)
               ,@options
               :contents (list ,@(mapcar (lambda (content)
                                           (cond ((and (consp content)
                                                       (realp (first content)))
                                                  `(list ',(first content)
                                                         ,(second content)))
                                                 (t
                                                  content)))
                                         contents))))

 (defclass xbox-pane (composite-pane 
                      permanent-medium-sheet-output-mixin ;arg!
                      )
   ()
   (:documentation ""))
 

 (defmethod content-sr* ((box xbox-pane) (content fill-content))
   (declare (ignorable box content))
   (make-space-requirement
    :major 0
    :min-major 0
    :max-major +fill+
    :minor 0
    :min-minor 0
    :max-minor 0))

 (defmethod content-sr* ((box xbox-pane) (content constant-content))
   (declare (ignorable box content))
   (let ((x (constant-content-amount content))
         (sr (and (content-pane content)
                  (compose-space (content-pane content)))))
     (make-space-requirement
      :major     x
      :min-major x
      :max-major x
      :minor     (if sr (space-requirement-minor sr) 0)
      :min-minor (if sr (space-requirement-min-minor sr) 0)
      :max-minor (if sr (space-requirement-max-minor sr) 0))))

 (defmethod content-sr* ((box xbox-pane) (content proportional-content))
   (compose-space (content-pane content)))

 (defmethod content-sr* ((box xbox-pane) (content plain-content))
   (compose-space (content-pane content)))
 
 (defmethod compose-space ((box xbox-pane))
   (with-slots (contents) box
     (loop
         for content in contents
         for sr = (content-sr* box content)
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
              :max-minor (max max-minor minor))))))

 (defmethod allocate-space-aux ((box xbox-pane) width height)
   (with-slots (contents) box
     (let* ((content-srs (mapcar #'(lambda (c) (content-sr* box c)) contents))
            (allot       (mapcar #'ceiling (mapcar #'space-requirement-major content-srs)))
            (wanted      (reduce #'+ allot))
            (excess      (- major wanted)))
       #+NIL
       (progn
         (format T "~&;; ALLOCATE-SPACE-AUX ~S~%" box)
         (format T "~&;;   major = ~D, wanted = ~D, excess = ~D, allot = ~D.~%"
                 major wanted excess allot))

       (let ((qvector
              (mapcar (lambda (c)
                        (typecase c
                          (fill-content
                           (vector 1 0 0))
                          (proportional-content
                           (vector 0 (content-proportion c) 0))
                          (t
                           (vector 0 0 (abs (- (if (> excess 0)
                                                   (space-requirement-max-major (content-sr* box c))
                                                 (space-requirement-min-major (content-sr* box c)))
                                               (space-requirement-major (content-sr* box c))))))))
                      contents)))
         ;;
         #+NIL
         (progn
           (format T "~&;;   old allotment = ~S.~%" allot)
           (format T "~&;;   qvector 0 = ~S.~%" (mapcar #'(lambda (x) (elt x 0)) qvector))
           (format T "~&;;   qvector 1 = ~S.~%" (mapcar #'(lambda (x) (elt x 1)) qvector))
           (format T "~&;;   qvector 2 = ~S.~%" (mapcar #'(lambda (x) (elt x 2)) qvector)))
         ;;
         (dotimes (j 3)
           (let ((sum (reduce #'+ (mapcar (lambda (x) (elt x j)) qvector))))
             (unless (zerop sum)
               (setf allot
                 (mapcar (lambda (allot content q)
                           (let ((q (elt q j)))
                             (let ((delta (ceiling (if (zerop sum) 0 (/ (* excess q) sum)))))
                               (decf excess delta)
                               (decf sum q)
                               (+ allot delta))))
                         allot contents qvector))
               #+NIL
               (format T "~&;;   new excess = ~F, allotment = ~S.~%" excess allot)
               )))
         ;;
         #+NIL (format T "~&;;   excess = ~F.~%" excess)
         #+NIL (format T "~&;;   new allotment = ~S.~%" allot)

         (values allot
                 (mapcar #'ceiling (mapcar #'space-requirement-minor content-srs))) ))))
 
 (defmethod allocate-space ((box xbox-pane) width height)
   (declare (ignore minor))
   (with-slots (contents) box
     (multiple-value-bind (majors minors) (allocate-space-aux box width height)
       ;; now actually layout the children
       (let ((x 0))
         (loop 
             for content in contents 
             for major in majors 
             for minor in minors
             do
               (let ((child (content-pane content)))
                 (when child
                   #+NIL
                   (format T "~&;;   child ~S at 0, ~D ~D x ~D~%" 
                           child x width height)
                   (move-sheet child
                               ((lambda (major minor) height width) x 0)
                               ((lambda (major minor) width height) x 0))
                   (allocate-space child 
                                   width height)
                   )
                 (incf x major)))))))
 
 (defclass xrack-pane (xbox-pane) ())
 
 (defmethod allocate-space-aux :around ((box xrack-pane) width height)
   (multiple-value-bind (majors minors) (call-next-method)
     (values majors
             (mapcar (lambda (x) x minor) minors))))
 )

;;; TABLE PANE

(defclass table-pane (composite-pane)
  (array)
  (:documentation "The table layout implies that each colums has the same width
 and each lines has the same height - same rules for max and min -")
  )

(defclass table-content (content)
  ((row :initarg :row :reader table-content-row)
   (col :initarg :col :reader table-content-col)
   ))

(defmethod parse-contents ((pane table-pane) contents)
  (loop 
      for row in contents
      for i from 0
      nconc (loop
                for col in row
                for j from 0
                collect (make-instance 'table-content 
                          :pane col
                          :row i
                          :col j))))

(defmethod initialize-instance :after ((pane table-pane) &key &allow-other-keys)
  (with-slots (contents array) pane
    (let ((nrows (1+ (loop for c in contents maximize (table-content-row c))))
          (ncols (1+ (loop for c in contents maximize (table-content-col c)))))
      (setf array (make-array (list nrows ncols) :initial-element nil))
      (dolist (c contents)
        (setf (aref array (table-content-row c) (table-content-col c)) c)))))

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
        (let* ((allot (mapcar #'space-requirement-major srs))
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
            (unless (zerop sum)
              (setf allot
                (mapcar (lambda (allot q)
                          (let ((delta (ceiling (if (zerop sum) 0 (/ (* excess q) sum)))))
                            (decf excess delta)
                            (decf sum q)
                            (+ allot delta)))
                        allot qs))))
          allot)) )

(defmethod table-pane-row-space-requirement ((pane table-pane) i)
  (with-slots ( array) pane
    (stack-space-requirements-horizontally
     (loop for j from 0 below (array-dimension array 1)
         collect (compose-space (content-pane (aref array i j)))))))

(defmethod table-pane-col-space-requirement ((pane table-pane) j)
  (with-slots ( array) pane
    (stack-space-requirements-vertically
     (loop for i from 0 below (array-dimension array 0)
         collect (compose-space (content-pane (aref array i j)))))))

(defmethod compose-space ((pane table-pane))
  (with-slots (contents array) pane
    (let ((rsrs (loop for i from 0 below (array-dimension array 0) collect (table-pane-row-space-requirement pane i)))
          (csrs (loop for j from 0 below (array-dimension array 1) collect (table-pane-col-space-requirement pane j))))
      (let ((r (stack-space-requirements-vertically rsrs))
            (c (stack-space-requirements-horizontally csrs)))
        (let ((res
               (make-space-requirement
                :width (space-requirement-width r)
                :min-width (space-requirement-min-width r)
                :max-width (space-requirement-max-width r)
                :height (space-requirement-height c)
                :min-height (space-requirement-min-height c)
                :max-height (space-requirement-max-height c))))
          #+NIL (format T "~%;;; TABLE-PANE sr = ~S." res)
          res)))))

(defmethod allocate-space ((pane table-pane) width height &aux rsrs csrs)
  (with-slots (contents array) pane
    ;; allot rows
    (let ((rows (allot-space-vertically
                 (setq rsrs (loop for i from 0 below (array-dimension array 0) collect (table-pane-row-space-requirement pane i)))
                 height))
          (cols (allot-space-horizontally
                 (setq csrs (loop for j from 0 below (array-dimension array 1) collect (table-pane-col-space-requirement pane j)))
                 width)))
      #+NIL(progn
             (format T "~&;; row space requirements = ~S." rsrs)
             (format T "~&;; col space requirements = ~S." csrs)
             (format T "~&;; row allotment: needed = ~S result = ~S." height rows)
             (format T "~&;; col allotment: needed = ~S result = ~S." width cols))
      ;; now finally layout each child
      (loop
          for y = 0 then (+ y h)
          for h in rows
          for i from 0
          do (loop
                 for x = 0 then (+ x w)
                 for w in cols
                 for j from 0
                 do (progn
                      (move-sheet (content-pane (aref array i j)) x y)
                      (allocate-space (content-pane (aref array i j)) w h)))))))
#||
(defmethod initialize-instance :before ((table table-pane)
					&rest args
					&key contents
					&allow-other-keys)
  (declare (ignore args))
  (unless (apply #'= (mapcar #'length contents))
    (error "The variable contents hasn't the good format")))
||#

(defun table-pane-p (pane)
  (typep pane 'table-pane))

(defmacro tabling ((&rest options &key (grid nil) &allow-other-keys) &body contents)
  (if grid
      `(make-pane 'grid-pane ,@options :contents (list ,@contents))
      `(make-pane 'table-pane ,@options :contents (list ,@contents))))



;(defmethod sheet-adopt-child :before ((table table-pane) child)
;  (declare (ignore child))
;  (when (= (length (sheet-children table)) (table-pane-number table))
;    (error "The table can't adopt more childs than specified by the table-number")))

(defmethod sheet-disowned-child :before ((table table-pane) child
					 &key (error-p t))
  (declare (ignore child error-p))
  (error "The table pane can't disown one of its child"))


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

(defmethod compose-space ((grid grid-pane))
  (mapc #'compose-space (sheet-children grid))
  (loop with nb-children-pl = (table-pane-number grid)
	with nb-children-pc = (/ (length (sheet-children grid)) nb-children-pl)
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
		  :min-height (* min-height nb-children-pc)))))
     
(defmethod allocate-space ((grid grid-pane) width height)
  (loop with nb-kids-p-l = (table-pane-number grid)
	with nb-kids-p-c = (/ (length (sheet-children grid)) nb-kids-p-l)
	for children in (format-children grid) 
	for c from nb-kids-p-c downto 1
	for tmp-height = height then (decf tmp-height new-height)
	for new-height = (/ tmp-height c)
	for y = 0 then (+ y new-height)
	do (loop for child in children
		 for l from nb-kids-p-l downto 1
		 for tmp-width = width then (decf tmp-width new-width)
		 for new-width = (/ tmp-width l)
		 for x = 0 then (+ x new-width)
		 do (move-sheet child x y)
		    (allocate-space child (round new-width) (round new-height)))))

;;; SPACING PANE

(defclass spacing-pane (composite-pane)
  ()
  (:documentation "The spacing pane will create a margin for his child.
The margin sizes (w h) are given with the :width and :height initargs.
During realization the child of the spacing will have as cordinates
 x = w/2 , y = h/2."))

(defmacro spacing ((&rest options) &body contents)
  `(make-pane 'spacing-pane ,@options :contents (list ,@contents)))

(defun spacing-p (pane)
  (typep pane 'spacing-pane))

(defmethod parse-contents ((pane spacing-pane) contents)
  (cond ((and (consp contents)
              (null (cdr contents))
              (panep (car contents)))
         (list (make-instance 'plain-content :pane (car contents))))
        (t
         (error "Bogus ~S option: ~S. A ~S needs exactly one child not more, not less."
                :contents
                'spacing-pane))))

(defmethod initialize-instance :after ((spacing spacing-pane) &key thickness &allow-other-keys)
  (with-slots (user-width user-min-width user-max-width
               user-height user-min-height user-max-height)
      spacing
    (setf user-width  (max (or thickness 0) (or user-width 0)))
    (setf user-height (max (or thickness 0) (or user-height 0)))
    ;; hmm
    ))

(defmethod compose-space ((spacing spacing-pane))
  (with-slots (contents
               user-width user-min-width user-max-width
               user-height user-min-height user-max-height) spacing
    (let ((sr (compose-space (content-pane (first contents)))))
      (make-space-requirement
       :width           (+ (or user-width 0) (space-requirement-width sr))
       :height          (+ (or user-height 0) (space-requirement-height sr))
       :min-width       (min (+ (or user-width 0) (space-requirement-width sr))
                             (+ (space-requirement-min-width sr) (or user-min-width 0)))
       :min-height      (min (+ (or user-height 0) (space-requirement-height sr))
                             (+ (space-requirement-min-height sr) (or user-min-height 0)))
       :max-width       (max (+ (or user-width 0) (space-requirement-width sr))
                             (+ (space-requirement-max-width sr) (or user-max-width 0)))
       :max-height      (max (+ (or user-height 0) (space-requirement-height sr))
                             (+ (space-requirement-max-height sr) (or user-max-height 0))) ))))

(defmethod allocate-space ((spacing spacing-pane) width height)
  (with-slots (contents user-width user-height) spacing
    (allocate-space (content-pane (first contents))
                    (- width user-width)
                    (- height user-height))
    (move-and-resize-sheet (content-pane (first contents))
                           (/ user-width 2) (/ user-height 2)
                           (- width user-width)
                           (- height user-height))))
   
;;; BORDER-PANE

(defclass border-pane (spacing-pane)
  ((border-width :initarg :border-width :initform 1 :reader border-pane-width)
   )
  (:documentation ""))

(defmethod initialize-instance :after ((bp border-pane) &rest ignore)
  (declare (ignore ignore))
  #+NIL
  (with-slots (border-width) bp
    (let ((2*border-width (* 2 border-width)))
      (setf (slot-value bp 'margin-width) 2*border-width
	    (slot-value bp 'margin-height) 2*border-width 
	    (slot-value bp 'margin-max-width) 2*border-width
	    (slot-value bp 'margin-max-height) 2*border-width
	    (slot-value bp 'margin-min-width) 2*border-width
	    (slot-value bp 'margin-min-height) 2*border-width))))

(defmacro bordering ((&rest options) &body contents)
  `(make-pane 'border-pane ,@options :contents (list ,@contents)))

(defmethod allocate-space ((bp border-pane) width height)
  (when (first (sheet-children bp))
    (let ((border-width (border-pane-width bp)))
      (setf (sheet-transformation (first (sheet-children bp)))
	    (make-translation-transformation border-width border-width))
      (allocate-space (first (sheet-children bp))
		      (- width (* 2 border-width))
		      (- height (* 2 border-width))))))

;;; RAISED PANE

(defclass raised-pane (border-pane permanent-medium-sheet-output-mixin) ())

(defmacro raising ((&rest options) &body contents)
  `(make-pane 'raised-pane ,@options :contents (list ,@contents)))

(defmethod dispatch-repaint ((raised-pane raised-pane) region)
  (repaint-sheet raised-pane region))

(defmethod handle-event ((pane raised-pane) (event window-repaint-event))
  (dispatch-repaint pane (sheet-region pane)))

(defmethod repaint-sheet ((pane raised-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
      (draw-edges-lines* pane 0 0 (- x2 x1 1) (- y2 y1 1)))))

;;; RESTRAINING PANE

(defclass restraining-pane (composite-pane) ())

(defun restraining-pane-p (pane)
  (typep pane 'restraining-pane))

(defmacro restraining ((&rest options) &body contents)
  `(make-pane 'restraining-pane ,@options :contents (list ,@contents)))

;;; BBOARD PANE

(defclass bboard-pane (composite-pane) ())

(defmethod compose-space ((bboard bboard-pane))
  (make-space-requirement :width 300 :height 300))

;;; VIEWPORT

(defclass viewport-pane (single-child-composite-pane) ())

(defmethod allocate-space ((pane viewport-pane) w h)
  (allocate-space (first (sheet-children pane))
                  (space-requirement-width (compose-space (first (sheet-children pane))))
                  (space-requirement-height (compose-space (first (sheet-children pane))))))

;;; SCROLLER-PANE

(defparameter *scrollbar-thickness* 15)

(defclass scroller-pane (composite-pane)
  ((scroll-bar :type (member '(t :vertical :horizontal))
	       :initform t
	       :initarg :scroll-bar
	       :accessor scroller-pane-scroll-bar)
   (viewport :initform nil)
   (vscrollbar :initform nil)
   (hscrollbar :initform nil)))

(defun scroll-page-callback (scroll-bar direction)
  (with-slots (client orientation) scroll-bar
    (multiple-value-bind (old-x old-y)
        (untransform-position (sheet-transformation client)
                              0 0)
      (if (eq orientation :vertical)
          (scroll-extent client
                         old-x
                         (- old-y
                            (* direction
                               (bounding-rectangle-height
                                (pane-viewport-region client)))))
          (scroll-extent client
                         (- old-x
                            (* direction
                               (bounding-rectangle-width
                                (pane-viewport-region client))))
                         old-y)))))

(defun scroll-line-callback (scroll-bar direction)
  (with-slots (client orientation) scroll-bar
    (multiple-value-bind (old-x old-y)
        (untransform-position (sheet-transformation client)
                              0 0)
      (if (eq orientation :vertical)
          (scroll-extent client
                         old-x
                         (- old-y
                            (* direction
                               (stream-line-height client))))
          (scroll-extent client
                         (- old-x
                            (* direction
                               (stream-line-height client)))
                         old-y)))))

(defmethod initialize-instance :after ((pane scroller-pane) &rest args)
  (declare (ignore args))
  (with-slots (scroll-bar viewport vscrollbar hscrollbar) pane
    (setq viewport (first (sheet-children pane)))
    (when (not (eq scroll-bar :horizontal))
      (setq vscrollbar
            (make-pane 'scroll-bar-pane
                       :orientation :vertical
                       :client (first (sheet-children viewport))
                       :drag-callback
                       #'(lambda (gadget new-value)
                           (scroll-extent (gadget-client gadget)
                                          0
                                          (* new-value 
                                             (- (bounding-rectangle-max-y
                                                 (sheet-region
                                                  (gadget-client gadget)))
                                                (bounding-rectangle-max-y
                                                 (sheet-region
                                                  viewport)))))
                           #+NIL
                           (let ((old-x (bounding-rectangle-min-x
                                         (pane-viewport-region
                                          (gadget-client gadget)))))
                             (scroll-extent (gadget-client gadget)
                                            old-x new-value)))
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
                       :foreground +grey+
                       :background +grey40+))
      (sheet-adopt-child pane vscrollbar))
    (when (not (eq scroll-bar :vertical))
      (setq hscrollbar
            (make-pane 'scroll-bar-pane
                       :orientation :horizontal
                       :length (bounding-rectangle-width (sheet-region viewport))
                       :client (first (sheet-children viewport))
                       :drag-callback
                       #'(lambda (gadget new-value)
                           (let ((old-y (bounding-rectangle-min-y
                                         (pane-viewport-region
                                          (gadget-client gadget)))))
                             (scroll-extent (gadget-client gadget)
                                            new-value old-y)))
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
                       :foreground +grey+
                       :background +grey40+))
      (sheet-adopt-child pane hscrollbar))))
    
(defmacro scrolling ((&rest options) &body contents)
  `(let ((viewport (make-pane 'viewport-pane :contents (list ,@contents))))
     (make-pane 'scroller-pane ,@options :contents (list viewport))))

(defmethod compose-space ((pane scroller-pane))
  (with-slots (viewport vscrollbar hscrollbar) pane
    (if viewport
        (let ((req (compose-space viewport)))
          (when vscrollbar
            (setq req (space-requirement+* req
                                           :height *scrollbar-thickness*
                                           :min-height *scrollbar-thickness*
                                           :max-height *scrollbar-thickness*)))
          (when hscrollbar
            (setq req (space-requirement+* req
                                           :width *scrollbar-thickness*
                                           :min-width *scrollbar-thickness*
                                           :max-width *scrollbar-thickness*)))
          req)
        (make-space-requirement))))

(defmethod allocate-space ((pane scroller-pane) width height)
  (with-slots (viewport vscrollbar hscrollbar) pane
    (when viewport
      (setf (sheet-transformation viewport)
	(make-translation-transformation (if vscrollbar *scrollbar-thickness* 0) 0))
      (allocate-space viewport
		      (if vscrollbar (- width *scrollbar-thickness*) width)
		      (if hscrollbar (- height *scrollbar-thickness*) height)))
    (when vscrollbar
      (setf (sheet-transformation vscrollbar)
	(make-translation-transformation 0 0))
      (allocate-space vscrollbar
		      *scrollbar-thickness*
		      (if hscrollbar (- height *scrollbar-thickness*) height)))
    (when hscrollbar
      #+NIL
      (setf (sheet-transformation hscrollbar)
	(make-translation-transformation (if vscrollbar
                                             *scrollbar-thickness*
                                             0)
                                         (- height *scrollbar-thickness*)))
      (move-sheet hscrollbar
                  (if vscrollbar
                      *scrollbar-thickness*
                    0)
                  (- height *scrollbar-thickness*))
      (allocate-space hscrollbar
		      (if vscrollbar (- width *scrollbar-thickness*) width)
		      *scrollbar-thickness*))))

(defun is-in-scroller-pane (pane)
  (let ((parent (sheet-parent pane)))
    (and (typep parent 'viewport-pane)
         (typep (sheet-parent parent) 'scroller-pane))))

(defmethod pane-viewport ((pane basic-pane))
  (when (is-in-scroller-pane pane)
    (sheet-parent pane)))

(defmethod pane-viewport-region ((pane basic-pane))
  (when (is-in-scroller-pane pane)
    (sheet-region pane)))

(defmethod pane-scroller ((pane basic-pane))
  (when (is-in-scroller-pane pane)
    (sheet-parent (sheet-parent pane))))

(defun update-scroll-bars (pane entire-region x y)
  #+NIL
  (multiple-value-bind (min-x min-y max-x max-y) (bounding-rectangle* entire-region)
    (with-slots (vscrollbar hscrollbar viewport) (pane-scroller pane)
      (when vscrollbar
	(with-slots (value) vscrollbar
	  (setf value y))
	(setf (gadget-min-value vscrollbar) min-y
	      (gadget-max-value vscrollbar) max-y)
	(dispatch-repaint vscrollbar (sheet-region vscrollbar)))
      (when hscrollbar
	(with-slots (value) hscrollbar
	  (setf value x))
	(setf (gadget-min-value hscrollbar) min-x
	      (gadget-max-value hscrollbar) max-x)
	(dispatch-repaint hscrollbar (sheet-region hscrollbar))))))

(defmethod scroll-extent ((pane basic-pane) x y)
  (when (is-in-scroller-pane pane)
    (move-sheet pane (- x) (- y))
    #+NIL(update-scroll-bars pane (sheet-region pane) x y)
    #+NIL(dispatch-repaint pane (sheet-region pane))))

;;; LABEL PANE

(defclass label-pane (composite-pane permanent-medium-sheet-output-mixin)
  ((label :type string :initarg :label :accessor label-pane-label)
   (alignment :type (member :bottom :top)
              :initform :top
              :initarg :label-alignment
              :reader label-pane-label-alignment)
   )
  (:documentation ""))

(defmacro labelling ((&rest options) &body contents)
  `(make-pane 'label-pane ,@options :contents (list ,@contents)))

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
     (+ m0 (if (slot-value pane 'contents) 
               (+ a m0 m0 d)
             0))
     (+ m0 a))))

(defmethod compose-space ((pane label-pane))
  (let* ((w (text-size pane (label-pane-label pane)))
         (a (text-style-ascent (pane-text-style pane) pane))
         (d (text-style-descent (pane-text-style pane) pane))
         (m0 2)
         (h (+ a d m0 m0)))
    (cond ((slot-value pane 'contents)
           (let ((sr2 (compose-space (content-pane (first (slot-value pane 'contents))))))
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
           (let ((sr1 (make-space-requirement :width w :min-width w :max-width w
                                              :height h :min-height h :max-height h)))
             (when (slot-value pane 'contents)
               (let ((sr2 (compose-space (content-pane (first (slot-value pane 'contents))))))
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
    (declare (ignorable right top left bottom))
    (when (slot-value pane 'contents)
      (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
        (move-sheet (content-pane (first (slot-value pane 'contents)))
                    (+ x1 left) (+ y1 top))
        (allocate-space (content-pane (first (slot-value pane 'contents)))
                        (- (- x2 right) (+ x1 left))
                        (- (- y2 bottom) (+ y1 top)))))))
             

(defun curry (fun &rest args)
  #'(lambda (&rest more)
      (apply fun (append args more))))

(defmethod draw-design (medium (design rectangle))
  (multiple-value-call #'draw-rectangle* medium (rectangle-edges* design)))

(defmethod draw-design (medium (design standard-region-union))
  (map-over-region-set-regions (curry #'draw-design medium)
                               design))

(defmethod draw-design (medium (design standard-line))
  (multiple-value-call #'draw-line* medium 
                       (line-start-point* design)
                       (line-end-point* design)))

(defmethod draw-design (medium (design standard-polyline))
  (map-over-polygon-segments (curry #'draw-line* medium)
                             design))

(defmethod draw-design (medium (design (eql +nowhere+)))
  )

(defmethod repaint-sheet ((pane label-pane) region)
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
                      (:left (+ x1 m0 (if (slot-value pane 'contents) 
                                          (+ a m0 m0 d)
                                        0)))
                      (:right (- x2 m0 (if (slot-value pane 'contents) 
                                           (+ a m0 m0 d)
                                         0)
                                 tw))
                      (:center (- (/ (- x2 x1) 2) (/ tw 2))))
                    (ecase (label-pane-label-alignment pane)
                      (:top (+ y1 m0 a))
                      (:bottom (- y2 m0 d))))
          (draw-text* pane (label-pane-label pane)
                      tx ty)
          (when (slot-value pane 'contents)
            (draw-design pane 
                         (region-difference
                          (make-polyline* (list
                                           (+ x1 bleft) (+ y1 btop)
                                           (+ x1 bleft) (- y2 bbottom)
                                           (- x2 bright) (- y2 bbottom)
                                           (- x2 bright) (+ y1 btop))
                                          :closed t)
                          (make-rectangle* (- tx m0) (- ty a) (+ tx tw m0) (+ ty d)))) ))))))

;;xxx
(defmethod dispatch-repaint ((label-pane label-pane) region)
  (repaint-sheet label-pane region))

;;xxx
(defmethod handle-event ((pane label-pane) (event window-repaint-event))
  (dispatch-repaint pane (sheet-region pane)))

;;; GENERIC FUNCTIONS

(defgeneric window-clear (clim-stream-pane))
(defgeneric window-refresh (clim-stream-pane))
(defgeneric window-viewport (clim-stream-pane))
(defgeneric window-erase-viewport (clim-stream-pane))
(defgeneric window-viewport-position (clim-stream-pane))
(defgeneric* (setf window-viewport-position) (x y clim-stream-pane))


;;;
;;; 29.4 CLIM Stream Panes
;;;

(defclass clim-stream-pane (permanent-medium-sheet-output-mixin
                            standard-extended-input-stream
                            standard-extended-output-stream
                            standard-output-recording-stream
                            ;; sheet-leaf-mixin
                            basic-pane)
  ((display-time :initform nil
		 :initarg :display-time
		 :accessor pane-display-time)
   (incremental-redisplay :type (member '(t nil))
			  :initform nil
			  :initarg :incremental-redisplay 
			  :accessor pane-incremental-redisplay)
   (scroll-bars :type (member '(t :vertical :horizontal nil))
		:initform nil
		:initarg :scroll-bars
		:accessor pane-scroll-bars)
   (display-function :initform 'default-frame-top-level
		     :initarg :display-function
		     :accessor pane-display-function)
   ; Should inherit from label-pane for this one ??
   (label :type string :initform nil
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
		       :initarg :end-of-line-action
		       :reader pane-end-of-page-action)
   (output-history :initform (make-instance 'standard-tree-output-history)
		   :initarg :output-history
		   :accessor pane-output-history))
  (:documentation
   "This class implements a pane that supports the CLIM graphics,
    extended input and output, and output recording protocols."))


(defmethod handle-event ((pane clim-stream-pane) (event window-repaint-event))
  (dispatch-repaint pane (sheet-region pane)))

(defmethod compose-space ((pane clim-stream-pane))
  (make-space-requirement :width 300 :height 300))

(defmethod dispatch-repaint ((pane clim-stream-pane) region)
  (repaint-sheet pane region))

(defmethod window-clear ((pane clim-stream-pane))
  (let ((output-history (pane-output-history pane)))
    (with-bounding-rectangle* (left top right bottom) output-history
      (medium-clear-area (sheet-medium pane) left top right bottom))
    (clear-output-record output-history))
  (window-erase-viewport pane)
  (let ((cursor (stream-text-cursor pane)))
    (when cursor
      (setf (cursor-position cursor) (values 0 0))))
  (scroll-extent pane 0 0))

(defmethod window-refresh ((pane clim-stream-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
    (draw-rectangle* (sheet-medium pane) x1 y1 x2 y2 :ink +background-ink+))
  (stream-replay pane))

(defmethod window-viewport ((pane clim-stream-pane))
  (pane-viewport-region pane))

(defmethod window-erase-viewport ((pane clim-stream-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (or (pane-viewport-region pane)
                                              (sheet-region pane))
    (draw-rectangle* (sheet-medium pane) x1 y1 x2 y2 :ink +background-ink+)))

(defmethod window-viewport-position ((pane clim-stream-pane))
  (multiple-value-bind (x y) (bounding-rectangle* (pane-output-history pane))
    (values x y)))

(defmethod* (setf window-viewport-position) (x y (pane clim-stream-pane))
  (scroll-extent pane x y))

#+NIL
(defun scroll-area (pane dx dy)
  (let ((transform (sheet-transformation pane)))
    ;; Region has been "scrolled" already.
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
      (multiple-value-bind (destx desty)
	  (transform-position transform x1 y1)
	(multiple-value-bind (srcx srcy)
	    (transform-position transform (- x1 dx) (- y1 dy))
	  (format *debug-io* "dx ~S dy ~S srcx ~S srcy ~S destx ~S desty ~S~%"
		  dx dy srcx srcy destx desty)
	  (copy-area pane  srcx srcy (- x2 x1) (- y2 y1) destx desty))))))

#+NIL
(defmethod scroll-extent ((pane clim-stream-pane) x y)
  (when (is-in-scroller-pane pane)
    (let ((new-x (max x 0))
	  (new-y (max y 0))
	  (output-history (pane-output-history pane)))
      (let ((entire-region
	     (make-bounding-rectangle 0 0
				      (bounding-rectangle-max-x output-history)
				      (bounding-rectangle-max-y output-history)))
	    dx dy)
	(set-bounding-rectangle-position (sheet-region pane) new-x new-y)
	;; find out the coordinates, in the coordinates system of
	;; pane, of the upper-left corner, i.e. the one with
	;; coordinates (0,0) in the viewport
	(multiple-value-bind (x0 y0)
	    (untransform-position (sheet-transformation pane) 0 0)
	  (setq dx (- x0 new-x)
		dy (- y0 new-y))
	  ;; alter the sheet transformation to reflect the new position
	  (setf (sheet-transformation pane)
	    (compose-translation-with-transformation
	     (sheet-transformation pane) dx dy))
          (update-scroll-bars pane entire-region new-x new-y)
	  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
	    (cond
	     ((and (zerop dx)
		   (< (abs dy) (- y2 y1)))
	      (copy-area pane 0 0 (- x2 x1) (- y2 y1) 0 dy)
	      #+nil
	      (scroll-area pane 0 dy)
	      (cond
	       ((< dy 0)
		(draw-rectangle* (sheet-medium pane) x1 (+ y2 dy) x2 y2 :ink +background-ink+)
		(stream-replay pane (make-bounding-rectangle x1 (+ y2 dy) x2 y2)))
	       (t
		(draw-rectangle* (sheet-medium pane) x1 y1 x2 (+ y1 dy) :ink +background-ink+)
		(stream-replay pane (make-bounding-rectangle x1 y1 x2 (+ y1 dy)))))
	      )
	     ((and (zerop dy)
		   (< (abs dx) (- x2 x1)))
	      (copy-area pane 0 0 (- x2 x1) (- y2 y1) dx 0)
	      #+nil
	      (scroll-area pane dx 0)
	      (cond
	       ((< dx 0)
		(draw-rectangle* (sheet-medium pane) (+ x2 dx) y1 x2 y2 :ink +background-ink+)
		(stream-replay pane (make-bounding-rectangle (+ x2 dx) y1 x2 y2)))
	       (t
		(draw-rectangle* (sheet-medium pane) x1 y1 (+ x1 dx) y2 :ink +background-ink+)
		(stream-replay pane (make-bounding-rectangle x1 y1 (+ x1 dx) y2))))
	      )
	     (t
	      (draw-rectangle* (sheet-medium pane) x1 y1 x2 y2 :ink +background-ink+)
	      (stream-replay pane (sheet-region pane))))))))))

;;; INTERACTOR PANES 

(defclass interactor-pane (clim-stream-pane)
  ())

(defmethod initialize-instance :before ((interactor interactor-pane) &rest args)
  (declare (ignore args))
  (setf (pane-scroll-bars interactor) :vertical))

(defmethod initialize-instance :after ((pane interactor-pane) &rest args)
  (declare (ignore args))
#+ignore  (let ((cursor (stream-text-cursor pane)))
    (setf (cursor-visibility cursor) t)))


;;; APPLICATION PANES

(defclass application-pane (clim-stream-pane)
  ())

(defmethod initialize-instance :before ((application application-pane) &rest args)
  (declare (ignore args))
  (setf (pane-display-time application) :command-loop
	(pane-scroll-bars application) t))
	

;;; COMMAND-MENU PANE

(defclass command-menu-pane (clim-stream-pane)
  ())

(defmethod initialize-instance :before ((command-menu command-menu-pane) &rest ignore)
  (declare (ignore ignore))
  (setf (pane-display-time command-menu) :command-loop
	(pane-incremental-redisplay command-menu) t
	(pane-scroll-bars command-menu) t
	(pane-display-function command-menu) 'diplay-command-menu))


;;; TITLE PANE

(defclass title-pane (clim-stream-pane)
  ())

(defmethod initialize-instance :before ((title title-pane) &rest args)
  (declare (ignore args))
  (setf (pane-display-time title) t))


;;; POINTER DOCUMENTATION PANE

(defclass pointer-documentation-pane (clim-stream-pane)
  ())


;;; CONSTRUCTORS

(defun make-clim-stream-pane (&rest options 
				    &key (type 'clim-stream-pane)
				         (scroll-bars :vertical)
					 (border-width 1)
				    &allow-other-keys)
  (declare (ignorable scroll-bars))
  (loop for key in '(:type :scroll-bars :border-width)
	do (remf options key))
  (let ((pane (apply #'make-pane type options)))
    (when scroll-bars
      (setq pane (make-pane 'scroller-pane 
			    :scroll-bar scroll-bars
			    :contents (list (make-pane 'viewport-pane 
						       :contents (list pane))))))
    (when (and border-width (> border-width 0))
      (setq pane (make-pane 'border-pane 
			    :border-width border-width 
			    :contents (list pane))))
    pane))

(defun make-clim-interactor-pane (&rest options)
  (apply #'make-clim-stream-pane :type 'interactor-pane options))

(defun make-clim-application-pane (&rest options)
  (apply #'make-clim-stream-pane :type 'application-pane options))

;;; 29.4.5 Creating a Standalone CLIM Window

(define-application-frame a-window-stream () 
  ((the-io))
  (:panes
   (io
    (scrolling (:height 400 :width 700)
      (setf (slot-value *application-frame* 'the-io)
        (make-pane 'clim-stream-pane
                   :width 700
                   :height 2000)))))
  (:layouts
   (:default io)))

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
  (declare (ignorable left top right bottom width height
                      foreground background
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
                      input-buffer
                      scroll-bars
                      borders
                      label))
  (setf port (or port (find-port)))
  (let* ((fm (find-frame-manager :port port))
         (fr (make-application-frame 'a-window-stream
                                     :frame-manager fm)))
    (with-look-and-feel-realization (fm fr))
    fr))

(defun foofoo ()
  (open-window-stream))



(defmethod text-size ((sheet sheet) string &rest more)
  (apply #'text-size (sheet-medium sheet) string more))
(defmethod text-style-ascent (ts (sheet sheet))
  (text-style-ascent ts (sheet-medium sheet)))
(defmethod text-style-descent (ts (sheet sheet))
  (text-style-descent ts (sheet-medium sheet)))

;;;;;;

#||
(defclass list-pane (value-gadget)
  ((mode        :initarg :mode
                :initform :some-of
                :reader list-pane-mode
                :type (member :one-of :some-of))
   (items       :initarg :items
                :initform nil
                :reader list-pane-items
                :type sequence)
   (name-key    :initarg :name-key
                :initform #'princ-to-string
                :reader list-pane-name-key
                :documentation "A function to be applied to items to gain a printable representation")
   (value-key   :initarg :value-key
                :initform #'identity
                :reader list-pane-value-key
                :documentation "A function to be applied to items to gain its value 
                                for the purpose of GADGET-VALUE.")
   (test        :initarg :test
                :initform #'eql
                :reader list-pane-test
                :documentation "A function to compare two items for equality.") ))

(defclass generic-list-pane (basic-pane list-pane permanent-medium-sheet-output-mixin)
  ((item-strings :initform nil
                 :documentation "Vector of item strings.")
   (selected-items :initform nil
                   :documentation "List of indexes of selected items.")
   ))

(defmethod generic-list-pane-item-strings ((pane generic-list-pane))
  (with-slots (item-strings) pane
    (or item-strings
        (setf item-strings
          (map 'vector (lambda (item)
                         (let ((s (funcall (list-pane-name-key pane) item)))
                           (if (stringp s)
                               s
                             (princ-to-string s)))) ;defensive programming!
               (list-pane-items pane))))))

(defmethod compose-space ((pane generic-list-pane))
  (let* ((n (length (generic-list-pane-item-strings pane)))
         (w (reduce #'max (map 'vector (lambda (item-string)
                                         (text-size pane item-string))
                               (generic-list-pane-item-strings pane))
                    :initial-value 0))
         (h (* n (+ (text-style-ascent (pane-text-style pane) pane)
                    (text-style-descent (pane-text-style pane) pane)))))
    (make-space-requirement :width w :height h
                            :min-width w :min-height h
                            :max-width w :max-height h)))

#+NIL
(defmethod allocate-space ((pane generic-list-pane) width height)
  )

(defmethod handle-repaint ((pane generic-list-pane) region)
  (with-slots (selected-items) pane
    (let* ((a (text-style-ascent (pane-text-style pane) pane))
           (d (text-style-descent (pane-text-style pane) pane)))
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
        (loop
            for i from 0
            for x across (generic-list-pane-item-strings pane) do
              (let ((y (+ a (* i (+ a d)))))
                (cond ((member i selected-items)
                       (draw-rectangle* pane x1 (- y a) x2 (+ y d))
                       (draw-text* pane x 0 y :ink +background-ink+))
                      (t
                       (draw-rectangle* pane x1 (- y a) x2 (+ y d) :ink +background-ink+)
                       (draw-text* pane x 0 y)))))))))

(defmethod handle-event ((pane generic-list-pane) (event pointer-button-press-event))
  (multiple-value-bind (mx my) (values (pointer-event-x event) (pointer-event-y event))
    (let ((k (floor my (+ (text-style-ascent (pane-text-style pane) pane)
                          (text-style-descent (pane-text-style pane) pane))))
          (n (length (generic-list-pane-item-strings pane))))
      (if (member k (slot-value pane 'selected-items))
          (setf (slot-value pane 'selected-items) (delete k (slot-value pane 'selected-items)))
        (pushnew k (slot-value pane 'selected-items)))
      (dispatch-repaint pane +everywhere+)
      )))

(defmethod handle-event ((sheet immediate-repainting-mixin) (event window-repaint-event))
  (dispatch-repaint sheet (window-event-region event)))
||#