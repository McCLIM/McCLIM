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
;;; Implementation of the 29.3 Composite and Layout Panes (panes).
;;;

(in-package :clim-internals)

;;;
;;; Ambiguities and Obmissions
;;;
;;;
;;; This is a scratch pad, were we can document, what the spec doesn't
;;; tells us about CLIM. Reason: While coding, one sees were the spec
;;; is vague or wrong; later when the task to update the spec is due,
;;; things might be forgotten. --GB
;;;

;;;
;;; - Default of :equalize-width / :equalize-height is T
;;;
;;; - LAYOUT-PANE is mentioned in the spec's example, but not in the
;;;   text.
;;;
;;; - Behaviour of :align-x, :align-y is uncertain.
;;;   (Should it be specifed on the childs? on the parents?)
;;;
;;; - BORDER-PANE is not in the spec and just a different name of
;;;   OUTLINED-PANE, where is it from? --GB
;;;
;;; - RAISED-PANE, where form? --GB
;;;
;;; - In XBOX-PANE: I would like to also allow for (1 <pane>) being a
;;;   proportional content.

;;; TODO
;;;
;;; - VBOX/HBOX/VRACK/HRACK:
;;;   . should align its children
;;;     Q: Should we cope with proportional content differently?
;;;   . test units for spacing and fixed width
;;;     Q: When to resolve?
;;;   . adopt/disown/enable/disable
;;;
;;; - TABLE-PANE
;;;   . test units
;;;   . adopt/disown/enable/disable
;;;   . allow for partially filled rows/cols?
;;;
;;; - GRID-PANE
;;;   . align children
;;;   . test units
;;;   . adopt/disown/enable/disable
;;;
;;; - SPACING-PANE
;;;   . align child
;;;     Or: expand them as we did?
;;;   . adopt/disown/enable/disable
;;;
;;; - RESTRAINING-PANE
;;;   . ???
;;;
;;; - LABEL-PANE
;;;   . test units
;;;   . adopt/disown/enable/disable
;;;   . expand child? leave it?
;;;
;;; - SCROLLER-PANE
;;;   . much!
;;;
;;; - we still need to think about what should happen when children
;;;   get disabled or adopted or disowned.
;;;
;;; - adjust class names.
;;;
;;; - advertise layout-child et al
;;;
;;; - reuse single-child-composite-pane
;;;
;;; - MAKE-SPACE-REQUIREMENT right?
;;;   . default arguments in the spec are different
;;;   . DUIM's default for maxima is not +fill+ but the dimension
;;;
;;; - what are the appropriate default values for align?
;;;

;;; - for layout purposes the list of children should be considered in
;;;   reverse: The first element of children should come last.

;;;--GB 2002-02-27


;;;
;;; Utilities
;;;

;; Since, I hate to duplicate code for HBOX and VBOX, I define this
;; evil macro:

(defmacro dada ((&rest substs) &body body)
  "This is an evil macro."
  (setf substs (sort substs #'> :key (lambda (s)
                                       (length (symbol-name (first s))))))
  `(progn
     ,@(loop for k from 1 below (length (first substs))
             collect
             (labels ((subst-one (new old sym)
                        (if-let ((p (search (symbol-name old) (symbol-name sym))))
                          (let ((pack (if (eq (symbol-package sym)
                                              (find-package :keyword))
                                          (symbol-package sym)
                                          *package*)))
                            (intern (concatenate
                                     'string
                                     (subseq (symbol-name sym) 0 p)
                                     (symbol-name new)
                                     (subseq (symbol-name sym)
                                             (+ p (length (symbol-name old)))))
                                    pack))
                          sym))
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

;;;;
;;;; Composite Panes
;;;;

(defclass composite-pane (basic-pane)
  ()
  (:documentation "protocol class"))

;; When a composite-pane receives the note-space-requirements-changed
;; from one of its children, the normal behaviour is to change its
;; space-requirements. For the moment exception are restraining-pane
;; and viewport-pane. -- admich 2020-08-11
(defmethod note-space-requirements-changed ((pane composite-pane) client)
  (declare (ignore client))
  (change-space-requirements pane))

(defmethod spacing-value-to-device-units ((pane extended-output-stream) x)
  (etypecase x
    (real x)
    (cons (destructuring-bind (value type) x
            (ecase type
              (:pixel     value)
              (:point     (* value (graft-pixels-per-inch (graft pane)) 1/72))
              (:mm        (* value (graft-pixels-per-millimeter (graft pane))))
              (:character (* value (stream-character-width pane #\m)))
              (:line      (* value (stream-line-height pane))))))))

(defmethod spacing-value-to-device-units ((pane composite-pane) x)
  (if (and (consp x) (member (second x) '(:character :line)))
      (loop for sheet in (sheet-children pane)
            maximize (spacing-value-to-device-units sheet x))
      (call-next-method)))

(defmethod spacing-value-to-device-units ((pane basic-pane) x)
  (etypecase x
    (real x)
    (cons (destructuring-bind (value type) x
            (ecase type
              (:pixel     value)
              (:point     (* value (graft-pixels-per-inch (graft pane)) 1/72))
              (:mm        (* value (graft-pixels-per-millimeter (graft pane))))
              (:character 0)
              (:line      0))))))

;;; MULTIPLE-CHILD-COMPOSITE PANE

(defclass multiple-child-composite-pane (sheet-multiple-child-mixin composite-pane) ())

;;; SINGLE-CHILD-COMPOSITE PANE

(defclass single-child-composite-pane (sheet-single-child-mixin composite-pane) ())


(defmethod initialize-instance :after ((pane single-child-composite-pane)
                                       &rest args
                                       &key contents
                                       &allow-other-keys)
  (declare (ignore args))
  (when contents
    (when (cdr contents)
      (error 'sheet-supports-only-one-child :sheet pane))
    (sheet-adopt-child pane (first contents))))

(defmethod compose-space ((pane single-child-composite-pane)
                          &rest args &key width height)
  (declare (ignore width height))
  (if-let ((child (sheet-child pane)))
    (apply #'compose-space child args)
    (make-space-requirement)))

(defmethod allocate-space ((pane single-child-composite-pane) width height)
  (when-let ((child (sheet-child pane)))
    (allocate-space child width height)))

;;; TOP-LEVEL-SHEET

(defclass top-level-sheet-pane (top-level-sheet-mixin single-child-composite-pane)
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
           (cond (resize-frame
                  (layout-frame frame))
                 (t
                  (if (frame-resize-frame frame)
                      (layout-frame frame)
                      (multiple-value-bind (width height)
                          (bounding-rectangle-size pane)
                        (layout-frame frame width height)))))))))

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
       ;; negative offsets are handled by the native transformation?
       (make-translation-transformation x y)))))

(defmethod handle-event ((pane top-level-sheet-pane)
                         (event window-manager-delete-event))
  (frame-exit (pane-frame (event-sheet event))))

;;; UNMANAGED-TOP-LEVEL-SHEET PANE

(defclass unmanaged-top-level-sheet-pane (unmanaged-sheet-mixin top-level-sheet-pane)
  ()
  (:documentation "Top-level sheet without window manager intervention"))

(defmethod sheet-native-transformation ((sheet top-level-sheet-pane))
  (with-slots (native-transformation) sheet
    (unless native-transformation
      (setf native-transformation +identity-transformation+))
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

  (let* ((space-requirements (compose-space pane))
         (width (space-requirement-width space-requirements))
         (height (space-requirement-height space-requirements)))
    (resize-sheet pane width height)
    (allocate-space pane width height)))

;;; Now each child (client) of a box-layout pane is described by the
;;; following class:

(defclass box-client ()
  ((fillp
    :initarg       :fillp
    :initform      nil
    :accessor      box-client-fillp
    :documentation "Whether this child can stretch infinitely.")
   (fixed-size
    :initarg       :fixed-size
    :initform      nil
    :accessor      box-client-fixed-size
    :documentation "Possible fixed size of a child.")
   (proportion
    :initarg       :proportion
    :initform      nil
    :accessor      box-client-proportion
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
    :initform nil))
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
  ()
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
    (let (p
          (sr (if-let ((pane (box-client-pane client)))
                (compose-space pane)
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
              :minor     (space-requirement-minor sr)
              :min-minor (space-requirement-min-minor sr)
              :max-minor (space-requirement-max-minor sr)))
            (t
             sr))))

  (defgeneric xically-content-sr*** (pane client major))

  (defmethod xically-content-sr*** ((pane box-layout-mixin) client major)
    (let (p
          (sr (if-let ((pane (box-client-pane client)))
                (compose-space pane)
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
              :minor     (space-requirement-minor sr)
              :min-minor (space-requirement-min-minor sr)
              :max-minor (space-requirement-max-minor sr)))
            ((setq p (box-client-proportion client))
             (make-space-requirement
              :major     (clamp (* p major)
                                (space-requirement-min-major sr)
                                (space-requirement-max-major sr))
              :min-major (space-requirement-min-major sr)
              :max-major (space-requirement-max-major sr)
              :minor     (space-requirement-minor sr)
              :min-minor (space-requirement-min-minor sr)
              :max-minor (space-requirement-max-minor sr)))
            (t
             sr))))

  (defgeneric box-layout-mixin/xically-compose-space (pane))

  (defmethod box-layout-mixin/xically-compose-space ((pane box-layout-mixin))
    (let ((n (length (sheet-enabled-children pane))))
      (with-slots (major-spacing) pane
        (loop for client in (box-layout-mixin-clients pane)
              for sr = (xically-content-sr** pane client)
              sum (space-requirement-major sr) into major
              sum (space-requirement-min-major sr) into min-major
              sum (space-requirement-max-major sr) into max-major
              maximize (space-requirement-minor sr) into minor
              maximize (space-requirement-min-minor sr) into min-minor
              minimize (space-requirement-max-minor sr) into max-minor
              finally (return (space-requirement+*
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
    (let* ((clients (box-layout-mixin-clients box))
           (children (reverse (sheet-enabled-children box)))
           (spacing (* (1- (length children)) (slot-value box 'major-spacing)))
           (content-srs (mapcar (lambda (c) (xically-content-sr*** box c major))
                                clients))
           (allot       (mapcar #'space-requirement-major content-srs))
           (wanted      (reduce #'+ allot))
           (excess      (- major wanted spacing)))
      (when *dump-allocate-space*
        (format *trace-output* "~&;; ~S ~S ~D children~%"
                'box-layout-mixin/xically-allocate-space-aux* box (length children))
        (format *trace-output* "~&;;   major = ~D, wanted = ~D, excess = ~D, allot = ~D.~%"
                major wanted excess allot))

      (let ((qvector (mapcar (lambda (c)
                               (cond
                                 ((box-client-fillp c)
                                  (vector 1 0 0))
                                 (t
                                  (let ((sr (xically-content-sr*** box c major)))
                                    (vector 0 0 (abs (- (if (> excess 0)
                                                            (space-requirement-max-major sr)
                                                            (space-requirement-min-major sr))
                                                        (space-requirement-major sr))))))))
                             clients)))
        ;;
        (when *dump-allocate-space*
          (format *trace-output* "~&;;   old allotment = ~S.~%" allot)
          (format *trace-output* "~&;;   qvector = ~S.~%" qvector)
          (format *trace-output* "~&;;   qvector 0 = ~S.~%" (mapcar (lambda (x) (elt x 0)) qvector))
          (format *trace-output* "~&;;   qvector 1 = ~S.~%" (mapcar (lambda (x) (elt x 1)) qvector))
          (format *trace-output* "~&;;   qvector 2 = ~S.~%" (mapcar (lambda (x) (elt x 2)) qvector)))
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
                (format *trace-output* "~&;;   new excess = ~F, allotment = ~S.~%" excess allot)))))
        ;;
        (when *dump-allocate-space*
          (format *trace-output* "~&;;   excess = ~F.~%" excess)
          (format *trace-output* "~&;;   new allotment = ~S.~%" allot))

        (values allot (mapcar #'space-requirement-minor content-srs)))))

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
    (multiple-value-bind (majors minors)
        (box-layout-mixin/xically-allocate-space-aux* pane real-width real-height)
      (loop with spacing = (slot-value pane 'major-spacing)
            with x = 0
            for child in (box-layout-mixin-clients pane)
            for major in majors
            for minor in minors
            do (when-let ((pane (box-client-pane child)))
                 (layout-child pane
                               (pane-align-x (box-client-pane child))
                               (pane-align-y (box-client-pane child))
                               ((lambda (major minor) height width) x 0)
                               ((lambda (major minor) width height) x 0)
                               ((lambda (major minor) height width) width real-width)
                               ((lambda (major minor) height width) real-height height))
                 (incf x spacing))
               (incf x major))))

  (defmethod box-layout-mixin/xically-allocate-space ((pane rack-layout-mixin) real-width real-height)
    (multiple-value-bind (majors minors)
        (box-layout-mixin/xically-allocate-space-aux* pane real-width real-height)
      (loop with spacing = (slot-value pane 'major-spacing)
            with x = 0
            for child in (box-layout-mixin-clients pane)
            for major in majors
            for minor in minors
            do (when-let ((pane (box-client-pane child)))
                 (layout-child pane
                               :expand
                               :expand
                               ((lambda (major minor) height width) x 0)
                               ((lambda (major minor) width height) x 0)
                               ((lambda (major minor) height width) width real-width)
                               ((lambda (major minor) height width) real-height height))
                 (incf x spacing))
               (incf x major)))))

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
    (loop for content in contents
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

(defclass box-pane (box-layout-mixin multiple-child-composite-pane)
  ()
  (:documentation "Superclass for hbox-pane and vbox-pane that provides the
                    initialization common to both."))

(defmethod initialize-instance :after ((pane box-pane) &key contents)
  (setf (%pane-contents pane) contents))

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

    (loop for box in contents
          for client = (parse-box-content box)
          for box-pane = (box-client-pane client)
          collect client into clients
          when box-pane
            collect box-pane into children
          finally
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

(defclass table-pane (multiple-child-composite-pane)
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
    (loop for sr in srs
          sum (space-requirement-major sr) into major
          sum (space-requirement-min-major sr) into min-major
          sum (space-requirement-max-major sr) into max-major
          maximize (space-requirement-minor sr) into minor
          maximize (space-requirement-min-minor sr) into min-minor
          minimize (space-requirement-max-minor sr) into max-minor
          finally (return (make-space-requirement
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
           (qs (mapcar (lambda (sr)
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
      allot)))

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
        (make-space-requirement
         :width      (+ (space-requirement-width r) xs)
         :min-width  (+ (space-requirement-min-width r) xs)
         :max-width  (+ (space-requirement-max-width r) xs)
         :height     (+ (space-requirement-height c) ys)
         :min-height (+ (space-requirement-min-height c) ys)
         :max-height (+ (space-requirement-max-height c) ys))))))

(defmethod allocate-space ((pane table-pane) width height)
  (with-slots (array x-spacing y-spacing) pane
    ;; allot rows
    (let* ((row-count (array-dimension array 0))
           (col-count (array-dimension array 1))
           (rows (allot-space-vertically
                  (loop for i from 0 below row-count
                        collect (table-pane-row-space-requirement pane i))
                  (- height (* y-spacing (1- row-count)))))
           (cols (allot-space-horizontally
                  (loop for j from 0 below col-count
                        collect (table-pane-col-space-requirement pane j))
                  (- width (* x-spacing (1- col-count))))))
      ;; now finally layout each child
      (loop for y = 0 then (+ y h y-spacing)
            for h in rows
            for i from 0
            do (loop for x = 0 then (+ x w x-spacing)
                     for w in cols
                     for j from 0
                     for child = (aref array i j)
                     do (layout-child child
                                      (pane-align-x child)
                                      (pane-align-y child)
                                      x y w h))))))

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
          and max-width = 5000000 then (max width (min max-width (sr-min-width child)))
          and max-height = 5000000 then (max height (min max-height (sr-max-height child)))
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
                        single-child-composite-pane)
  ((border-width :initarg :thickness
                 :initform 1))
  (:documentation "Never trust a random documentation string."))

(defmacro spacing ((&rest options) &body contents)
  `(make-pane 'spacing-pane ,@options :contents (list ,@contents)))

(defun spacing-p (pane)
  (typep pane 'spacing-pane))

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

(defclass raised-pane (border-pane)
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

(defclass lowered-pane (border-pane)
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

(defclass bboard-pane (multiple-child-composite-pane) ())

(defmethod initialize-instance :after ((sheet bboard-pane) &key contents)
  (dolist (child (alexandria:ensure-list contents))
    (sheet-adopt-child sheet child)))

(defmethod compose-space ((bboard bboard-pane) &key (width 100) (height 100))
  (make-space-requirement :width width :height height))

;;; VIEWPORT

(defclass viewport-pane (single-child-composite-pane) ())

(defmethod compose-space ((pane viewport-pane) &key width height)
  (declare (ignorable width height))
  ;; I _think_ this is right, it certainly shouldn't be the
  ;; requirements of the child, apart from the max sizes. If the child
  ;; does not want to go bigger than a specific size, we should not
  ;; force it to do so.
  (if-let ((child-sr (compose-space (sheet-child pane))))
    (make-space-requirement :max-width (space-requirement-max-width child-sr)
                            :max-height (space-requirement-max-height child-sr))
    (make-space-requirement)))

(defmethod allocate-space ((pane viewport-pane) width height)
  (let* ((parent       (sheet-parent pane))
         (child        (sheet-child pane))
         (child-space  (compose-space child))
         (child-width  (space-requirement-width child-space))
         (child-height (space-requirement-height child-space)))
    ;; This must update (and perform the required repaints) the
    ;; transformation and region of the child and the scrollbars.
    ;;
    ;; Step 1: Allocate space of CHILD. This will resize but not move CHILD.
    (allocate-space child (max child-width width) (max child-height height))
    ;; Step 2: Update the scroll bars. This looks at the bounding
    ;; rectangle of CHILD which should already be updated.
    (scroller-pane/update-scroll-bars parent)
    ;; Step 3: move CHILD to the position corresponding to the updated
    ;; values of the scroll bars.
    (with-slots (hscrollbar vscrollbar) parent
      (move-sheet child
                  (if hscrollbar (- (gadget-value hscrollbar)) 0)
                  (if vscrollbar (- (gadget-value vscrollbar)) 0)))))

(defmethod note-input-focus-changed ((pane viewport-pane) state)
  (note-input-focus-changed (sheet-child pane) state))

(defmethod note-space-requirements-changed ((pane viewport-pane) child)
  (multiple-value-bind (width height) (bounding-rectangle-size pane)
    (allocate-space pane width height)))

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

(defclass scroller-pane (multiple-child-composite-pane)
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
               :initarg :scroll-bars
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

(defun align-subpixel (pos ref)
  (+ (truncate pos) (nth-value 1 (truncate ref))))

(defun scroller-pane/vertical-drag-callback (pane new-value)
  "Callback for the vertical scroll-bar of a scroller-pane."
  (with-slots (viewport hscrollbar vscrollbar) pane
    (let ((scrollee (sheet-child viewport)))
      (when (pane-viewport scrollee)
        (let ((transform (sheet-transformation scrollee)))
          (multiple-value-bind (t1 t2 t3 t4 old-x old-y)
              (get-transformation transform)
            (declare (ignore t1 t2 t3 t4))
            ;; The call to ALIGN-SUBPIXEL ensures that the subpixel
            ;; position remains the same before and after a scrollbar
            ;; drag.
            ;;
            ;; When scrolling a viewport, if the decimal portion of
            ;; any corrdinate changes, the rounding will change. This
            ;; causes rounding effects on things that are drawn. Such
            ;; effects are not a problem for static views, but when
            ;; scrolling, the effect is that graphics sightly change
            ;; appearance.
            ;;
            ;; In addition, when using optimised scrolling (the
            ;; content of the pane is copied and only the newly
            ;; exposed area is repainted), it is important that the
            ;; rounding remains consistent in order to avoid artifacts
            ;; between the area that was copied and the part that has
            ;; been newly updated. Fir this reason, optimised
            ;; scrolling only takes effect when the decimal portion of
            ;; the translation remains constant.
            ;;
            ;; Because of this, this function ensures that the
            ;; subpixel positioning (i.e. the decimal part) is
            ;; preserved after the scrollbar is moved.
            (move-sheet scrollee
                        (if hscrollbar
                            (align-subpixel (- (gadget-value hscrollbar)) old-x)
                            0)
                        (align-subpixel (- new-value) old-y))))))))

(defun scroller-pane/horizontal-drag-callback (pane new-value)
  "Callback for the horizontal scroll-bar of a scroller-pane."
  (with-slots (viewport hscrollbar vscrollbar) pane
    (let ((scrollee (sheet-child viewport)))
      (when (pane-viewport scrollee)
        (let ((transform (sheet-transformation scrollee)))
          (multiple-value-bind (t1 t2 t3 t4 old-x old-y)
              (get-transformation transform)
            (declare (ignore t1 t2 t3 t4))
            (move-sheet scrollee
                        (align-subpixel (- new-value) old-x)
                        (if vscrollbar
                            (align-subpixel (- (gadget-value vscrollbar)) old-y)
                            0))))))))

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
  (when-let ((parent (sheet-parent pane)))
    (when (typep parent 'viewport-pane)
      parent)))

;;; Default for streams that aren't even panes.

(defmethod pane-viewport-region ((pane t))
  nil)

(defmethod pane-viewport-region ((pane basic-pane))
  (when-let ((viewport (pane-viewport pane)))
    (untransform-region (sheet-delta-transformation pane viewport)
                        (sheet-region viewport))))

(defmethod pane-scroller ((pane basic-pane))
  (when-let ((viewport (pane-viewport pane)))
    (sheet-parent viewport)))

(defmethod scroll-extent ((pane basic-pane) x y)
  (when (pane-viewport pane)
    (move-sheet pane (- x) (- y))
    (when-let  ((scroller (pane-scroller pane)))
      (scroller-pane/update-scroll-bars scroller))))


;;; LABEL PANE

(defclass label-pane (single-child-composite-pane)
  ((label :type string
          :initarg :label
          :accessor clime:label-pane-label
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

(defmethod reinitialize-instance :after ((instance label-pane)
                                         &key (label nil label-supplied-p))
  (when label-supplied-p
    (setf (clime:label-pane-label instance) label)))

(defmethod (setf clime:label-pane-label) :after (new-value (pane label-pane))
  (when (if-let ((requirements (pane-space-requirement pane)))
          (progn
            (setf (pane-space-requirement pane) nil)
            (not (space-requirement-equal requirements (compose-space pane))))
          t)
    (change-space-requirements pane))
  (repaint-sheet pane (sheet-region pane)))

(defmacro labelling ((&rest options) &body contents)
  `(make-pane 'label-pane ,@options :contents (list ,@contents)))

(defun label-pane-margins (pane)
  (let* ((alignment (label-pane-label-alignment pane))
         (label (clime:label-pane-label pane))
         (text-style (pane-text-style pane))
         (line-height (text-style-ascent text-style pane))
         (m0 2)
         (2m0 (* 2 m0)))
    (multiple-value-bind (text-width text-height)
        (text-size pane label :text-style text-style)
      (let ((horizontal-inner-margin (if (sheet-child pane)
                                         (+ line-height 2m0)
                                         0)))
        (values
         ;; Margins of inner sheet region.
         horizontal-inner-margin
         (+ (if (eq alignment :top) text-height line-height) 2m0)
         horizontal-inner-margin
         (+ (if (eq alignment :bottom) text-height line-height) 2m0)
         ;; Offsets and dimensions of label text.
         m0 text-width text-height
         ;; Margin of surrounding border.
         (+ m0 (/ line-height 2)))))))

(defmethod compose-space ((pane label-pane) &key width height)
  (declare (ignore width height))
  (multiple-value-bind (right top left bottom
                        text-offset text-width text-height)
      (label-pane-margins pane)
    (let* ((padded-width (+ text-width (* 2 text-offset)))
           (padded-height (+ text-height (* 2 text-offset))))
      (if-let ((child (sheet-child pane)))
        (let ((sr2 (compose-space child)))
          (make-space-requirement
           :width      (+ left right (max padded-width (space-requirement-width sr2)))
           :min-width  (+ left right (max padded-width (space-requirement-min-width sr2)))
           :max-width  (+ left right (max padded-width (space-requirement-max-width sr2)))
           :height     (+ top bottom (space-requirement-height sr2))
           :min-height (+ top bottom (space-requirement-min-height sr2))
           :max-height (+ top bottom (space-requirement-max-height sr2))))
        (make-space-requirement :width padded-width
                                :min-width padded-width
                                :height padded-height
                                :min-height padded-height
                                :max-height padded-height)))))

(defmethod allocate-space ((pane label-pane) width height)
  (when-let ((child (sheet-child pane)))
    (multiple-value-bind (left top right bottom) (label-pane-margins pane)
      (move-sheet child left top)
      (allocate-space child (- (- width right) left) (- (- height bottom) top)))))

(defmethod handle-repaint ((pane label-pane) region)
  (declare (ignore region))
  (let* ((region (sheet-region pane))
         (align-x (pane-align-x pane))
         (label (clime:label-pane-label pane)))
    (with-bounding-rectangle* (x1 y1 x2 y2) region
      (multiple-value-bind (ileft itop iright ibottom
                            text-offset text-width text-height
                            border-margin)
          (label-pane-margins pane)
        (declare (ignore itop ibottom))
        (multiple-value-bind (text-pivot-x text-pivot-y dx)
            (values (ecase align-x
                      (:left (+ x1 ileft text-offset))
                      (:right (- x2 iright text-offset))
                      (:center (/ (- x2 x1) 2)))
                    (ecase (label-pane-label-alignment pane)
                      (:top    (+ y1 text-offset))
                      (:bottom (- y2 text-offset text-height)))
                    (ecase align-x
                      (:left 0)
                      (:right (- text-width))
                      (:center (- (/ text-width 2)))))
          ;; Draw label.
          (draw-rectangle* pane x1 text-pivot-y x2 (+ text-pivot-y text-height)
                           :ink (pane-background pane))
          (draw-text* pane label text-pivot-x text-pivot-y
                      :align-x align-x :align-y :top)
          ;; Draw border around child without drawing over the label text.
          (when (sheet-child pane)
            (let* ((text-x (+ text-pivot-x dx))
                   (text-region (make-rectangle* text-x
                                                 text-pivot-y
                                                 (+ text-x text-width)
                                                 (+ text-pivot-y text-height))))
              (with-drawing-options
                  (pane :clipping-region (region-difference region text-region))
                (draw-bordered-rectangle* pane
                                          (+ x1 border-margin) (+ y1 border-margin)
                                          (- x2 border-margin) (- y2 border-margin)
                                          :style :groove)))))))))

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
                       (max sx0 (min (- sx1 viewport-width)
                                     (+ vx0 (* delta horizontal))))
                       (max sy0 (min (- sy1 viewport-height)
                                     (+ vy0 (* delta vertical)))))))))

(defmethod handle-event ((sheet mouse-wheel-scroll-mixin)
                         (event pointer-scroll-event))
  (if (zerop (event-modifier-state event))
      (multiple-value-bind (viewport sheet*)
          (find-viewport-for-scroll sheet)
        (when viewport
          (scroll-sheet sheet*
                        (pointer-event-delta-x event)
                        (pointer-event-delta-y event))))
      (call-next-method)))
