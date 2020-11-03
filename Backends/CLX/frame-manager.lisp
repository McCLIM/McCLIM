;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2000,2001 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2004 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2014 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2016-2018 Elias Mårtenson <lokedhs@gmail.com>
;;;  (c) copyright 2016-2019 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The frame manager and ad-hoc sheet class generation logic of the
;;; CLX backend.

(in-package #:clim-clx)

;;; CLX-FRAME-MANAGER class

(defclass clx-frame-manager (frame-manager)
  ((mirroring :initarg :mirroring
              :initform :full
              :reader mirroring)
   (class-gensym :initarg :class-gensym
                 :initform (gensym "CLX-")
                 :reader class-gensym)))

;;; We use &ALLOW-OTHER-KEYS since the INITIALIZE-INSTANCE for
;;; CLX-PORT passes various initargs that CLX-FRAME-MANAGER doesn't
;;; necessarily accept.
(defmethod initialize-instance :after ((instance clx-frame-manager)
                                       &key &allow-other-keys))

;; Abstract pane lookup logic

(defmethod find-concrete-pane-class ((fm clx-frame-manager)
                                     pane-type &optional errorp)
  ;; This backend doesn't have any specialized pane implementations
  ;; but depending on circumstances it may add optional mirroring to
  ;; the class by defining an ad-hoc subclass. Such automatically
  ;; defined concrete classes use a name that is interned in the
  ;; backend package and derived from the original class name by
  ;; including a gensym prefix, the original symbol package and the
  ;; original symbol name.
  (declare (ignore errorp))
  (maybe-add-mirroring-superclasses
   (call-next-method) (mirroring fm)
   (symbol-name (class-gensym fm)) (find-package '#:clim-clx)
   (lambda (concrete-pane-class)
     `(,(find-class 'mirrored-sheet-mixin)
       ,@(unless (subtypep concrete-pane-class 'sheet-with-medium-mixin)
           `(,(find-class 'permanent-medium-sheet-output-mixin)))
       ,concrete-pane-class))))

;;; Default mirroring predicate
(defun add-mirroring-superclasses-p (class mirroring)
  (cond ((functionp mirroring)
         (funcall mirroring class))
        ((subtypep class 'mirrored-sheet-mixin)
         nil)
        ((and (eq mirroring :single)
              (subtypep class 'top-level-sheet-pane))
         t)
        ((and (eq mirroring :full)
              (subtypep class 'basic-pane))
         t)
        ((and (eq mirroring :random) ; for testing
              (or (subtypep class 'top-level-sheet-pane)
                  (zerop (random 2)))))))

;;; This is an example of how MAKE-PANE-1 might create specialized
;;; instances of the generic pane types based upon the type of the
;;; frame manager. However, in the CLX case, we don't expect there to
;;; be any CLX specific panes. CLX uses the default generic panes
;;; instead.
(defun maybe-add-mirroring-superclasses
    (concrete-pane-class mirroring
     class-name-prefix class-name-package compute-superclasses)
  (flet ((make-class-name (concrete-class-name)
           (let ((*package* class-name-package))
             (alexandria:symbolicate
              class-name-prefix "-"
              (if-let ((package (symbol-package concrete-class-name)))
                (package-name package)
                "UNINTERNED")
              ":" (symbol-name concrete-class-name))))
         (define-class (metaclass name concrete-class)
           (let* ((superclasses (funcall compute-superclasses concrete-class))
                  (class (make-instance metaclass
                                        :name name
                                        :direct-superclasses superclasses)))
             (setf (find-class name) class))))
    (if (add-mirroring-superclasses-p concrete-pane-class mirroring)
        (multiple-value-bind (concrete-class concrete-class-name)
            (if (typep concrete-pane-class 'class)
                (values concrete-pane-class (class-name concrete-pane-class))
                (values (find-class concrete-pane-class) concrete-pane-class))
          (multiple-value-bind (class-symbol foundp)
              (make-class-name concrete-class-name)
            (if foundp
                (find-class class-symbol)
                (define-class (class-of concrete-class)
                              class-symbol
                              concrete-class))))
        concrete-pane-class)))

(defmethod adopt-frame :before ((fm clx-frame-manager) (frame menu-frame))
  ;; Temporary kludge.
  (when (eq (slot-value frame 'climi::top) nil)
    (multiple-value-bind (x y)
        (xlib:query-pointer (clx-port-window (port fm)))
      (incf x 10)
      (setf (slot-value frame 'climi::left) x
            (slot-value frame 'climi::top) y))))

(defmethod adopt-frame :after ((fm clx-frame-manager) (frame menu-frame))
  (when (sheet-enabled-p (slot-value frame 'top-level-sheet))
    (xlib:map-window (sheet-direct-xmirror (slot-value frame 'top-level-sheet)))))

(defgeneric tell-window-manager-about-space-requirements (pane))

(defmethod adopt-frame :after ((fm clx-frame-manager) (frame application-frame))
  (let ((sheet (slot-value frame 'top-level-sheet)))
    (let* ((top-level-sheet (frame-top-level-sheet frame))
           (mirror (sheet-direct-xmirror top-level-sheet)))
      (case (clim-extensions:find-frame-type frame)
        (:override-redirect (setf (xlib:window-override-redirect mirror) :on))
        (:dialog (xlib:change-property mirror
                                       :_NET_WM_WINDOW_TYPE
                                       (list (xlib:intern-atom (xlib:window-display mirror) :_NET_WM_WINDOW_TYPE_DIALOG))
                                       :atom 32)))
      (multiple-value-bind (w h x y) (climi::frame-geometry* frame)
        (declare (ignore w h))
        (when (and x y)
          (setf (xlib:drawable-x mirror) x
                (xlib:drawable-y mirror) y))
        (tell-window-manager-about-space-requirements top-level-sheet))
      ;; :structure-notify events were not yet turned on, turn them
      ;; on now, so that we get informed about the windows position
      ;; (and possibly size), when the window gets maped.
      (setf (xlib:window-event-mask mirror)
            (logior (xlib:window-event-mask mirror)
                    (xlib:make-event-mask :structure-notify)))
      ;; Care for calling-frame, be careful not to trip on missing bits
      (let* ((calling-frame (frame-calling-frame frame))
             (tls (and calling-frame (frame-top-level-sheet calling-frame)))
             (calling-mirror (and tls (sheet-xmirror tls))))
        (when calling-mirror
          (setf (xlib:transient-for mirror)
                calling-mirror)))
      ;;
      (when (sheet-enabled-p sheet)
        (xlib:map-window mirror)))))

(defmethod tell-window-manager-about-space-requirements ((pane top-level-sheet-pane))
  (multiple-value-bind (w h x y) (climi::frame-geometry* (pane-frame pane))
    (declare (ignore w h))
    (let ((q (compose-space pane)))
      (let ((mirror (sheet-direct-xmirror pane)))
        (setf (xlib:wm-normal-hints mirror)
              (xlib:make-wm-size-hints
               :user-specified-position-p (and x y)
               :x x :y y
               :width  (round (space-requirement-width q))
               :height (round (space-requirement-height q))
               :max-width (min 65535 (round (space-requirement-max-width q)))
               :max-height (min 65535 (round (space-requirement-max-height q)))
               :min-width (round (space-requirement-min-width q))
               :min-height (round (space-requirement-min-height q))))))))

(defmethod tell-window-manager-about-space-requirements ((pane t))
  ;; hmm
  nil)

(defmethod note-space-requirements-changed :after ((graft clx-graft) pane)
  (tell-window-manager-about-space-requirements pane))

#+nil
(defmethod (setf clim:sheet-transformation) :around (transformation (sheet clx-pane-mixin))
  (log:info "transforming clx sheet: ~s" sheet)
  (unless (transformation-equal transformation (sheet-transformation sheet))
    (let ((old-transformation (sheet-transformation sheet)))
      (let ((climi::*inhibit-dispatch-repaint* nil))
        (call-next-method))
      #+nil
      (when (sheet-viewable-p sheet)
        (let* ((sheet-region (sheet-region sheet))
               (new-region (transform-region (sheet-transformation sheet) sheet-region))
               (old-region (transform-region old-transformation sheet-region)))
          (log:info "OLD: ~s    NEW: ~s" old-region new-region)
          #+nil
          (dispatch-repaint (sheet-parent sheet)
                            (region-union new-region old-region)))))))
