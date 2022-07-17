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
;;;  (c) copyright 2021 by Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of the 29.4 CLIM Stream Panes.
;;;

(in-package #:clim-internals)

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

(defclass clim-stream-pane (text-selection-mixin
                            updating-output-stream-mixin
                            pane-display-mixin
                            #-clim-mp standard-repainting-mixin
                            standard-output-recording-stream
                            standard-extended-input-stream
                            standard-extended-output-stream
                            ;; sheet-leaf-mixin
                            sheet-multiple-child-mixin   ; needed for GADGET-OUTPUT-RECORD
                            basic-pane)
  ((redisplay-needed :initarg :display-time)
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
   ;; size required by the stream
   (stream-width :initform 100 :accessor stream-width)
   (stream-height :initform 100 :accessor stream-height))
  (:default-initargs :display-time t)
  (:documentation
   "This class implements a pane that supports the CLIM graphics,
    extended input and output, and output recording protocols."))

(defmethod redisplay-frame-pane
    ((frame application-frame) (pane updating-output-stream-mixin) &key force-p)
  (setf (id-counter pane) 0)
  (let ((incremental-redisplay (pane-incremental-redisplay pane)))
    (cond ((not incremental-redisplay)
           (call-next-method))
          ((or (null (updating-record pane))
               force-p)
           (setf (updating-record pane)
                 (updating-output (pane :unique-id 'top-level)
                   (call-next-method frame pane :force-p force-p))))
          ;; Implements the extension to the :incremental-redisplay
          ;; pane argument found in the Franz User Guide.
          (t (let ((record (updating-record pane)))
               (if (consp incremental-redisplay)
                   (apply #'redisplay record pane incremental-redisplay)
                   (redisplay record pane))) ))))

(defmethod handle-event ((sheet clim-stream-pane)
                         (event window-manager-focus-event))
  (setf (port-keyboard-input-focus (port sheet)) sheet))

;;; This method is defined to prevent the sheet scrolling defined for the
;;; mouse-wheel-scroll-mixin when the event activates a command. In other
;;; words, we scroll the clim-stream-pane only when scrolling does not match
;;; the current input context. -- jd 2020-08-29
(defmethod handle-event ((sheet clim-stream-pane) (event pointer-scroll-event))
  (unless (find-innermost-applicable-presentation
           *input-context*
           sheet
           (pointer-event-x event)
           (pointer-event-y event)
           :frame (pane-frame sheet)
           :event event)
    (call-next-method)))

(defmethod interactive-stream-p ((stream clim-stream-pane))
  t)

(defmethod redisplay-frame-pane :after ((frame application-frame)
                                        (pane clim-stream-pane)
                                        &key force-p)
  (declare (ignore frame force-p))
  (change-space-requirements pane))

(defun invoke-display-function (frame pane)
  (let ((display-function (pane-display-function pane)))
    (cond ((consp display-function)
           (apply (car display-function)
                  frame pane (cdr display-function)))
          (display-function
           (funcall display-function frame pane))
          (t nil))
    (finish-output pane)))

(defun change-stream-space-requirements (pane &key width height)
  (check-type pane clim-stream-pane)
  (when width
    (setf (stream-width pane) width))
  (when height
    (setf (stream-height pane) height))
  (change-space-requirements pane))

(defmethod compose-space ((pane clim-stream-pane) &key (width 100) (height 100))
  (with-bounding-rectangle* (min-x min-y max-x max-y)
      (stream-output-history pane)
    (let* ((w (max max-x (- max-x min-x)))
           (h (max max-y (- max-y min-y)))
           (width (max w width (stream-width pane)))
           (height (max h height (stream-height pane))))
      (make-space-requirement
       :min-width (clamp w 0 width)
       :width width
       :max-width +fill+
       :min-height (clamp h 0 height)
       :height height
       :max-height +fill+))))

(defmethod allocate-space ((pane clim-stream-pane) width height)
  (multiple-value-bind (w h)
      (untransform-distance (sheet-native-transformation pane) width height)
    (with-bounding-rectangle* (min-x min-y) (stream-output-history pane)
      (let* ((x0 (clamp min-x (- w) 0))
             (y0 (clamp min-y (- h) 0)))
        (setf (sheet-region pane)
              (make-rectangle* x0 y0 (+ x0 w) (+ y0 h)))))))

(defmethod window-clear ((pane clim-stream-pane))
  (stream-close-text-output-record pane)
  (clear-output-record (stream-output-history pane))
  (window-erase-viewport pane)
  (when-let ((cursor (stream-text-cursor pane)))
    (setf (cursor-position cursor)
          (stream-cursor-initial-position pane)))
  (setf (stream-width pane) 0)
  (setf (stream-height pane) 0)
  (scroll-extent pane 0 0)
  (change-space-requirements pane))

(defmethod window-refresh ((pane clim-stream-pane))
  (window-erase-viewport pane)
  (stream-replay pane))

(defun clim-stream-pane-default-display-function (frame pane)
  (declare (ignore frame))
  (stream-replay pane))

(defmethod window-viewport ((pane clim-stream-pane))
  (or (pane-viewport-region pane)
      (sheet-region pane)))

(defmethod window-erase-viewport ((pane clim-stream-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (window-viewport pane)
    (medium-clear-area (sheet-medium pane) x1 y1 x2 y2)))

(defmethod window-viewport-position ((pane clim-stream-pane))
  (if (pane-scroller pane)
      (bounding-rectangle-position (window-viewport pane))
      (values 0 0)))

(defmethod* (setf window-viewport-position) (x y (pane clim-stream-pane))
  (scroll-extent pane x y)
  (values x y))

;;; output any buffered stuff before input
(defmethod stream-read-gesture :before ((stream clim-stream-pane)
                                        &key timeout peek-p
                                          input-wait-test
                                          input-wait-handler
                                          pointer-button-press-handler)
  (declare (ignore timeout peek-p input-wait-test input-wait-handler
                   pointer-button-press-handler))
  (force-output stream))

(defmethod redisplay-frame-pane ((frame application-frame)
                                 (pane symbol)
                                 &key force-p)
  (when-let ((actual-pane (get-frame-pane frame pane)))
    (redisplay-frame-pane frame actual-pane :force-p force-p)))

(define-presentation-method presentation-type-history-for-stream
    ((type t) (stream clim-stream-pane))
  (funcall-presentation-generic-function presentation-type-history type))

(defmethod %note-stream-end-of-page ((stream clim-stream-pane) action new-height)
  (declare (ignore action))
  (when (stream-drawing-p stream)
    (change-stream-space-requirements stream :height new-height)
    (unless (eq :allow (stream-end-of-page-action stream))
      (when-let ((viewport (pane-viewport stream)))
        (let ((child (sheet-child viewport)))
          (scroll-extent child
                         0
                         (max 0 (- (bounding-rectangle-height child)
                                   (bounding-rectangle-height viewport)))))))))

;;; INTERACTOR PANES

(defclass interactor-pane (clim-stream-pane)
  ()
  (:default-initargs :end-of-line-action :scroll
                     :incremental-redisplay t))

;;; KLUDGE: this is a hack to get keyboard focus (click-to-focus)
;;; roughly working for interactor panes.  It's a hack somewhat
;;; analogous to the mouse-wheel / select-and-paste handling in
;;; DISPATCH-EVENT, just in a slightly different place.
(defmethod frame-input-context-button-press-handler :before
    ((frame application-frame) (stream interactor-pane) button-press-event)
  (declare (ignore button-press-event))
  (let ((previous (stream-set-input-focus stream)))
    (when (and previous (typep previous 'gadget))
      (let ((client (gadget-client previous))
            (id (gadget-id previous)))
        (disarmed-callback previous client id)))))

;;; APPLICATION PANES

(defclass application-pane (clim-stream-pane)
  ()
  (:default-initargs :display-time :command-loop))

;;; COMMAND-MENU PANE

(defclass command-menu-pane (clim-stream-pane)
  ()
  (:default-initargs :display-time :command-loop
                     :incremental-redisplay t
                     :display-function 'display-command-menu))

;;; TITLE PANE

(defclass title-pane (clim-stream-pane)
  ((title :initarg :title-string
          :initarg :display-string
          :accessor title-string))
  (:default-initargs :display-time t
                     :title-string "Default Title"
                     :text-style (make-text-style :serif :bold :very-large)
                     :display-function 'display-title))

(defmethod display-title (frame (pane title-pane))
  (declare (ignore frame))
  (let* ((title-string (title-string pane))
         (a (text-style-ascent (pane-text-style pane) pane))
         (tw (text-size pane title-string)))
    (with-bounding-rectangle* (x1 y1 x2 nil :center-x cx) (sheet-region pane)
      (multiple-value-bind (tx ty)
          (values (- cx (/ tw 2))
                  (+ y1 2 a))
        (draw-text* pane title-string tx ty)))))

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
  (declare (ignore type args))
  (window-clear stream)
  (when (background-message stream)
    (setf (background-message stream) nil)
    (redisplay-frame-pane (pane-frame stream) stream)))

(defmethod stream-accept :around ((pane pointer-documentation-pane) type &rest args)
  (declare (ignore type args))
  (unwind-protect (loop
                    (handler-case
                        (with-input-focus (pane)
                          (return (call-next-method)))
                      (parse-error () nil)))
    (window-clear pane)))


;;; Constructors

(defconstant +stream-pane-wrapper-initargs+
  '(:label :label-alignment :scroll-bar :scroll-bars :borders))

(defconstant +space-requirement-initargs+
  '(:width :min-width :max-width :height :min-height :max-height))

(defun separate-stream-pane-initargs (initargs)
  ;; If :scroll-bars isn't a cons the user space requirement options belong to
  ;; the outermost container of the stream (scroller-pane, label-pane or
  ;; outline-pane). If :scroll-bars is a cons the user space requirement options
  ;; belong to the clim stream and it is possible to set the space requirement
  ;; of the scroller using the cdr of :scroll-bars as: :SCROLL-BARS '(:VERTICAL
  ;; :WIDTH 300) -- admich 2020-10-13
  (loop with any-wrapper-p = nil
        with complex-scroll-bars-p = nil
        for (key value) on initargs by #'cddr
        if (and (member key +space-requirement-initargs+ :test #'eq)
                (not (eq value :compute)))
          nconc (list key value) into space-options
        else if (member key +stream-pane-wrapper-initargs+)
               nconc (list key value) into wrapper-options
               and do (case key
                        (:borders
                         (when value (setf any-wrapper-p t)))
                        (:label
                         (when value (setf any-wrapper-p t)))
                        (:scroll-bars
                         (when value (setf any-wrapper-p t))
                         (when (consp value) (setf complex-scroll-bars-p t))))
        else
          nconc (list key value) into stream-options
        finally
           (return
             (if (or (not any-wrapper-p) complex-scroll-bars-p)
                 (values (append stream-options space-options) wrapper-options '())
                 (values stream-options wrapper-options space-options)))))

(defun make-unwrapped-stream-pane
    (type &rest initargs
          &key (display-after-commands nil display-after-commands-p)
          &allow-other-keys)
  (when display-after-commands-p
    (check-type display-after-commands (member nil t :no-clear))
    (when (member :display-time initargs)
      (error "MAKE-CLIM-STREAM-PANE can not be called with both ~
              :DISPLAY-AFTER-COMMANDS and :DISPLAY-TIME keywords")))
  (apply #'make-pane type (append (alexandria:remove-from-plist
                                   initargs :display-after-commands)
                                  (when display-after-commands-p
                                    (list :display-time
                                          (if (eq display-after-commands t)
                                              :command-loop
                                              display-after-commands))))))

(defun wrap-stream-pane (stream-pane user-space-requirements
                         &key label
                              (label-alignment :top)
                              (scroll-bar :vertical)
                              (scroll-bars scroll-bar)
                              (borders t))
  (let* ((pane stream-pane)
         (stream pane))
    (when scroll-bars
      (setf pane (make-pane 'viewport-pane :contents (list pane)))
      (setf pane (apply #'make-pane 'scroller-pane
                        :contents (list pane)
                        (append
                         ;; From the Franz manual if :scroll-bars is a cons the
                         ;; car is treated as the non-cons argument and the cdr
                         ;; is a list of keyword argument pairs to be used as
                         ;; options of the scroller-pane.
                         (if (consp scroll-bars)
                             `(:scroll-bar ,@scroll-bars)
                             `(:scroll-bar ,scroll-bars))
                         (when (and user-space-requirements
                                    (not (or label borders)))
                           user-space-requirements)))))
    (when label
      (setf pane (apply #'make-pane 'label-pane
                        :label label
                        :label-alignment label-alignment
                        :contents (list pane)
                        (when (and user-space-requirements (not borders))
                          user-space-requirements))))
    (when borders
      (setf pane (apply #'make-pane 'outlined-pane
                        :thickness (if (not (numberp borders))
                                       1
                                       borders)
                        :contents (list pane)
                        user-space-requirements)))
    (values pane stream)))

(defun make-clim-stream-pane (&rest options &key (type 'clim-stream-pane)
                                                 (label nil)
                                                 (label-alignment :top)
                                                 (scroll-bar :vertical)
                                                 (scroll-bars scroll-bar)
                                                 (borders t)
                              &allow-other-keys)
  (declare (ignore label label-alignment))
  (multiple-value-bind (stream-options wrapper-options wrapper-space-options)
      (separate-stream-pane-initargs
       (list* :scroll-bar scroll-bar :scroll-bars scroll-bars :borders borders
              (alexandria:remove-from-plist
               options :type :scroll-bar :scroll-bars :borders)))
    (let ((stream (apply #'make-unwrapped-stream-pane type stream-options)))
      (apply #'wrap-stream-pane stream wrapper-space-options wrapper-options))))

(macrolet
    ((define (name type default-scroll-bar)
       `(defun ,name (&rest options &key (scroll-bar  nil scroll-bar-p)
                                         (scroll-bars nil scroll-bars-p)
                      &allow-other-keys)
          (declare (ignore scroll-bar scroll-bars))
          (apply #'make-clim-stream-pane :type ',type
                 (if (or scroll-bar-p scroll-bars-p)
                     options
                     (list* :scroll-bars ,default-scroll-bar options))))))
  (define make-clim-interactor-pane interactor-pane :vertical)
  (define make-clim-application-pane application-pane t)
  (define make-clim-pointer-documentation-pane pointer-documentation-pane nil)
  (define make-clim-command-menu-pane command-menu-pane t))
