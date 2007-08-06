;;; -*- Mode: Lisp; Package: DREI -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

;;; Implementation of most of the CLIM-facing parts of Drei, including
;;; the pane and gadget itself as well as the command tables. The
;;; solely input-editor oriented stuff is in input-editor.lisp.

(in-package :drei)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Drei gadget and pane.
;;;
;;; An application can use Drei in two different ways - by using
;;; `drei-pane' directly, and controlling the command loop itself
;;; (this is what Climacs does), which offers complete control, but
;;; may end up being crummy if the application is not primarily a text
;;; editor, or it can opt to use the Drei gadget by using the keyword
;;; symbol `:drei' as the type argument to `make-pane'. This will
;;; create a Drei gadget that acts independently of the application
;;; command loop (through event handlers), in effect, it should be a
;;; drop-in replacement for the standard CLIM `:text-editor' gadget.

(defvar *background-color* +white+)
(defvar *foreground-color* +black+)
(defvar *show-mark* nil
  "If true, show a visual representation of the mark.")

;;; Cursors.

;;; NOTE: Despite the name, this does not have anything to do with
;;; CLIM cursors, though perhaps this facility should be built on top
;;; of what CLIM already provides. That seemed a bit hairy, though.
(defclass drei-cursor (standard-sequence-output-record)
  ((%drei-instance :reader drei-instance
                   :initarg :drei-instance
                   :initform (error "A Drei cursor must be associated with a Drei instance"))
   (%mark :reader mark
          :initarg :mark
          :initform (error "A Drei cursor must be associated with a mark."))
   (%active :accessor active
            :initarg :active
            :initform t
            :type boolean
            :documentation "Whether the cursor is active or
not. An active cursor is drawn using the active ink, and an
inactive is drawn using the inactive ink. Typically, a cursor
will be active when the associated Drei instance has focus.")
   (%enabled :accessor enabled
             :initarg :enabled
             :initform t
             :type boolean
             :documentation "When a cursor is enabled, it will be
drawn when its associated Drei instance is drawn. When it is not
enabled, it will simply be ignored during redisplay.")
   (%active-ink :accessor active-ink
                :initarg :active-ink
                :initform +red+
                :type color
                :documentation "The ink used to draw the cursor
when it is active.")
   (%inactive-ink :accessor inactive-ink
                  :initarg :inactive-ink
                  :initform +blue+
                  :type color
                  :documentation "The ink used to draw the cursor
when it is inactive."))
  (:documentation "A visual representation of a given mark in a
Drei buffer. The most important role for instances of subclasses
of this class is to visually represent the position of point."))

(defgeneric ink (cursor)
  (:documentation "Return the ink object that should be used for
  displaying the given cursor."))

(defmethod ink ((cursor drei-cursor))
  (if (active cursor)
      (active-ink cursor)
      (inactive-ink cursor)))

(defmethod (setf enabled) ((new-value null) (cursor drei-cursor))
  (erase-output-record cursor (editor-pane (drei-instance cursor)) nil))

(defclass point-cursor (drei-cursor)
  ()
  (:default-initargs
   :mark nil
    :active t)
  (:documentation "A class that should be used for the visual
representation of the point of a Drei instance."))

(defmethod mark ((cursor point-cursor))
  (point (drei-instance cursor)))

(defclass mark-cursor (drei-cursor)
  ()
  (:default-initargs
   :active-ink +dark-green+
    :inactive-ink +dark-green+
    :mark nil
    :active t)
  (:documentation "A class that should be used for the visual
representation of the mark of a Drei instance."))

(defmethod mark ((cursor mark-cursor))
  (mark (drei-instance cursor)))

(defmethod enabled ((cursor mark-cursor))
  *show-mark*)

(defgeneric visible (cursor drei)
  (:documentation "Is `cursor', associated with `drei', visible?
If this function returns true, it is assumed that it is safe to
display `cursor' to the editor stream. If just one of the
applicable methods returns false, the entire function returns
false.")
  (:method-combination and)
  (:method and (cursor drei)
    (enabled cursor)))

;;; Drei instances.

(defclass drei-pane (drei application-pane)
  ()
  (:default-initargs
   :incremental-redisplay t
    :end-of-line-action :scroll
    :background *background-color*
    :foreground *foreground-color*
    :display-function 'display-drei-pane
    :default-view +drei-textual-view+
    :width 900
    :active nil)
  (:documentation "An actual, instantiable Drei pane that
permits (and requires) the host application to control the
command loop completely."))

(defmethod display-drei ((drei drei-pane))
  (redisplay-frame-pane (pane-frame drei) drei))

(defmethod editor-pane ((drei drei-pane))
  ;; The whole point of the `drei-pane' class is that it's its own
  ;; display surface.
  drei)

(defmethod visible and (cursor (drei drei-pane))
  ;; We should only redisplay when the cursor is on display, or
  ;; `offset-to-screen-position' will return a non-number.
  (<= (offset (top drei))
      (offset (mark cursor))
      (offset (bot drei))))

(defmethod tab-width ((pane drei-pane))
  (tab-width (stream-default-view pane)))

(defmethod space-width ((pane drei-pane))
  (space-width (stream-default-view pane)))

(defmethod note-sheet-grafted :around ((pane drei-pane))
  (call-next-method)
  (setf (stream-default-view pane) (view pane))
  (with-slots (space-width tab-width) (stream-default-view pane)
    (with-sheet-medium (medium pane)
      (setf (medium-text-style medium) (pane-text-style pane))
      (let ((style (medium-text-style medium)))
        (setf space-width (text-size medium " " :text-style style)
              tab-width (* 8 space-width))))))

;;; The fun is that in the gadget version of Drei, we do not control
;;; the application command loop, and in fact, need to operate
;;; completely independently of it - we can only act when the our port
;;; deigns to bestow an event upon the gadget. So, we basically have
;;; to manually take care of reading gestures (asynchronously),
;;; redisplaying, updating the syntax and all the other fun
;;; details. On top of this, we have to account for the fact that some
;;; other part of the application might catch the users fancy, and
;;; since we do not (and can not) control the command loop, we can not
;;; prevent the user from "leaving" the gadget at inconvenient times
;;; (such as in the middle of entering a complex set of gestures, or
;;; answering questions asked by a command). So, we keep some state
;;; information in the `drei-gadget-pane' object and use it to cobble
;;; together our own poor man's version of an ESA command loop. Syntax
;;; updating is done after a command has been executed, and only then
;;; (or by commands at their own discretion).
(defclass drei-gadget-pane (drei-pane value-gadget action-gadget
                                      asynchronous-command-processor)
  ((%currently-processing :initform nil
                          :accessor currently-processing-p)
   (%previous-focus :accessor previous-focus :initform nil
                    :documentation "The pane that previously had
keyboard focus"))
  (:default-initargs :command-executor 'execute-drei-command)
  (:documentation "An actual, instantiable Drei gadget with
 event-based command processing."))

(defmethod initialize-instance :after ((drei drei-gadget-pane) &rest args)
  (declare (ignore args))
  ;; Heh, it seems that the :ACTIVE initarg steps over McCLIM's toes
  ;; and affects whether the gadget is active or not (which is
  ;; different from whether the Drei is active). It must be active by
  ;; default!
  (activate-gadget drei))

(defmethod gadget-value ((gadget drei-gadget-pane))
  ;; This is supposed to be a string, but a Drei buffer can contain
  ;; literal objects. We return a string if we can, an array
  ;; otherwise. This is a bit slow, as we cons up the array and then
  ;; probably a new one for the string, most of the time.
  (let ((contents (buffer-sequence (buffer gadget)
                                   0 (size (buffer gadget)))))
    (if (every #'characterp contents)
        (coerce contents 'string)
        contents)))

(defmethod (setf gadget-value) (new-value (gadget drei-gadget-pane)
                                &key (invoke-callback t))
  ;; I think we're supposed to permit this, even if the buffer is
  ;; non-editable.
  (letf (((read-only-p (buffer gadget)) nil))
    (performing-drei-operations (gadget :with-undo nil :redisplay nil)
      (delete-buffer-range (buffer gadget) 0 (size (buffer gadget)))
      (insert-buffer-sequence (buffer gadget) 0 new-value)))
  (when invoke-callback
    (value-changed-callback gadget
                            (gadget-client gadget)
                            (gadget-id gadget)
                            new-value)))

(defmethod armed-callback :after ((gadget drei-gadget-pane) client id)
  (declare (ignore client id))
  (setf (active gadget) t)
  (display-drei gadget))

(defmethod disarmed-callback :after ((gadget drei-gadget-pane) client id)
  (declare (ignore client id))
  (setf (active gadget) nil)
  (display-drei gadget))

(defgeneric handle-gesture (drei gesture)
  (:documentation "This generic function is called whenever a
Drei gadget variant has determined that a keyboard event
corresponds to a useful gesture that should be handled. A useful
gesture is, for example, one that is not simply a click on a
modifier key. When this function is called, the Drei special
variables (`*current-window*', `*current-buffer*', etc) are
properly bound."))

(defmethod handle-gesture ((drei drei-gadget-pane) gesture)
  (let ((*command-processor* drei)
        (*abort-gestures* *esa-abort-gestures*))
    ;; It is important that the minibuffer of the Drei object is
    ;; actually the minibuffer that will be used for output, or it
    ;; will not be properly redisplayed by `display-drei'.
    (accepting-from-user (drei)
      (letf (((minibuffer drei) (or (minibuffer drei) *minibuffer*
                                    (unless (eq drei *standard-input*)
                                      *standard-input*))))
        (handler-case (process-gesture drei gesture)
          (unbound-gesture-sequence (c)
            (display-message "~A is unbound" (gesture-name (gestures c))))
          (abort-gesture ()
            (display-message "Aborted")))
        (display-drei drei)
        (when (modified-p (buffer drei))                   
          (clear-modify (buffer drei))
          (when (gadget-value-changed-callback drei)
            (value-changed-callback drei
                                    (gadget-client drei)
                                    (gadget-id drei)
                                    (gadget-value drei))))))))

(defmethod execute-drei-command :after ((drei drei-gadget-pane) command)
  (with-accessors ((buffer buffer)) drei
    (when (syntax buffer)
      (update-syntax buffer (syntax buffer)))
    (when (modified-p buffer)
      (setf (needs-saving buffer) t))))

;;; This is the method that functions as the entry point for all Drei
;;; gadget logic.
(defmethod handle-event ((gadget drei-gadget-pane) (event key-press-event))
  (unless (and (currently-processing-p gadget) (directly-processing-p gadget))
    (letf (((currently-processing-p gadget) t))
      (let ((gesture (convert-to-gesture event)))
        (when (proper-gesture-p gesture)
          (with-bound-drei-special-variables (gadget :prompt (format nil "~A " (gesture-name gesture)))
            (let ((*standard-input* (or *minibuffer* *standard-input*)))
              (handle-gesture gadget gesture))))))))

(defmethod handle-event :before 
    ((gadget drei-gadget-pane) (event pointer-button-press-event))
  (let ((previous (stream-set-input-focus gadget)))
    (when (and previous (typep previous 'gadget))
      (disarmed-callback previous (gadget-client previous) (gadget-id previous)))
    (armed-callback gadget (gadget-client gadget) (gadget-id gadget))))

(defmethod invoke-accepting-from-user ((drei drei-gadget-pane) (continuation function))
  ;; When an `accept' is called during the execution of a command for
  ;; the Drei gadget, we must deactivate the gadget in order to not
  ;; eat keyboard events.
  (unwind-protect (progn (disarmed-callback drei t t)
                         (funcall continuation))
    (armed-callback drei t t)))

(defmethod additional-command-tables append ((drei drei-gadget-pane)
                                             (table drei-command-table))
  `(exclusive-gadget-table))

(defclass drei-area (drei standard-sequence-output-record
                          command-processor
                          instant-macro-execution-mixin)
  ((%background-ink :initarg :background-ink
                    :reader background-ink
                    :initform +background-ink+)
   (%min-width :reader min-width
               :initarg :min-width
               :initform 0
               :documentation "The minimum width of the Drei
editable area. Should be an integer >= 0 or T, meaning that it
will extend to the end of the viewport, if the Drei area is in a
scrolling arrangement.")
   (%drei-position :accessor input-editor-position
                   :initarg :input-editor-position
                   :documentation "The position of the Drei
editing area in the coordinate system of the encapsulated
stream. An (X,Y) list, not necessarily the same as the position
of the associated output record."))
  (:default-initargs :command-executor 'execute-drei-command)
  (:documentation "A Drei editable area implemented as an output
record."))

(defmethod initialize-instance :after ((area drei-area)
				       &key)
  (setf (input-editor-position area)
        (multiple-value-list (output-record-position (editor-pane area))))
  (tree-recompute-extent area))

(defmethod display-drei ((drei drei-area))
  (display-drei-area drei))

;; For areas, we need to switch to ESA abort gestures after we have
;; left the CLIM gesture reading machinery, but before we start doing
;; ESA gesture processing.
(defmethod process-gesture :around ((command-processor drei-area) gesture)
  (let ((*abort-gestures* *esa-abort-gestures*))
    (call-next-method)))

(defmethod (setf active) :after (new-val (drei drei-area))
  (replay drei (editor-pane drei)))

(defmethod additional-command-tables append ((drei drei-area) (table drei-command-table))
  `(exclusive-input-editor-table))

(defclass drei-minibuffer-pane (minibuffer-pane)
  ()
  (:default-initargs
   :background +light-gray+ :max-height 20
   :height 20 :min-height 20))

(defclass drei-constellation (vrack-pane)
  ((drei :initform (error "A Drei instance must be provided for the constellation.")
         :accessor drei
         :initarg :drei)
   (minibuffer :initform (error "A minibuffer instance must be provided for the constellation.")
               :accessor minibuffer
               :initarg :minibuffer))
  (:documentation "A constellation of a Drei gadget instance and
  a minibuffer."))

(defmethod display-drei :after ((drei drei))
  (when (and *minibuffer* (not (eq *minibuffer* (editor-pane drei))))
    ;; We need to use :force-p t to remove any existing output from
    ;; the pane.
    (redisplay-frame-pane (pane-frame *minibuffer*) *minibuffer* :force-p t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Programmer interface stuff
;;;
;;; We want it to be dead-easy to integrate Drei in CLIM applications.

;;; XXX This is brittle. If an :around method, that does funky
;;; (side-effecting) stuff, runs before this method, things might
;;; break. Let's hope nothing of that sort happens (this works in
;;; McCLIM. The method, not the hoping.)
(defmethod make-pane-1 :around (fm (frame application-frame)
                                   (type (eql :drei))
                                   &rest args &key
                                   (syntax nil) (initial-contents "")
                                   (minibuffer t) (border-width 1)
                                   (scroll-bars :horizontal)
                                   (drei-class 'drei-gadget-pane))
  (check-type initial-contents array)
  (check-type border-width integer)
  (check-type scroll-bars (member t :both :vertical :horizontal nil))
  (with-keywords-removed (args (:minibuffer :scroll-bars :border-width
                                            :syntax :drei-class))
    (let* ((borderp (and border-width (plusp border-width)))
           (minibuffer-pane (cond ((eq minibuffer t)
                                   (make-pane 'drei-minibuffer-pane))
                                  ((typep minibuffer 'minibuffer-pane)
                                   minibuffer)
                                  ((null minibuffer)
                                   nil)
                                  (t (error "Provided minibuffer
is not T, NIL or a `minibuffer-pane'."))))
           (drei-pane (apply #'make-pane-1 fm frame drei-class
                             :minibuffer minibuffer-pane args))
           (pane drei-pane))
      (letf (((read-only-p (buffer drei-pane)) nil))
        (insert-sequence (point drei-pane) initial-contents))
      (if syntax
          (setf (syntax (buffer drei-pane))
                (make-instance (or (when (syntaxp syntax)
                                     syntax)
                                   (syntax-from-name (string syntax))
                                   (error "Syntax ~A not found" (string syntax)))
                               :buffer (buffer drei-pane)))
          (update-syntax (buffer drei-pane) (syntax (buffer drei-pane))))
      (when scroll-bars
        (setf pane (scrolling (:scroll-bar scroll-bars)
                     pane)))
      (when minibuffer
        (setf pane (make-pane 'drei-constellation
                              :drei drei-pane
                              :minibuffer minibuffer-pane
                              :contents (list pane minibuffer-pane))))
      (when borderp
        (setf pane (#+(or mcclim building-mcclim)
                      climi::bordering
                      #-(or mcclim building-mcclim) outlining
                      (:border-width border-width)
                      pane)))
      pane)))
