;;;; xkb.lisp

#|

Support for XKB:

1.  The client must communicate to the server that it is XKB-aware and
    subscribe for XKB events.

2.  The client keeps a copy of the XKB keymap and updates it as needed.  It is
    used to interpret keycodes.

3.  Key press and key release events are treated differently.

Implementation

We rely on the xkeyboard library for the XKB functionality.  Unfortunately, the
library is incomplete (in particular, regarding the events).

In terms of ports, we confine XKB-specific functionality to the XKB-MIXIN
class.  Mixing it with CLX-PORT, we obtain the basic CLX-XKB-PORT class for
XKB-aware ports.

CLX-PORT seems the right base class because it is the most general class with
nontrivial initialization.

XKB initialization is performed in an :AFTER method of INITIALIZE-CLX for this
class.  In order for CLX-XKB-PORT to be caught up as the default port class, we
set :PORT-TYPE property of the :X11 and :CLX symbols to the CLX-XKB-PORT symbol
overriding the port.lisp file.

We lazily update client's XKB keymap, invalidating it on XkbMapNotify events.
The keycodes are analyzed using the client map object associated with the
keymap, which can be retrieved with the XKB-CLIENT-MAPPING function.  The
client map is cached.

The XKB functionality is connected to the rest of CLIM through a method for
GET-NEXT-EVENT.  The method for the CLX-XKB-PORT class is implemented in much
the same way as the one for CLX-BASIC-PORT in port.lisp.  The provisory nature
of the latter is an excuse for code duplication.

The XKB-EVENT-HANDLER used by GET-NEXT-EVENT processes XKB, key press, and key
release events and falls back to EVENT-HANDLER for other types of events.  The
keycodes are analysed by the XKB-EVENT-TO-KEY-NAME-AND-MODIFIERS function being
a counterpart of CLIM-XCOMMON:X-EVENT-TO-KEY-NAME-AND-MODIFIERS.

|#

(in-package :clim-clx)

(defclass xkb-mixin ()
  ((keymap :accessor xkb-keymap :documentation "XKB keymap or NIL" :initform nil)
   (client-mapping :reader xkb-client-mapping :documentation "Cached client mapping")))

(defclass clx-xkb-port (xkb-mixin clx-port)
  ())

(setf (get :x11 :port-type) 'clx-xkb-port)
(setf (get :clx :port-type) 'clx-xkb-port)

(defmethod initialize-clx :after ((obj xkb-mixin))
  (let ((display (clx-port-display obj)))
    (xlib:enable-xkeyboard display)
    (xlib::xkb-select-all-events display)))

(defgeneric update-xkb-keymap (obj)
  (:documentation "Request the XKB keymap from the server and update the copy associated with OBJ.  Return the keymap."))

(defmethod update-xkb-keymap ((obj xkb-mixin))
  (let ((display (clx-port-display obj)))
    (setf (xkb-keymap obj) (xlib:get-map display
                                         (xlib:device-state-device-id (xlib:get-state display)) 
                                         #b11111111))))

(defgeneric invalidate-xkb-keymap (obj)
  (:documentation "Mark the copy of the XKB keymap associated with OBJ as invalid."))

(defmethod invalidate-xkb-keymap ((obj xkb-mixin))
  (setf (xkb-keymap obj) nil))

(defgeneric ensure-xkb-keymap (obj)
  (:documentation "If the XKB keymap associated with OBJ is invalid, update it.  Return the keymap."))

(defmethod ensure-xkb-keymap ((obj xkb-mixin))
  (or (xkb-keymap obj)
      (update-xkb-keymap obj)))

(defmethod (setf xkb-keymap) :after (value (obj xkb-mixin))
  (setf (slot-value obj 'client-mapping) (if (null value)
                                             nil
                                             (xlib:transform-xkb-keymap-to-client-mapping (xkb-keymap obj)))))


;;; Even though xlib:keyevent->keysym returns the modifiers, we ignore them,
;;; opting to recalculate the modifiers using
;;; clim-xcommon:x-keysym-to-clim-modifiers and the magic underneath it.  The
;;; latter function only understands non-XKB states, so we pass only the least
;;; significant byte of STATE down to it. No special treatment of shifts as in
;;; x-event-to-key-name-and-modifiers is included, because I've been unable to
;;; detect any differences.

(defun xkb-event-to-key-name-and-modifiers (port event-key keycode state)
  (declare (ignorable event-key state))
  (ensure-xkb-keymap port)
  (multiple-value-bind (keysym xkb-modifiers) (xlib:keyevent->keysym (xkb-client-mapping port) keycode state)
    (declare (ignorable xkb-modifiers))
    (let ((char (xlib:xkb/keysym->character keysym xlib:+xkbkeysymdb+))
          (keysym-name (clim-xcommon:keysym-to-keysym-name keysym)))
      (let ((modifiers (clim-xcommon:x-keysym-to-clim-modifiers
                         port
                         event-key
                         char
                         keysym-name
                         (logand state #xFF))))
        (values char modifiers keysym-name)))))

(defun xkb-event-handler (&rest args &key window event-key code state time x y root-x root-y xkb-type &allow-other-keys)
  (if (eql event-key :xkb-event)
      (when (eql xkb-type 1) ; XkbMapNotify
        (invalidate-xkb-keymap *clx-port*)
        nil)
      (let ((sheet (and window (port-lookup-sheet *clx-port* window))))
        (when sheet
          (case event-key
            ((:key-press :key-release)
             (multiple-value-bind (keyname modifier-state keysym-name)
               (xkb-event-to-key-name-and-modifiers *clx-port* 
                                                    event-key code state)
               (make-instance (if (eq event-key :key-press)
                                  'key-press-event
                                  'key-release-event)
                              :key-name keysym-name
                              :key-character (and (characterp keyname) keyname)
                              :x x :y y
                              :graft-x root-x
                              :graft-y root-y
                              :sheet (or (frame-properties (pane-frame sheet) 'focus) sheet)
                              :modifier-state modifier-state :timestamp time)))
            (otherwise (apply #'event-handler args)))))))
            
(defmethod get-next-event ((port xkb-mixin) &key wait-function (timeout nil))
  (declare (ignore wait-function))
  (let* ((*clx-port* port)
         (display (clx-port-display port)))
    (unless (xlib:event-listen display)
      (xlib:display-force-output (clx-port-display port)))
    (or (xlib:process-event (clx-port-display port) :timeout timeout :handler #'xkb-event-handler :discard-p t)
        :timeout)))
