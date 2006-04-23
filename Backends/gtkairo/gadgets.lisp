;;; -*- Mode: Lisp; -*-

;;;  (c) 2006 David Lichteblau (david@lichteblau.com)

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

(in-package :clim-gtkairo)

(defclass gadget-event (window-event) ())

(defun make-gadget-event (sheet)
  (make-instance 'gadget-event :sheet sheet))


;;;; Classes

;; FIXME: Hier implementieren wir die Widgets nicht vollstaendig selbst,
;; sondern erben von den Standard-Widgets.  Damit das gut geht, muessen
;; wir unten deren Redisplay-Methoden unterdruecken...  Besser waere es
;; vielleicht, von TOGGLE-BUTTON statt TOGGLE-BUTTON-PANE zu erben und
;; alles selbst zu machen.  Mindestens COMPOSE-SPACE muesste man dann
;; hier implementieren.
(defclass gtk-button (native-widget-mixin push-button) ())
(defclass gtk-check-button (native-widget-mixin toggle-button-pane) ())
(defclass gtk-radio-button (native-widget-mixin toggle-button-pane) ())
(defclass gtk-vscale (native-widget-mixin slider-pane) ())
(defclass gtk-hscale (native-widget-mixin slider-pane) ())
(defclass gtk-vscrollbar (native-widget-mixin scroll-bar-pane) ())
(defclass gtk-hscrollbar (native-widget-mixin scroll-bar-pane) ())


;;;; Constructors

(defmethod realize-native-widget ((sheet gtk-button))
  (let ((button (gtk_button_new_with_label (climi::gadget-label sheet))))
    (when (pane-background sheet)
      (gtk-widget-modify-bg button (pane-background sheet)))
    button))

(defmethod realize-native-widget ((sheet gtk-check-button))
  (gtk_check_button_new_with_label (climi::gadget-label sheet)))

(defun make-scale (fn sheet)
  (let* ((min (df (gadget-min-value sheet)))
	 (max (df (gadget-max-value sheet)))
	 (n (or (climi::slider-number-of-quanta sheet) 100))
	 (widget (funcall fn min max (/ (- max min) n))))
    (gtk_scale_set_digits widget (climi::slider-decimal-places sheet))
    (gtk_scale_set_draw_value widget
			      (if (climi::gadget-show-value-p sheet) 1 0))
    widget))

(defmethod realize-native-widget ((sheet gtk-vscale))
  (make-scale #'gtk_vscale_new_with_range sheet))

(defmethod realize-native-widget ((sheet gtk-hscale))
  (make-scale #'gtk_hscale_new_with_range sheet))

(defun make-scrollbar (fn sheet)
  (let* ((min (df (gadget-min-value sheet)))
	 (max (df (gadget-max-value sheet)))
	 (l (- max min))
	 (adjustment
	  ;; FIXME!
	  (gtk_adjustment_new 0.0d0 min max (/ l 100) (/ l 10) l)))
    (funcall fn adjustment)))

(defmethod realize-native-widget ((sheet gtk-vscrollbar))
  (make-scrollbar #'gtk_vscrollbar_new sheet))

(defmethod realize-native-widget ((sheet gtk-hscrollbar))
  (make-scrollbar #'gtk_hscrollbar_new sheet))

(defmethod realize-native-widget ((sheet gtk-radio-button))
  (let* ((first
	  (some #'sheet-direct-mirror (sheet-children (gadget-client sheet))))
	 (group (if first
		    (gtk_radio_button_get_group (mirror-widget first))
		    (cffi:null-pointer))))
    (gtk_radio_button_new_with_label group (climi::gadget-label sheet))))


;;;; Event definition

(defmethod connect-native-signals ((sheet native-widget-mixin) widget)
  (connect-signal widget "clicked" 'clicked-handler))

(defmethod connect-native-signals ((sheet gtk-vscale) widget)
  (connect-signal widget "value-changed" 'clicked-handler))

(defmethod connect-native-signals ((sheet gtk-hscale) widget)
  (connect-signal widget "value-changed" 'clicked-handler))

(defmethod connect-native-signals ((sheet gtk-vscrollbar) widget)
  (connect-signal widget "value-changed" 'clicked-handler))

(defmethod connect-native-signals ((sheet gtk-hscrollbar) widget)
  (connect-signal widget "value-changed" 'clicked-handler))


;;;; Event handling

(defmethod handle-event ((pane gtk-button) (event gadget-event))
  (activate-callback pane (gadget-client pane) (gadget-id pane)))

(defmethod handle-event ((pane gtk-check-button) (event gadget-event))
  (setf (gadget-value pane :invoke-callback t) (not (gadget-value pane))))

(defmethod handle-event ((pane gtk-radio-button) (event gadget-event))
  (setf (gadget-value pane :invoke-callback t) (not (gadget-value pane))))

(defmethod handle-event ((pane gtk-vscale) (event gadget-event))
  (setf (gadget-value pane :invoke-callback t)
	(gtk_range_get_value (mirror-widget (sheet-direct-mirror pane)))))

(defmethod handle-event ((pane gtk-hscale) (event gadget-event))
  (setf (gadget-value pane :invoke-callback t)
	(gtk_range_get_value (mirror-widget (sheet-direct-mirror pane)))))

(defmethod handle-event ((pane gtk-vscrollbar) (event gadget-event))
  (setf (gadget-value pane :invoke-callback t)
	(gtk_range_get_value (mirror-widget (sheet-direct-mirror pane)))))

(defmethod handle-event ((pane gtk-hscrollbar) (event gadget-event))
  (setf (gadget-value pane :invoke-callback t)
	(gtk_range_get_value (mirror-widget (sheet-direct-mirror pane)))))



;;; Workarounds

(defmethod handle-repaint ((pane native-widget-mixin) region)
  (declare (ignore region))
  ;; siehe oben
  )


;;; Vermischtes

(defmethod (setf gadget-value) :after
    (value (gadget gtk-radio-button) &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-gtk ()
    (let ((mirror (sheet-direct-mirror gadget)))
      (when mirror
	;; see hack in clicked-handler
	(gtk_toggle_button_set_active (mirror-widget mirror)
				      (if value 1 0))))))

(defmethod (setf gadget-value) :after
    (value (gadget gtk-check-button) &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-gtk ()
    (let ((mirror (sheet-direct-mirror gadget)))
      (when mirror
	;; see hack in clicked-handler
	(gtk_toggle_button_set_active (mirror-widget mirror)
				      (if value 1 0))))))

;; KLUDGE: this is getting called before the sheet has been realized.
(defmethod compose-space ((gadget native-widget-mixin) &key width height)
  (declare (ignore width height))
  (let* ((widget (native-widget gadget))
	 (widgetp widget))
    (unless widgetp
      (setf widget (realize-native-widget gadget)))
    (prog1
	(cffi:with-foreign-object (r 'gtkrequisition)
	  (gtk_widget_size_request widget r)
	  (cffi:with-foreign-slots ((width height) r gtkrequisition)
	    (make-space-requirement :width width :height height)))
      (unless widgetp
	(gtk_widget_destroy widget)))))
