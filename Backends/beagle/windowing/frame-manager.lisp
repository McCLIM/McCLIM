;;; -*- Mode: Lisp; Package: BEAGLE -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000,2001 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2003, 2004 by
;;;           Duncan Rose (duncan@robotcat.demon.co.uk)

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


(in-package :beagle)


(defclass beagle-standard-frame-manager (frame-manager) ()
  (:documentation "Frame manager for Beagle back end that provides the ``cross platform'' McCLIM
look and feel"))


(defclass beagle-aqua-frame-manager (frame-manager) ()
  (:documentation "Frame manager for Beagle back end that provides Apple's Aqua look
and feel for McCLIM. If any pane types are not implemented for Beagle / Aqua, the
``cross platform'' look and feel will be used."))


;;; This is an example of how make-pane-1 might create specialized instances of the
;;; generic pane types based upon the type of the frame-manager. Unlike in the CLX
;;; case, we *do* expect there to be Beagle specific panes (eventually!).
(defmethod make-pane-1 ((fm beagle-aqua-frame-manager)
			(frame application-frame)
			type
			&rest args)
  (apply #'make-instance
	 (or (find-symbol (concatenate 'string
				       (symbol-name '#:beagle-)
				       (symbol-name type))
			  :beagle)
	     (find-symbol (concatenate 'string
				       (symbol-name '#:beagle-)
				       (symbol-name type)
				       (symbol-name '#:-pane))
			  :beagle)
	     (find-symbol (concatenate 'string
				       (symbol-name type)
				       (symbol-name '#:-pane))
			  :climi)
	     type)
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))


;;; We must implement this method to ensure the menu-frame has its top + left slots set.
(defmethod adopt-frame :before ((fm beagle-aqua-frame-manager) (frame menu-frame))
  ;; Temporary kludge.
  (when (eq (slot-value frame 'climi::top) nil)
    (slet ((mouse-location (send (@class ns-event) 'mouse-location)))
      (setf (slot-value frame 'climi::left) (decf (pref mouse-location :<NSP>oint.x) 10)
            (slot-value frame 'climi::top)  (incf (pref mouse-location :<NSP>oint.y) 10)))))


;;; ----------------------------------------------------------------------------

;;; "standard" look and feel (i.e. exactly the same, give or take, as the CLX
;;; (and other?) back ends.

;;; Don't even check for beagle-* panes we don't want to find them.
(defmethod make-pane-1 ((fm beagle-standard-frame-manager)
			(frame application-frame)
			type
			&rest args)
  (apply #'make-instance
	 (or (find-symbol (concatenate 'string
				       (symbol-name type)
				       (symbol-name '#:-pane))
			  :climi)
	     type)
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))


;;; We must implement this method to ensure the menu-frame has its top + left slots set.
(defmethod adopt-frame :before ((fm beagle-standard-frame-manager) (frame menu-frame))
  ;; Temporary kludge.
  (when (eq (slot-value frame 'climi::top) nil)
    (slet ((mouse-location (send (@class ns-event) 'mouse-location)))
      (setf (slot-value frame 'climi::left) (decf (pref mouse-location :<NSP>oint.x) 10)
            (slot-value frame 'climi::top)  (incf (pref mouse-location :<NSP>oint.y) 10)))))


(defmethod adopt-frame :after ((fm beagle-standard-frame-manager) (frame menu-frame))
  (declare (ignore fm))
  (when (sheet-enabled-p (slot-value frame 'top-level-sheet))
    (send (send (sheet-direct-mirror (slot-value frame 'top-level-sheet)) 'window)
	  :make-key-and-order-front nil)))  ; <- just :order-front?


(defmethod adopt-frame :after ((fm beagle-aqua-frame-manager) (frame menu-frame))
  (declare (ignore fm))
  (when (sheet-enabled-p (slot-value frame 'top-level-sheet))
    (send (send (sheet-direct-mirror (slot-value frame 'top-level-sheet)) 'window)
	  :make-key-and-order-front nil)))  ; <- just :order-front?


;;; Will this method be invoked for all frame types? E.g. what if we have CLX + Beagle
;;; frame managers? Will this method be invoked for both? Need to run a test...

;;; Aargh, menu frames go through a different path... ENABLE-FRAME is not invoked
;;; for them :-(
(defmethod enable-frame :after (frame)
  (declare (special *beagle-port*))
  ;; How to get the frame manager for the frame? (frame-manager frame) [might be
  ;; needed if we do indeed need to differentiate between CLX fm and Beagle fm].
  ;; A better solution might be to introduce a BEAGLE-FRAME and a CLX-FRAME type.
  (let* ((sheet  (frame-top-level-sheet frame))
	 (window (send (port-lookup-mirror *beagle-port* sheet) 'window)))
    (unless (send window 'is-key-window)
      (send window :make-key-and-order-front nil))))

