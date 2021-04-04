;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;  (c) copyright 2000 by Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2014 by Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2004 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2019, 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;  (c) copyright 2021 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Menu frame class. This class does not implement fully the application frame
;;; protocol.
;;;

(in-package #:clim-internals)

(defclass menu-frame (application-frame)
  ((left :initform 0 :initarg :left)
   (top :initform 0 :initarg :top)
   (min-width :initform nil :initarg :min-width)
   (top-level-sheet :initform nil :reader frame-top-level-sheet)
   (panes :reader frame-panes :initarg :panes)
   (graft :initform nil :accessor graft)
   (state :initarg :state
          :initform :disowned
          :reader frame-state)
   (manager :initform nil
            :reader frame-manager
            :accessor %frame-manager)))

(defclass menu-unmanaged-top-level-sheet-pane (unmanaged-top-level-sheet-pane)
  ())

(defmethod enable-frame ((frame menu-frame))
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) t)
  (setf (slot-value frame 'state) :enabled)
  (note-frame-enabled (frame-manager frame) frame))

(defmethod disable-frame ((frame menu-frame))
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) nil)
  (setf (slot-value frame 'state) :disabled)
  (note-frame-disabled (frame-manager frame) frame))

(defmethod shrink-frame ((frame menu-frame))
  (declare (ignore frame))
  (warn "MENU-FRAME can't be shrunk."))

(defun make-menu-frame (pane &key (left 0) (top 0) (min-width 1))
  (make-instance 'menu-frame :panes pane :left left :top top :min-width min-width))
