;;; -*- Mode: Lisp; Package: BEAGLE -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2004 by Duncan Rose (duncan@robotcat.demon.co.uk)

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

;;; Could probably tidy this up a lot; but it seems to do what we need
;;; at the moment, so tidy up later...

;;; BEAGLE-GRAFT class

(defclass beagle-graft (graft)
  ())

;; Should a graft have a mirror? CLX uses the port-window for this. I guess we'll
;; have to use the screen.
(defmethod make-graft ((port beagle-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'beagle-graft
			      :port port :mirror (beagle-port-screen port)
			      :orientation orientation :units units)))
    (climi::%%set-sheet-region (make-bounding-rectangle 0 0
                                                        (get-width (beagle-port-screen port))
                                                        (get-height (beagle-port-screen port)))
                               graft)
    (push graft (port-grafts port))
    graft))

;; Do we want these sizes based on the visible-frame (not including menu bar or
;; dock), or on the frame (entire screen)? Let's be nice for now and base it on
;; the visible-frame.

(defmethod graft-width ((graft beagle-graft) &key (units :device))
  (let ((screen (beagle-port-screen (port graft))))
    (ecase units
      (:device (get-width screen))
      (:inches (get-width-in-inches screen))
      (:millimeters (* (get-width-in-inches screen) 25.4s0))
      (:screen-sized 1))))

(defmethod graft-height ((graft beagle-graft) &key (units :device))
  (let ((screen (beagle-port-screen (port graft))))
    (ecase units
      (:device (get-height screen))
      (:inches (get-height-in-inches screen))
      (:millimeters (* (get-height-in-inches screen) 25.4s0))
      (:screen-sized 1))))

;;; ____________________________________________________________________________
;;;
;;; Support functions specific to :beagle graft; can be invoked externally
;;; if necessary, but will be non-portable between back ends.

;; Assumes square pixels...
(defun get-pixels-per-inch (screen)
  "Returns the number of pixels per inch on the ``screen'' (native Cocoa) object
provided as an argument.
Square pixels are assumed."
  (send (send screen 'device-description)
		:object-for-key #@"NSDeviceResolution"))

(defun get-width (screen)
  (slet ((frame (send screen 'visible-frame)))
		(pref frame :<NSR>ect.size.width)))

(defun get-width-in-inches (screen)
  (/ (get-width screen) (get-width-pixels-per-inch screen)))

(defun get-width-pixels-per-inch (screen)
  (slet ((size (send (get-pixels-per-inch screen) 'size-value)))
		(pref size :<NSS>ize.width)))

(defun get-height-in-inches (screen)
  (/ (get-height screen) (get-height-pixels-per-inch screen)))

(defun get-height (screen)
  (slet ((frame (send screen 'visible-frame)))
		(pref frame :<NSR>ect.size.height)))

(defun get-height-pixels-per-inch (screen)
  (slet ((size (send (get-pixels-per-inch screen) 'size-value)))
		(pref size :<NSS>ize.height)))




