;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)

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

;;; CLX-FRAME-MANAGER class

(defclass clx-frame-manager (frame-manager)
  ()
  )

;;; This is an example of how make-pane-1 might create specialized
;;; instances of the generic pane types based upon the type of the
;;; frame-manager. However, in the CLX case, we don't expect there to
;;; be any CLX specific panes. CLX uses the default generic panes
;;; instead.
(defmethod make-pane-1 ((fm clx-frame-manager) (frame application-frame) type &rest args)
  (apply #'make-instance
	 (or (find-symbol (concatenate 'string "CLX-" (symbol-name type)) :clim)
	     (find-symbol (concatenate 'string "CLX-" (symbol-name type) "-PANE") :clim)
	     (find-symbol (concatenate 'string (symbol-name type) "-PANE") :clim)
	     type)
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))


(defmethod adopt-frame :after ((fm clx-frame-manager) (frame menu-frame))
  (xlib:map-window (sheet-direct-mirror (slot-value frame 'top-level-sheet))))

(defmethod adopt-frame :after ((fm clx-frame-manager) (frame application-frame))
  (xlib:map-window (sheet-direct-mirror (slot-value frame 'top-level-sheet))))
