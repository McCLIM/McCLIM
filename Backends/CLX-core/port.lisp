;;;  (c) copyright 2016 by 
;;;           Robert Strandh (robert.strandh@gmail.com)

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

(cl:in-package #:mcclim-clx-core)

(defclass port (clim:basic-port)
  (;; This slot contains the CLX DISPLAY object that defines the
   ;; connection between this port and the display server.
   (%display :initarg :display :reader display)
   ;; This slot contains the CLX SCREEN object that contain the root
   ;; window of this port.
   (%screen :initarg :screen :reader screen)
   ;; this slot contains the root window of the screen.
   (%root :initarg :root :reader root)))
