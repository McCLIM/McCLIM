;;;; ----------------------------------------------------------------------
;;;;     Title: X11 standard keysym interpretation rules
;;;;   Created: 2016-01-25
;;;;    Author: Robert Strandh <robert.strandh@gmail.com>
;;;;   License: LGPL (See file COPYING for details).
;;;; ----------------------------------------------------------------------
;;;;  (c) copyright 2016 by Robert Strandh

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the 
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clim-clx)

(defclass keysym-interpretation ()
  (;; This slot contains a mask to be applied to an X11 modifier mask
   ;; to determine whether the "mode switch" modifier is in effect.
   (%mode-switch-mask :initarg :mode-swith-mask
		      :initform #b00000000
		      :accessor mode-switch-mask)))
