;;; -*- Mode: Lisp; Package: User -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

(in-package :user)

#+excl(require :clx)
#+excl(require :loop)

(defpackage :CLIM-INTERNALS
  (:use #+clisp :clos #+excl :stream :lisp)
  (:nicknames :climi)
  #+excl(:import-from :excl compile-system load-system)
  )

(defpackage :CLIM
  (:use :clim-internals :common-lisp)
  )

(defpackage :CLIM-DEMO
  (:use :clim :common-lisp)
  #+excl(:import-from :excl compile-system load-system exit)
  )

(pushnew :CLIM *features*)
(provide :CLIM)

#+mk-defsystem (use-package "MK")

(defsystem :CLIM #-(or cmu clisp) ()
  #+clisp :source-extension #+clisp "lisp"
  #+(or cmu clisp) :components
  (:serial
   "design"
   "X11-colors"
   ;; "brectangle"
   "coordinates"
   "transforms"
   "regions"
   "sheets"
   "ports"
   "grafts"
   "medium"
   "output"
   "input"
   "events"
   "graphics"
   "stream-output"
   "recording"
   "stream-input"
   "presentations"
   "commands"
   "frames"
   "panes"
   "exports"
   "gadgets"

   "clx-port"
   "clx-medium"
   "clx-graft"
   "clx-frame-manager"
   
   "examples/calculator"
   "examples/colorslider"
   ))

