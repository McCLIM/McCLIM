;;; -*- Mode: Lisp; Package: User -*-

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

(in-package :common-lisp-user)

#+:excl(require :clx)
#+:excl(require :loop)

(clim-defsystem (:clim-clx :depends-on (:clim #+(and sbcl asdf) :clx))
  "Backends/CLX/package"
  "Backends/CLX/keysyms-common"
  "Backends/CLX/keysyms"
  "Backends/CLX/keysymdef"
  "Backends/CLX/port"
  "Backends/CLX/medium"
  "Backends/CLX/graft"
  "Backends/CLX/frame-manager"
  "Backends/CLX/image"
  "Backends/CLX/clim-extensions"
  )

