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

(in-package :common-lisp-user)

#+excl(require :clx)
#+excl(require :loop)

(defparameter *clim-directory* (directory-namestring *load-truename*))

#+cmu
(progn
  (unless (fboundp 'stream-read-char)
    (unless (ignore-errors (ext:search-list "gray-streams:"))
      (setf (ext:search-list "gray-streams:")
	'("target:pcl/" "library:subsystems/")))
    (load "gray-streams:gray-streams-library"))
  #+nil
  (load (merge-pathnames "patch-cmu.lisp" *clim-directory*))

  #-MK-DEFSYSTEM
  (load "library:subsystems/defsystem"))

(pushnew :CLIM *features*)

#+mk-defsystem (use-package "MK")

(defsystem :clim #-mk-defsystem ()
  #+mk-defsystem :source-pathname #+mk-defsystem *clim-directory*
  #+mk-defsystem :source-extension #+mk-defsystem "lisp"
  #+mk-defsystem :components
  (:serial
   "package"

   #.(OR
      #+(AND :CMU :MP (NOT :PTHREAD))  "lisp-dep/mp-cmu"
      #+EXCL                           "lisp-dep/mp-acl"
      #| fall back |#                  "lisp-dep/mp-nil")
   "defresource"
   
   "design"
   "X11-colors"
   ;; "brectangle"
   "coordinates"
   "transforms"
   "regions"
   "sheets"
   "pixmap"
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
;   "exports"
   "gadgets"
   "menu"
   ))
   
(defsystem :clim-clx #-mk-defsystem ()
  #+mk-defsystem :source-pathname #+mk-defsystem *clim-directory*
  #+mk-defsystem :source-extension #+mk-defsystem "lisp"
  #+mk-defsystem :depends-on #+mk-defsystem (:clim)
  #+mk-defsystem :components
  (:serial
   #-mk-defsystem :clim
   "clx-port"
   "clx-medium"
   "clx-graft"
   "clx-frame-manager"
   "image"
   ))
   
(defsystem :clim-examples #-mk-defsystem ()
  #+mk-defsystem :source-pathname #+mk-defsystem *clim-directory*
  #+mk-defsystem :source-extension #+mk-defsystem "lisp"
  #+mk-defsystem :depends-on #+mk-defsystem (:clim-clx)
  #+mk-defsystem :components
  (:serial
   #-mk-defsystem :clim-clx
   "examples/calculator"
   "examples/colorslider"
   "examples/menutest"
   "examples/address-book"
   "examples/traffic-lights"
   "examples/clim-fig"
   ))
