;;; -*- Mode: Lisp; Package: User -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2001 by Julien Boninfnate (boninfan@emi.u-bordeaux.fr)

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

  #-mk-defsystem
  (load "library:subsystems/defsystem"))

(pushnew :CLIM *features*)

#+mk-defsystem (use-package "MK")

(defsystem :clim #-mk-defsystem ()
  #+mk-defsystem :source-pathname #+mk-defsystem *clim-directory*
  #+mk-defsystem :source-extension #+mk-defsystem "lisp"
  #+mk-defsystem :components
  (:serial

   ;; First possible patches
   #+cmu       "lisp-dep/fix-cmu"

   "package"

   "decls"

   #.(or
      #+(and :cmu :mp (not :pthread))  "lisp-dep/mp-cmu"
      #+excl                           "lisp-dep/mp-acl"
      #| fall back |#                  "lisp-dep/mp-nil")
   "utils"
   "defresource"
   "setf-star"
   
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
   "repaint"
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
   "table-formatting"
   "postscript-medium"
   ))
   
(defsystem :clim-opengl #-mk-defsystem ()
  #+mk-defsystem :source-pathname #+mk-defsystem *clim-directory*
  #+mk-defsystem :source-extension #+mk-defsystem "lisp"
  #+mk-defsystem :depends-on #+mk-defsystem (:clim)
  #+mk-defsystem :components
  (:serial
   #-mk-defsystem :clim
   "opengl/opengl-x-frame-manager"
   "opengl/opengl-frame-manager"
   "opengl/opengl-x-port-before"
   "opengl/opengl-port"
   "opengl/opengl-x-port-after"
   "opengl/opengl-medium"
   "opengl/opengl-x-graft"
   ))

 
(defsystem :clim-examples #-mk-defsystem ()
  #+mk-defsystem :source-pathname #+mk-defsystem *clim-directory*
  #+mk-defsystem :source-extension #+mk-defsystem "lisp"
  #+mk-defsystem :depends-on #+mk-defsystem (:clim-opengl)
  #+mk-defsystem :components
  (:serial
   #-mk-defsystem :clim-clx
   "examples/calculator"
   "examples/colorslider"
   "examples/menutest"
   "examples/address-book"
   "examples/traffic-lights"
   "examples/clim-fig"
   "examples/postscript-test"
   "examples/transformations-test"
   ))
