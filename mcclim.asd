;;; -*- Mode: Lisp -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000, 2014 by 
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2005 by
;;;           Andreas Fuchs (asf@boinkor.net)
;;;
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


;;; Really, I wouldn't bother with anything but ASDF. Almost every lisp
;;; ships with it, and it has the added benefit of ASDF-INSTALL.
;;; Get ASDF, and be welcome to the 21st century. -- [2005-01-31:asf]

(defpackage :mcclim.system
  (:use :asdf :cl))
(in-package :mcclim.system)

(defparameter *clim-directory* (directory-namestring *load-truename*))

;;; Legacy CMUCL support stuff
#+cmu
(progn
  (unless (fboundp 'ext:stream-read-char)
    (unless (ignore-errors (ext:search-list "gray-streams:"))
      (setf (ext:search-list "gray-streams:")
        '("target:pcl/" "library:subsystems/")))
    (if (fboundp 'extensions:without-package-locks)
        (extensions:without-package-locks
         (load "gray-streams:gray-streams-library"))
      (load "gray-streams:gray-streams-library")))
  #-(or clx clim-gtkairo clim-graphic-forms)
  (require :clx)
  #+mp (when (eq mp::*initial-process* mp::*current-process*)
         (format t "~%~%You need to run (mp::startup-idle-and-top-level-loops) to start up the multiprocessing support.~%~%")))

;;; Make CLX asdf-loadable on Allegro 6.2
;;; possibly this should be further refined to function properly for
;;; Allegro on Windows platforms. [2005/04/18:rpg]

#+allegro
(defsystem :clx
  :components ((:file "require-clx")))

;;; Clozure CL native GUI stuff
#+clim-beagle
(require :cocoa)


#+clisp
(when (and (find-package :xlib)
           ;; Just some random symbol I know is unexported in CLISP's CLX.
           (not (eq (nth-value 1 (find-symbol "SET-SELECTION-OWNER" :xlib)) :external)))
  (warn "~@<CLISP provided you a CLX that is not capable of running the McCLIM CLX backend.
Deleting it, that it may be replaced with a working one.~@:>")
  (ext:without-package-lock ("XLIB")
    (delete-package :xlib)))

(defsystem :clim-clx
  :depends-on (:clim #+(or sbcl openmcl ecl clisp allegro) :clx)
  :components
  ((:module "Backends/CLX"
    :components
    ((:file "package")
     (:file "image" :depends-on ("package"))
     (:file "keysyms-common" :depends-on ("package"))
     (:file "keysyms" :depends-on ("keysyms-common" "package"))
     (:file "keysymdef" :depends-on ("keysyms-common" "package"))
     (:file "port" :depends-on ("keysyms-common" "keysyms" "package"))
     (:file "medium" :depends-on ("port" "keysyms" "package"))
     (:file "graft" :depends-on ("port" "package"))
     (:file "frame-manager" :depends-on ("medium" "port" "package"))))))

(defsystem :clim-beagle
  :depends-on (clim)
  :components
  ((:module "Backends"
            :components
            ((:module "beagle"
              :serial t
              :components
              ((:file "package")
               (:file "cocoa-util")
               (:module "native"
                :components ((:file "lisp-bezier-path")
                             (:file "lisp-window")
                             (:file "lisp-window-delegate")
                             (:file "lisp-view"
                              :depends-on ("lisp-bezier-path"))
                             (:file "lisp-view-additional"
                              :depends-on ("lisp-view"))
                             (:file "lisp-scroller")
                             (:file "lisp-slider")
                             (:file "lisp-button")
                             (:file "lisp-image")))
               (:module "windowing"
                :depends-on ("native")
                :components ((:file "port")
                             (:file "frame-manager")
                             (:file "mirror")
                             (:file "graft")))
               (:module "native-panes"
                :components ((:file "beagle-scroll-bar-pane")
                             (:file "beagle-slider-pane")
                             ;; Basic buttons - not collections of buttons
                             (:file "beagle-fundamental-button-pane")
                             ;; Button collections (radio + checkbox)
                             ;; (:file "beagle-button-collection-pane")
                             (:file "scroller-pane-fix")))
               (:module "output"
                :depends-on ("windowing")
                :components ((:file "medium")
                             (:file "fonts")))
               (:module "input"
                :depends-on ("windowing")
                :components ((:file "events")
                             (:file "keysymdef")))
               (:module "glimpse"
                :components ((:file "glimpse")
                             (:file "glimpse-support")
                             (:file "glimpse-command-tables")
                             (:file "glimpse-present-process"
                              :depends-on ("glimpse" "glimpse-support"))
                             (:file "glimpse-present-window"
                              :depends-on ("glimpse" "glimpse-support"))
                             (:file "glimpse-modeless-commands"
                              :depends-on ("glimpse" "glimpse-support"))
                             (:file "glimpse-process-commands"
                              :depends-on ("glimpse" "glimpse-support"))
                             (:file "glimpse-window-commands"
                              :depends-on ("glimpse" "glimpse-support"))))
               (:module "profile"
                :components ((:file "profile")))
               (:module "tests"
                :components ((:file "drawing-tests")
                             (:file "graft-tests")))))))))

;;; A system that loads the appropriate backend for the current
;;; platform.
(defsystem :clim-looks
    :depends-on (:clim :clim-postscript
                 ;; If we're on an implementation that ships CLX, use
                 ;; it. Same if the user has loaded CLX already.
                 #+(and (or sbcl scl openmcl ecl clx allegro)
                        (not (or clim-gtkairo clim-graphic-forms clim-beagle)))
                 :clim-clx
                 #+clim-graphic-forms             :clim-graphic-forms
                 #+clim-gl                        :clim-opengl
                 ;; OpenMCL and MCL support the beagle backend (native
                 ;; OS X look&feel on OS X).
                 #+clim-beagle :clim-beagle

                 #+clim-gtkairo :clim-gtkairo

                 ;; null backend
                 :clim-null
                 )
    :components (#-(or clim-gtkairo clim-graphic-forms clim-beagle)
                 (:file "Looks/pixie")))

;;; The actual McCLIM system that people should to use in their ASDF
;;; package dependency lists.
(defsystem :mcclim
  :author "Alexey Dejneka
Andreas Fuchs
Andy Hefner
Arnaud Rouanet
Brian Mastenbrook
Brian Spilsbury
Christophe Rhodes
Clemens Fruhwirth
Daniel Barlow
Duncan Rose
Edena Pixel
Frank Buss
Gilbert Baumann
Iban Hatchondo
Julien Boninfan
Lionel Salabartan
Max-Gerd Retzlaff
Mike McDonald
Peter Mechleborg
Rainer Joswig
Robert Goldman
Robert Strandh
Rudi Schlatte
Timothy Moore"
  :license "LGPL-2.1+"
  :version "0.9.7-dev"
  :description "McCLIM is an implementation of the CLIM 2.0 specification."
  :long-description "McCLIM is an implementation of the CLIM 2.0 specification.

CLIM (Common Lisp Interface Manager) is an advanced graphical user
interface management system."
  :depends-on (:clim-looks))

(defmethod perform :after ((op load-op) (c (eql (find-system :mcclim))))
  (pushnew :clim *features*)) ;; The fact that CLIM itself is available is true when all is loaded.

;; The fact that our CLIM implementation is McCLIM is already true now.
;; This feature is notably used by ESA and DREI, in cases where they need to
;; know whether they are compiled with McCLIM or another CLIM implementation.
(pushnew :mcclim *features*) 
