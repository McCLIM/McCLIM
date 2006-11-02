;;; -*- Mode: Lisp -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2005 by
;;;	      Andreas Fuchs (asf@boinkor.net)
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
  #-clx
  (require :clx)
  #+mp (when (eq mp::*initial-process* mp::*current-process*)
	 (format t "~%~%You need to run (mp::startup-idle-and-top-level-loops) to start up the multiprocessing support.~%~%")))

;;; Make CLX asdf-loadable on Allegro 6.2
;;; possibly this should be further refined to funciton properly for
;;; Allegro on Windows platforms. [2005/04/18:rpg]

#+allegro
(progn
  (defclass requireable-system (asdf:system)
    ())
  (defmethod asdf:perform ((op asdf:load-op) (system requireable-system))
    (require (intern (slot-value system 'asdf::name) :keyword)))
  (defmethod asdf::traverse ((op asdf:load-op) (system requireable-system))
    (list (cons op system)))  
  (defsystem :clx
    :class requireable-system))

(defmacro clim-defsystem ((module &key depends-on) &rest components)
  `(progn
     (asdf:defsystem ,module
	 ,@(and depends-on
		`(:depends-on ,depends-on))
	 :serial t
	 :components
	 (,@(loop for c in components
		  for p = (merge-pathnames
			   (parse-namestring c)
			   (make-pathname :type "lisp"
					  :defaults *clim-directory*))
		  collect `(:file ,(namestring p) :pathname ,p))))))

(defsystem :clim-lisp
    :components
  (;; First possible patches
   (:file "patch")
   (:module "Lisp-Dep"
            :depends-on ("patch")
            :components
            ((:file   #+cmu       "fix-cmu"
		      #+scl       "fix-scl"
                      #+excl      "fix-acl"
                      #+sbcl      "fix-sbcl"
                      #+openmcl   "fix-openmcl"
                      #+lispworks "fix-lispworks"
                      #+clisp     "fix-clisp")))
   (:file "package" :depends-on ("Lisp-Dep" "patch"))))

(defsystem :clim-core
    :depends-on (:clim-lisp :spatial-trees)
    :components ((:file "decls")
		 (:file "protocol-classes" :depends-on ("decls"))
                 (:module "Lisp-Dep"
                          :depends-on ("decls")
                          :components
                          ((:file #.(first
                                     (list
                                      #+(and :cmu :mp (not :pthread))  "mp-cmu"
                                      #+scl                     "mp-scl"
                                      #+sb-thread               "mp-sbcl"
                                      #+excl                    "mp-acl"
                                      #+openmcl                 "mp-openmcl"
                                      #+lispworks               "mp-lw"
                                      #| fall back |#           "mp-nil")))))
                 (:file "utils" :depends-on ("decls" "Lisp-Dep"))
                 (:file "design" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "utils"))
                 (:file "X11-colors" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "design"))
                 (:file "coordinates" :depends-on ("decls" "protocol-classes" "Lisp-Dep"))
                 (:file "setf-star" :depends-on ("decls" "Lisp-Dep"))
                 (:file "transforms" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "coordinates" "utils"))
                 (:file "regions" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "coordinates" "utils" "transforms" "setf-star" "design"))
                 (:file "sheets" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "utils" "transforms" "regions"))
                 (:file "pixmap" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "sheets" "transforms" "regions"))
                 (:file "events" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "transforms" "sheets" "utils"))
                 (:file "ports" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "events" "sheets" "pixmap" "utils"))
                 (:file "grafts" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "sheets" "ports" "transforms" "regions"))
                 (:file "medium" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "ports" "X11-colors" "utils" "pixmap" "regions"
                                                      "transforms" "design"))
                 (:file "output" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "medium"))
                 (:file "input" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "events" "regions" "sheets"))
                 (:file "repaint" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "sheets" "events"))
                 (:file "graphics" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "output" "utils" "medium" "sheets" "pixmap"
                                                         "regions" "design" "transforms"))
                 (:file "views" :depends-on ("utils" "protocol-classes"))
                 (:file "stream-output" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "design" "utils" "X11-colors" "views"
                                                              "output" "sheets" "regions" "graphics"
                                                              "medium" "setf-star"))
                 (:file "recording" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "output" "coordinates" "graphics" "design"
                                                          "medium" "transforms" "regions" "sheets"
                                                          "utils" "stream-output"))
                 (:file "encapsulate" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "sheets" "graphics" "utils" "medium" "input"
                                                            "stream-output" "recording"))
                 (:file "stream-input" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "input" "ports" "sheets" "events"
                                                            "encapsulate" "transforms" "utils"))
                 (:file "text-selection" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "X11-colors" "medium" "output"
                                                                   "transforms" "sheets" "stream-output"
                                                                   "ports" "recording" "regions"
                                                                   "events"))))

(defsystem :goatee-core
    :depends-on (:clim-core)
    :components
    ((:module "Goatee"
              :components
              ((:file "conditions")
               (:file "dbl-list")
               (:file "flexivector" :depends-on ("conditions" "dbl-list"))
               (:file "buffer" :depends-on ("conditions" "flexivector" "dbl-list"))
               (:file "editable-buffer" :depends-on ("dbl-list" "flexivector" "buffer"))
               (:file "editable-area" :depends-on ("flexivector" "editable-buffer" "dbl-list"
                                                                 "buffer"))
               (:file "clim-area" :depends-on ("flexivector" "dbl-list" "buffer"
                                                             "editable-area" "editable-buffer"))
               (:file "kill-ring" :depends-on ("editable-buffer" "dbl-list" "flexivector"
                                                                 "buffer"))
               (:file "goatee-command" :depends-on ("conditions" "clim-area" "dbl-list"
                                                                 "editable-buffer" "kill-ring"
                                                                 "editable-area" "buffer" "flexivector"))
               (:file "editing-stream" :depends-on ("goatee-command" "kill-ring" "dbl-list"
                                                                     "conditions" "editable-buffer"
                                                                     "flexivector" "clim-area" "buffer"))
               (:file "presentation-history" :depends-on ("editing-stream" "buffer"
                                                                           "flexivector" "editable-buffer"
                                                                           "goatee-command"))))))

;;; CLIM-PostScript is not a backend in the normal sense.
;;; It is an extension (Chap. 35.1 of the spec) and is an
;;; "included" part of McCLIM. Hence the defsystem is here.
(defsystem :clim-postscript
    :depends-on (:clim-core)
    :components
    ((:module "Backends/PostScript"
              :pathname #.(make-pathname :directory '(:relative "Backends" "PostScript"))
              :components
              ((:file "package")
               (:file "encoding" :depends-on ("package"))
               (:file "paper" :depends-on ("package"))
               (:file "class" :depends-on ("paper" "package"))
               (:file "font" :depends-on ("encoding" "class" "paper" "package"))
               (:file "graphics" :depends-on ("encoding" "paper" "class" "font" "package"))
               (:file "sheet" :depends-on ("paper" "class" "graphics" "package"))
               (:file "afm" :depends-on ("class" "paper" "font" "package"))
               (:file "standard-metrics" :depends-on ("font" "package"))))))

(defsystem :clim
    :depends-on (:clim-core :goatee-core)
    :components
    ((:file "text-formatting")
     (:file "input-editing")
     (:file "presentations")
     (:file "defresource")
     (:file "presentation-defs" :depends-on ("input-editing" "presentations"))
     (:file "pointer-tracking" :depends-on ("input-editing"))
     (:file "commands" :depends-on ("input-editing" "presentations"
						    "presentation-defs"))
     (:file "incremental-redisplay" :depends-on ("presentation-defs"))
     (:file "frames" :depends-on ("commands" "presentations" "presentation-defs"
                                             "pointer-tracking" "incremental-redisplay"))
     (:file "panes" :depends-on ("incremental-redisplay" "presentations"
                                                         "presentation-defs" "input-editing" "frames"))
     (:file "gadgets" :depends-on ("commands" "pointer-tracking" "input-editing" 
                                              "frames" "incremental-redisplay" "panes"))
     (:file "menu" :depends-on ("panes" "commands" "gadgets"
                                        "presentations" "frames"))
     (:file "table-formatting" :depends-on ("presentation-defs" "panes"
                                                                "presentations" "input-editing"))
     (:file "graph-formatting")
     (:file "bordered-output" :depends-on ("input-editing" "incremental-redisplay"
                                                           "presentation-defs" "panes"))
     (:file "dialog-views" :depends-on ("presentations" "incremental-redisplay"
                                                        "bordered-output" "presentation-defs"))
     (:file "dialog" :depends-on ("panes" "frames" "incremental-redisplay"
                                          "table-formatting" "presentations"
                                          "bordered-output" "presentation-defs"
                                          "dialog-views" "input-editing"
                                          "commands"))
     (:file "builtin-commands" :depends-on ("table-formatting" "commands" "presentations"
                                                               "presentation-defs" "input-editing"))
     (:file "describe" :depends-on ("presentations" "presentation-defs" "table-formatting"))
     (:file "menu-choose" :depends-on ("commands" "table-formatting" "presentation-defs"
						  "panes" "frames" "pointer-tracking"
						  "presentations"))
     (:file "Goatee/presentation-history" :depends-on ("presentation-defs")  ; XXX: this is loaded as part of the Goatee system. huh?
            :pathname #.(make-pathname :directory '(:relative "Goatee") :name "presentation-history" :type "lisp"))
     ))

(defsystem :clim-clx
    :depends-on (:clim #+(or sbcl openmcl ecl allegro) :clx)
    :components
    ((:module "Backends/CLX"
              :pathname #.(make-pathname :directory '(:relative "Backends" "CLX"))
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

(defsystem :clim-null
    :depends-on (:clim)
    :components
    ((:module "Backends/Null"
	      :pathname #.(make-pathname :directory '(:relative "Backends" "Null"))
	      :components
	      ((:file "package")
	       (:file "port" :depends-on ("package"))
	       (:file "medium" :depends-on ("port" "package"))
	       (:file "graft" :depends-on ("port" "package"))
	       (:file "frame-manager" :depends-on ("medium" "port" "package"))))))

(defsystem :clim-gtkairo
    :depends-on (:clim :cffi)
    :components
    ((:module "Backends/gtkairo"
	      :pathname #.(make-pathname :directory '(:relative "Backends" "gtkairo"))
	      :serial t			;asf wird's ja richten
	      :components
	      ((:file "clim-fix")
	       (:file "package")
	       (:file "gtk-ffi")
	       (:file "cairo-ffi")
	       (:file "port")
	       (:file "event")
	       (:file "keysymdef")
	       (:file "medium")
	       (:file "pixmap")
	       (:file "graft")
	       (:file "frame-manager")
	       (:file "gadgets")))))

;;; TODO/asf: I don't have the required libs to get :clim-opengl to load. tough.
(clim-defsystem (:clim-opengl :depends-on (:clim))
   "Backends/OpenGL/opengl-x-frame-manager"
   "Backends/OpenGL/opengl-frame-manager"
   "Backends/OpenGL/opengl-x-port-before"
   "Backends/OpenGL/opengl-port"
   "Backends/OpenGL/opengl-x-port-after"
   "Backends/OpenGL/opengl-medium"
   "Backends/OpenGL/opengl-x-graft")

;;; A system that loads the appropriate backend for the current
;;; platform.
(defsystem :clim-looks
    :depends-on (:clim :clim-postscript
                 ;; If we're on an implementation that ships CLX, use
                 ;; it. Same if the user has loaded CLX already.
                 #+(or sbcl scl openmcl ecl clx allegro) :clim-clx
                 #+gl                        :clim-opengl
                 ;; OpenMCL and MCL support the beagle backend (native
                 ;; OS X look&feel on OS X).

                 ;; But until it's ready, it's no use forcing users to
                 ;; cope with possible bugs.
                 ;; #+(or openmcl mcl)          :clim-beagle

		 #+gtkairo :clim-gtkairo

		 ;; null backend
		 :clim-null
                 )
    :components ((:file "Looks/pixie"
                        :pathname #.(make-pathname :directory '(:relative "Looks") :name "pixie" :type "lisp"))))

;;; name of :clim-clx-user chosen by mikemac for no good reason
(defsystem :clim-clx-user
    :depends-on (:clim :clim-clx))

;;; The actual McCLIM system that people should to use in their ASDF
;;; package dependency lists.
(defsystem :mcclim
    :version "0.9.3-dev"
    :depends-on (:clim-looks))

;;; CLIM-Examples depends on having at least one backend loaded.
(defsystem :clim-examples
    :depends-on (:mcclim)
    :components
    ((:module "Examples"
              :components
              ((:file "calculator")
               (:file "colorslider")
	       (:file "menutest") ; extra
               (:file "address-book")
               (:file "traffic-lights")
               (:file "clim-fig")
               (:file "postscript-test")
               (:file "puzzle")
               (:file "transformations-test")
	       (:file "demodemo")
               (:file "stream-test")
               (:file "presentation-test")
               (:file "dragndrop")
	       (:file "gadget-test")
               (:file "accepting-values")
               (:file "method-browser")
	       (:file "stopwatch")
	       (:file "dragndrop-translator")
               (:file "draggable-graph")
               (:file "text-size-test")
               (:file "drawing-benchmark")
               (:file "logic-cube")
               (:file "views")))
     (:module "Goatee"
	      :components
	      ((:file "goatee-test")))))

;;; This won't load in SBCL, either. I have really crappy code to
;;; extract dependency information from :serial t ASDF systems, but
;;; this comment is too narrow to contain it.
(clim-defsystem (:scigraph :depends-on (:mcclim))
  ;; The DWIM part of SCIGRAPH		
  "Apps/Scigraph/dwim/package"
  "Apps/Scigraph/dwim/feature-case"
  "Apps/Scigraph/dwim/macros"
  "Apps/Scigraph/dwim/tv"
  "Apps/Scigraph/dwim/draw"
  "Apps/Scigraph/dwim/present"
  "Apps/Scigraph/dwim/extensions"
  "Apps/Scigraph/dwim/wholine"
  "Apps/Scigraph/dwim/export"
  ;; The Scigraph part
  "Apps/Scigraph/scigraph/package" 
  "Apps/Scigraph/scigraph/copy"
  "Apps/Scigraph/scigraph/dump"
  "Apps/Scigraph/scigraph/duplicate"
  "Apps/Scigraph/scigraph/random"
  "Apps/Scigraph/scigraph/menu-tools"
  "Apps/Scigraph/scigraph/basic-classes"
  "Apps/Scigraph/scigraph/draw"
  "Apps/Scigraph/scigraph/mouse"
  "Apps/Scigraph/scigraph/color"
  "Apps/Scigraph/scigraph/basic-graph"
  "Apps/Scigraph/scigraph/graph-mixins"
  "Apps/Scigraph/scigraph/axis"
  "Apps/Scigraph/scigraph/moving-object"
  "Apps/Scigraph/scigraph/symbol"
  "Apps/Scigraph/scigraph/graph-data"
  "Apps/Scigraph/scigraph/legend"
  "Apps/Scigraph/scigraph/graph-classes"
  "Apps/Scigraph/scigraph/present"
  "Apps/Scigraph/scigraph/annotations"
  "Apps/Scigraph/scigraph/annotated-graph"
  "Apps/Scigraph/scigraph/contour"
  "Apps/Scigraph/scigraph/equation"
  "Apps/Scigraph/scigraph/popup-accept"
  "Apps/Scigraph/scigraph/popup-accept-methods"
  "Apps/Scigraph/scigraph/duplicate-methods"
  "Apps/Scigraph/scigraph/frame"
  "Apps/Scigraph/scigraph/export"
  "Apps/Scigraph/scigraph/demo-frame")

(defsystem :clim-listener
    :depends-on (:mcclim #+sbcl :sb-posix)
    :components
    ((:file "Experimental/xpm"
            :pathname #.(make-pathname :directory '(:relative "Experimental") :name "xpm" :type "lisp"))
     (:module "Apps/Listener"
              :pathname #.(make-pathname :directory '(:relative "Apps" "Listener"))
              :depends-on ("Experimental/xpm")
              :components
              ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "icons" :depends-on ("package" "util"))
               (:file "file-types" :depends-on ("package" "icons" "util"))
               (:file "dev-commands" :depends-on ("package" "icons" "file-types" "util"))
               (:file "listener" :depends-on ("package" "file-types" "icons" "dev-commands" "util"))
               #+CMU (:file "cmu-hacks" :depends-on ("package"))))))

(defsystem :clouseau
    :depends-on (:mcclim)
    :serial t
    :components
    ((:module "Apps/Inspector"
              :pathname #.(make-pathname :directory '(:relative "Apps" "Inspector"))
              :components
	      ((:file "package")
	       (:file "disassembly")
	       (:file "inspector")))))

(defmethod perform :after ((op load-op) (c (eql (find-system :clim))))
  (pushnew :clim *features*)
  (pushnew :mcclim *features*))

(defmethod perform :after ((op load-op) (c (eql (find-system :mcclim))))
  (pushnew :clim *features*)
  (pushnew :mcclim *features*))
