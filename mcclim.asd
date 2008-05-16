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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-swank-package ()
    (find-package :swank))
  (defun find-swank-system ()
    (handler-case (asdf:find-system :swank)
      (asdf:missing-component ())))
  (defun find-swank ()
    (or (find-swank-package)
        (find-swank-system)))
  (defun dep-on-swank ()
    (if (and (find-swank-system)
             (not (find-package :swank)))
        '(:and)
        '(:or)))
  (defun ifswank ()
    (if (find-swank)
        '(:and)
        '(:or))))

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

;;; Clozure CL native GUI stuff
#+clim-beagle
(require :cocoa)

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

(defsystem :clim-basic
    :depends-on (:clim-lisp :spatial-trees (:version "flexichain" "1.5.1"))
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
                 (:file "dead-keys" :depends-on ("decls"))
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
                                                            "encapsulate" "transforms" "utils" "dead-keys"))
                 (:file "text-selection" :depends-on ("decls" "protocol-classes" "Lisp-Dep" "X11-colors" "medium" "output"
                                                                   "transforms" "sheets" "stream-output"
                                                                   "ports" "recording" "regions"
                                                                   "events"))
		 (:file "bezier" :depends-on ("recording"))))

(defsystem :goatee-core
    :depends-on (:clim-basic)
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
  :depends-on (:clim-basic)
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

(defsystem :clim-core
  :depends-on (:clim-basic :goatee-core :clim-postscript)
  :components ((:file "text-formatting")
               (:file "defresource")
               (:file "presentations")
               (:file "xpm")
               (:file "bordered-output" :depends-on ("presentations"))
               (:file "table-formatting" :depends-on ("presentations"))
               (:file "input-editing" :depends-on ("presentations" "bordered-output" "table-formatting"))
               (:file "pointer-tracking" :depends-on ("input-editing"))
               (:file "graph-formatting")
               (:file "frames" :depends-on ("commands" "presentations" "presentation-defs"
                                                       "pointer-tracking" "incremental-redisplay"))
               (:file "dialog-views" :depends-on ("presentations" "incremental-redisplay"
                                                                  "bordered-output" "presentation-defs"))
               (:file "presentation-defs" :depends-on ("input-editing" "presentations"))
               (:file "gadgets" :depends-on ("commands" "pointer-tracking" "input-editing" 
                                                        "frames" "incremental-redisplay" "panes"))
               (:file "describe" :depends-on ("presentations" "presentation-defs" "table-formatting"))
               (:file "commands" :depends-on ("input-editing" "presentations"
                                                              "presentation-defs"))
               (:file "incremental-redisplay" :depends-on ("presentation-defs"))
               (:file "menu-choose" :depends-on ("commands" "table-formatting" "presentation-defs"
                                                            "panes" "frames" "pointer-tracking"
                                                            "presentations"))
               (:file "menu" :depends-on ("panes" "commands" "gadgets"
                                                  "presentations" "frames"))
               (:file "panes" :depends-on ("incremental-redisplay" "presentations"
                                                                   "presentation-defs" "input-editing" "frames"))
               (:file "dialog" :depends-on ("panes" "frames" "incremental-redisplay"
                                                    "table-formatting" "presentations"
                                                    "bordered-output" "presentation-defs"
                                                    "dialog-views" "input-editing"
                                                    "commands"))
               (:file "builtin-commands" :depends-on ("table-formatting"
                                                      "commands" "presentations"
                                                      "dialog" "presentation-defs"
                                                      "input-editing"))))

(defsystem :esa-mcclim
  :depends-on (:clim-core)
  :components ((:module "ESA"
                        :components ((:file "packages")
                                     (:file "utils" :depends-on ("packages"))
                                     (:file "colors" :depends-on ("packages"))
                                     (:file "esa" :depends-on ("colors" "packages" "utils"))
                                     (:file "esa-buffer" :depends-on ("packages" "esa"))
                                     (:file "esa-io" :depends-on ("packages" "esa" "esa-buffer"))
                                     (:file "esa-command-parser" :depends-on ("packages" "esa"))))))



(defsystem :drei-mcclim
  :depends-on ((:version "flexichain" "1.5.1") :esa-mcclim :clim-core #+#.(mcclim.system::dep-on-swank) :swank)
  :components
  ((:module "cl-automaton"
            :pathname #.(make-pathname :directory '(:relative "Drei" "cl-automaton"))
	    :components ((:file "automaton-package")
			 (:file "eqv-hash" :depends-on ("automaton-package"))
			 (:file "state-and-transition" :depends-on ("eqv-hash"))
			 (:file "automaton" :depends-on ("state-and-transition" "eqv-hash"))
			 (:file "regexp" :depends-on ("automaton"))))
   (:module "Persistent"
            :pathname #.(make-pathname :directory '(:relative "Drei" "Persistent"))
            :components ((:file "binseq-package")
                         (:file "binseq" :depends-on ("binseq-package"))
                         (:file "obinseq" :depends-on ("binseq-package" "binseq"))
                         (:file "binseq2" :depends-on ("binseq-package" "obinseq" "binseq"))))
   (:module "Drei" :depends-on ("cl-automaton" "Persistent")
            :components ((:file "packages")
                         (:file "buffer" :depends-on ("packages"))
                         (:file "delegating-buffer" :depends-on ("packages" "buffer"))
                         (:file "motion" :depends-on ("packages" "buffer" "syntax"))
                         (:file "editing" :depends-on ("packages" "buffer" "syntax" "motion" "kill-ring"))
                         (:file "base" :depends-on ("packages" "buffer" "persistent-buffer" "kill-ring"
                                                               "delegating-buffer"))
                         (:file "syntax" :depends-on ("packages" "buffer" "base"))
                         (:file "modes" :depends-on ("packages" "syntax"))
                         (:file "views" :depends-on ("packages" "buffer" "base" "syntax" "persistent-undo"
                                                                "persistent-buffer" "undo" "abbrev"
                                                                "delegating-buffer" "modes"))
                         (:file "drei" :depends-on ("packages" "views" "motion" "editing"))
                         (:file "drei-clim" :depends-on ("drei"))
                         (:file "drei-redisplay" :depends-on ("drei-clim"))
                         (:file "drawing-options" :depends-on ("drei-redisplay"))
                         (:file "input-editor" :depends-on ("drei-redisplay" "lisp-syntax" "core"))
                         (:file "abbrev" :depends-on ("packages"))
                         (:file "kill-ring" :depends-on ("packages"))
                         (:file "undo" :depends-on ("packages"))
                         (:file "basic-commands" :depends-on ("drei-clim" "motion" "editing"))
                         (:file "core" :depends-on ("drei"))
                         (:file "fundamental-syntax" :depends-on ("packages" "drei-redisplay" "core"))
                         (:file "buffer-streams" :depends-on ("core"))
                         (:file "rectangle" :depends-on ("core"))
                         (:file "targets" :depends-on ("core"))
                         (:file "core-commands" :depends-on ("core" "rectangle" "drei-clim"))
                         (:file "persistent-buffer"
                                :pathname #.(make-pathname :directory '(:relative "Persistent")
                                                           :name "persistent-buffer"
                                                           :type "lisp")
                                :depends-on ("packages"))
                         (:file "persistent-undo"
                                :pathname #p"Persistent/persistent-undo.lisp"
                                :depends-on ("packages" "buffer" "persistent-buffer" "undo"))
                         (:file "misc-commands" :depends-on ("basic-commands"))
                         (:file "search-commands" :depends-on ("core" "targets" "drei-clim"))
                         (:file "lr-syntax" :depends-on ("fundamental-syntax" "core" "drawing-options"))
                         (:file "lisp-syntax" :depends-on ("lr-syntax" "motion" "core"))
                         (:file "lisp-syntax-swine" :depends-on ("lisp-syntax"))
                         (:file "lisp-syntax-commands" :depends-on ("lisp-syntax-swine" "misc-commands"))
                         #+#.(mcclim.system::ifswank) (:file "lisp-syntax-swank" :depends-on ("lisp-syntax"))))))

(defsystem :drei-tests
  :depends-on (:drei-mcclim :fiveam)
  :components
  ((:module "Tests"
            :pathname #.(make-pathname :directory '(:relative "Drei" "Tests"))
            :components 
            ((:module
              "cl-automaton"
              :depends-on ("testing")
              :components
              ((:file "automaton-tests")
               (:file "state-and-transition-tests" :depends-on ("automaton-tests"))
               (:file "eqv-hash-tests" :depends-on ("automaton-tests"))
               (:file "regexp-tests" :depends-on ("automaton-tests"))))
             (:file "packages")
             (:file "testing" :depends-on ("packages"))
             (:file "buffer-tests" :depends-on ("testing"))
             (:file "base-tests" :depends-on ("testing"))
             (:file "kill-ring-tests" :depends-on ("testing"))
             (:file "motion-tests" :depends-on ("testing"))
             (:file "editing-tests" :depends-on ("testing"))
             (:file "core-tests" :depends-on ("testing"))
             (:file "buffer-streams-tests" :depends-on ("testing"))
             (:file "rectangle-tests" :depends-on ("testing"))
             (:file "undo-tests" :depends-on ("testing"))
             (:file "lisp-syntax-tests" :depends-on ("testing" "motion-tests"))
             (:file "lisp-syntax-swine-tests" :depends-on ("lisp-syntax-tests"))))))

(defsystem :clim
  :depends-on (:clim-core :goatee-core :clim-postscript :drei-mcclim)
  :components
  ((:file "Goatee/presentation-history" ; XXX: this is loaded as part of the Goatee system. huh?
          :pathname #.(make-pathname :directory '(:relative "Goatee") :name "presentation-history" :type "lisp"))
   (:file "input-editing-goatee")
   (:file "input-editing-drei")
   (:file "text-editor-gadget")
   (:file "Extensions/tab-layout"
	  :pathname #.(make-pathname :directory '(:relative "Extensions")
				     :name "tab-layout"))))

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
#+clisp
(defmethod asdf::traverse :around ((op compile-op) (c (eql (find-system :clim-clx))))
  ;; Just some random symbol I know is unexported in CLISP's CLX.
  (if (eq (nth-value 1 (find-symbol "SET-SELECTION-OWNER" :xlib))
       :external)
      (call-next-method)
      (restart-case (error "Your CLX is not capable of running the McCLIM CLX backend")
        (load-clx-via-asdf ()
         :report "Try replacing your CLX with a CLX loaded through ASDF, hopefully this will be Telent CLX."
         (ext:without-package-lock ("XLIB")
           (delete-package :xlib)
           (asdf:oos 'asdf:load-op :clx))
         (call-next-method)))))

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
                             (:file "graft-tests"))))))))
)

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
	       (:file "ffi")
	       (:file "graft")
	       (:file "port")
	       (:file "event")
	       (:file "keys")
	       (:file "medium")
	       (:file "pango")
	       (:file "cairo")
	       (:file "gdk")
	       (:file "pixmap")
	       (:file "frame-manager")
	       (:file "gadgets")))))

(defsystem :clim-graphic-forms
    :depends-on (:clim :graphic-forms-uitoolkit)
    :components
    ((:module "Backends/Graphic-Forms"
	      :pathname #.(make-pathname :directory '(:relative "Backends" "Graphic-Forms"))
	      :components
	      ((:file "package")
         (:file "utils" :depends-on ("package"))
	       (:file "graft" :depends-on ("package"))
	       (:file "port" :depends-on ("utils" "graft"))
	       (:file "medium" :depends-on ("port"))
         (:file "pixmap" :depends-on ("medium"))
	       (:file "frame-manager" :depends-on ("medium"))
         (:file "gadgets" :depends-on ("port"))))))

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
		 (:file "Looks/pixie"
                        :pathname #.(make-pathname :directory '(:relative "Looks") :name "pixie" :type "lisp"))))

;;; The actual McCLIM system that people should to use in their ASDF
;;; package dependency lists.
(defsystem :mcclim
    :version "0.9.7-dev"
    :depends-on (:clim-looks))

(defmethod perform :after ((op load-op) (c (eql (find-system :clim))))
  (pushnew :clim *features*)
  (pushnew :mcclim *features*))

(defmethod perform :after ((op load-op) (c (eql (find-system :mcclim))))
  (pushnew :clim *features*)
  (pushnew :mcclim *features*))

;; XXX This is very ugly, but ESA and Drei need to know whether they
;; are being compiled as part of McCLIM, or in another CLIM
;; implementation.
(defmethod perform :around (op c)
  (if (and (or (eql (component-system c) (find-system :esa-mcclim))
               (eql (component-system c) (find-system :drei-mcclim)))
           (not (find :building-mcclim *features*)))
      (unwind-protect (progn (push :building-mcclim *features*)
                             (call-next-method))
        (setf *features* (delete :building-mcclim *features*)))
      (call-next-method)))
