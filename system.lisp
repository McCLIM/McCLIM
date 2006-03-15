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

(defparameter *clim-directory* (directory-namestring *load-truename*))

#+cmu
(progn
  (unless (fboundp 'ext:stream-read-char)
    (unless (ignore-errors (ext:search-list "gray-streams:"))
      (setf (ext:search-list "gray-streams:")
	'("target:pcl/" "library:subsystems/")))
    (if (fboundp 'without-package-locks)
	(without-package-locks
	 (load "gray-streams:gray-streams-library"))
      (load "gray-streams:gray-streams-library")))
  #-clx
  (require :clx)
  #-(or mk-defsystem asdf)
  (load "library:subsystems/defsystem")
  #+mp (when (eq mp::*initial-process* mp::*current-process*)
	 (format t "~%~%You need to run (mp::startup-idle-and-top-level-loops) to start up the multiprocessing support.~%~%")))

(pushnew :clim *features*)
(pushnew :mcclim *features*)

+;;; I really didn't have good luck with this on Allegro, because
+;;; Allegro's CL-USER package uses it's EXCL stuff, which has its own
+;;; DEFSYSTEM. [2004/12/21:rpg]
+;;;#+mk-defsystem (use-package "MK")

(defmacro clim-defsystem ((module &key depends-on) &rest components)
  `(progn
     #+mk-defsystem
     (mk:defsystem ,module
       :source-pathname *clim-directory*
       :source-extension "lisp"
       ,@(and depends-on `(:depends-on ,depends-on))
        :components
	(:serial
	 ,@components))
     #+asdf
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
		  collect `(:file ,(pathname-name p) :pathname ,p))))
     #-(or mk-defsystem asdf)
     (defsystem ,module ()
       (:serial
	,@depends-on
	,@components))))

(clim-defsystem (:clim-lisp)
  ;; First possible patches
  "patch"
  #+cmu       "Lisp-Dep/fix-cmu"
  #+excl      "Lisp-Dep/fix-acl"
  #+sbcl      "Lisp-Dep/fix-sbcl"
  #+openmcl   "Lisp-Dep/fix-openmcl"
  #+lispworks "Lisp-Dep/fix-lispworks"
  #+clisp     "Lisp-Dep/fix-clisp"
  "package")

(clim-defsystem (:clim-core :depends-on (:clim-lisp))
   "decls"
   "protocol-classes"

   #.(or
      #+(and :cmu :mp (not :pthread))  "Lisp-Dep/mp-cmu"

      ;; Rumor is that SB-THREAD is a feature test for the presence of
      ;; multithreading in SBCL.

      #+sb-thread               "Lisp-Dep/mp-sbcl"
      #+excl                    "Lisp-Dep/mp-acl"
      #+openmcl                 "Lisp-Dep/mp-openmcl"
      #+lispworks               "Lisp-Dep/mp-lw"
      #| fall back |#           "Lisp-Dep/mp-nil")
   "utils"
   "defresource"
   "setf-star"
   
   "design"
   "X11-colors"
   "coordinates"
   "transforms"
   "regions"

   "sheets"
   "pixmap"
   
   "events"

   "ports" ; depends on events
   "grafts"
   "medium"
   "output"

   "input"
   "repaint"
   "graphics"
   "views"
   "stream-output"
   "recording"
   "encapsulate"
   "stream-input"			; depends on WITH-ENCAPSULATING-STREAM
   "text-selection"
)

(clim-defsystem (:goatee-core :depends-on (:clim-core))
  "Goatee/conditions"
  "Goatee/dbl-list"
  "Goatee/flexivector"
  "Goatee/buffer"
  "Goatee/editable-buffer"
  "Goatee/editable-area"
  "Goatee/clim-area"
  "Goatee/kill-ring"
  "Goatee/goatee-command"
  "Goatee/editing-stream"
  "Goatee/presentation-history"
  )

;;; CLIM-PostScript is not a backend in the normal sense.
;;; It is an extension (Chap. 35.1 of the spec) and is an
;;; "included" part of McCLIM. Hence the defsystem is here.
(clim-defsystem (:clim-postscript :depends-on (:clim-core))
   "Backends/PostScript/package"
   "Backends/PostScript/encoding"
   "Backends/PostScript/paper"
   "Backends/PostScript/class"
   "Backends/PostScript/font"
   "Backends/PostScript/graphics"
   "Backends/PostScript/sheet"
   "Backends/PostScript/afm"
   "Backends/PostScript/standard-metrics"
   )

(clim-defsystem (:clim :depends-on (:clim-core :goatee-core :clim-postscript))
   "text-formatting"
   "input-editing"
   "presentations"
   "presentation-defs"
   "pointer-tracking" ; depends on WITH-INPUT-CONTEXT
   "commands"
   "frames"
   "incremental-redisplay"
   "panes"
   "gadgets"
   "menu"
   "table-formatting"
   "graph-formatting"
   "bordered-output"
   "dialog-views"
   "dialog" ; depends on table formatting
   "builtin-commands" ; need dialog before commands are defined
   "describe"
   "menu-choose" ; depends on table formatting, presentations
   "Goatee/presentation-history"
   )

(load (merge-pathnames "Backends/CLX/system" *clim-directory*))
#+gl(load (merge-pathnames "Backends/OpenGL/system" *clim-directory*))

(clim-defsystem (:clim-looks :depends-on (#+clx :clim-clx #+gl :clim-opengl))
  "Looks/pixie")

;;; Will depend on :goatee soon...
;;; name of :clim-clx-user chosen by mikemac for no good reason
(clim-defsystem (:clim-clx-user :depends-on (:clim :clim-clx)))

;;; CLIM-Examples depends on having at least one backend loaded.
;;; Which backend is the user's choice.
(clim-defsystem (:clim-examples :depends-on (:clim #+clx :clim-looks))
   "Examples/calculator"
   "Examples/colorslider"
   "Examples/menutest"
   "Examples/address-book"
   "Examples/traffic-lights"
   "Examples/clim-fig"
   "Examples/postscript-test"
   "Examples/puzzle"
   "Examples/transformations-test"
   ;; "Examples/sliderdemo"
   "Examples/demodemo"
   "Examples/stream-test"
   "Examples/presentation-test"
   "Examples/dragndrop"
   "Examples/gadget-test"
   "Examples/method-browser"
   "Examples/dragndrop-translator"
   "Goatee/goatee-test"
   "Examples/accepting-values")

;;; The DWIM part of SCIGRAPH		
(clim-defsystem (:scigraph-dwim :depends-on (:clim #+clx :clim-looks))
  "Apps/Scigraph/dwim/package"
  "Apps/Scigraph/dwim/feature-case"
  "Apps/Scigraph/dwim/macros"
  "Apps/Scigraph/dwim/tv"
  "Apps/Scigraph/dwim/draw"
  "Apps/Scigraph/dwim/present"
  "Apps/Scigraph/dwim/extensions"
  "Apps/Scigraph/dwim/wholine"
  "Apps/Scigraph/dwim/export")

;;; The Scigraph part
(clim-defsystem (:scigraph :depends-on (:scigraph-dwim))
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

(clim-defsystem (:clim-listener :depends-on (:clim #+clx :clim-looks #+sbcl :sb-posix))
  "Experimental/xpm"
  "Apps/Listener/package"
  "Apps/Listener/hotfixes"
  "Apps/Listener/util"
  "Apps/Listener/icons.lisp"
  "Apps/Listener/file-types"
  "Apps/Listener/dev-commands"
  "Apps/Listener/listener"
  #+CMU "Apps/Listener/cmu-hacks")
