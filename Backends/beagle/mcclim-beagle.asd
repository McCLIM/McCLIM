
(defsystem #:mcclim-beagle
  :depends-on ((:require :cocoa) #:clim)
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
                 (:file "graft-tests")))))
