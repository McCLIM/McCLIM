(in-package #:asdf-user)

(defsystem "clim-core"
  :depends-on ("clim-core/system"
               "clim-core/silex"
               "clim-core/frames"))

;;; TODO split the file "protocol" for each module separately.
(defsystem "clim-core/system"
  :depends-on ("bordeaux-threads" "trivial-gray-streams" "trivial-features" "closer-mop")
  :components ((:module "system"
                :components
                ((:file "patch")    ;first possible patches
                 (:file "fix-acl"   :if-feature :excl)
                 (:file "fix-clisp" :if-feature :clisp)
                 (:file "packages")
                 (:file "multiprocessing")
                 (:file "setf-star")))
               (:module "utilities"
                :components
                ((:file "utilities")
                 (:file "protocol")
                 (:file "resources")
                 (:file "encapsulating-streams" :depends-on ("protocol"))))
               (:module "geometry"
                :depends-on ("system")
                :serial t
                :components ((:file "coordinates")
                             (:file "transforms")
                             (:file "bounding-rectangle")
                             (:file "regions")
                             (:file "3.2.1-points")
                             (:file "3.2.2-polygons-and-polylines")
                             (:file "3.2.3-lines")
                             (:file "3.2.4-rectangles")
                             (:file "3.2.5-ellipses-and-elliptical-arcs")
                             (:file "3.2.6-bezigons-and-bezier-curves")
                             (:file "region-utilities")
                             (:file "region-predicates")
                             (:file "region-composition")
                             (:file "region-set-composition")))))

(defsystem "clim-core/silex"
  :depends-on ("clim-core/system" "spatial-trees" "trivial-garbage")
  :components
  ((:module "drawing"
    :components ((:file "design")
                 (:file "text-style")
                 (:file "colors"   :depends-on ("design"))
                 (:file "pattern"  :depends-on ("design"))
                 (:file "medium"   :depends-on ("design" "text-style" "colors"))
                 (:file "graphics" :depends-on ("design" "text-style" "medium"))))
   (:module "windowing"
    :depends-on ("drawing")
    :components ((:file "standard-keys")
                 (:file "events")
                 (:file "output")
                 (:file "pointer")
                 (:file "sheets")
                 (:file "pixmaps" :depends-on ("output"))
                 (:file "mirrors" :depends-on ("sheets"))
                 (:file "ports"   :depends-on ("sheets" "events"))
                 (:file "fonts"   :depends-on ("ports"))
                 (:file "input"   :depends-on ("sheets" "ports"))
                 (:file "grafts"  :depends-on ("sheets" "ports"))
                 (:file "repaint" :depends-on ("sheets" "ports" "grafts" "events" "output"))
                 (:file "with-output-to-drawing-stream" :depends-on ("ports"))))
   (:module "extended-output"
    :depends-on ("drawing")
    :components ((:file "protocol")
                 (:file "extra-colors")
                 (:file "utilities"        :depends-on ("protocol"))
                 (:file "views"            :depends-on ("protocol"))
                 (:file "text-formatting"  :depends-on ("protocol" "utilities"))
                 (:file "text-cursor"      :depends-on ("protocol"))
                 (:file "stream-output"    :depends-on ("protocol" "utilities" "views" "text-formatting" "text-cursor"))
                 (:file "recording"        :depends-on ("protocol" "text-formatting"))
                 (:file "graph-formatting" :depends-on ("protocol" "stream-output" "recording"))
                 (:file "bordered-output"  :depends-on ("utilities" "extra-colors" "protocol" "recording"))
                 (:file "table-formatting" :depends-on ("utilities" "protocol" "recording"))
                 (:module "incremental-redisplay"
                  :serial t
                  :components ((:file "cache")
                               (:file "updating-stream")
                               (:file "updating-record")
                               (:file "redisplay")
                               (:file "propagate")))))
   (:module "extended-input"
    :depends-on ("windowing" "extended-output")
    :components ((:file "protocol")
                 (:file "gestures")
                 (:file "standard-gestures" :depends-on ("gestures"))
                 (:file "dead-keys")    ; dead-key merging
                 (:file "stream-input"      :depends-on ("dead-keys" "standard-gestures"))
                 (:file "pointer-tracking"  :depends-on ("stream-input"))))))

;;; TODO separate modules, move directories toplevel aftwards.
(defsystem "clim-core/frames"
  :depends-on ("clim-core/silex" "cluffer" (:feature :sbcl "sb-introspect"))
  :pathname "clim-core"
  :components
  ((:module "system"
    :components ((:file "protocol-classes")
                 (:file "declarations")
                 (:file "utilities")
                 (:file "describe")
                 (:file "encapsulate")))
   (:module "interactive-streams"
    :components ((:file "input-editing-stream")
                 (:file "parsing-conditions")
                 (:file "reading-and-writing-tokens")))
   (:module "presentations"
    :serial t
    :components ((:file "presentation-types")
                 (:file "presentation-functions")
                 (:file "presentation-type-functions")
                 (:file "presentation-histories")
                 (:file "typed-output")
                 (:file "typed-input")
                 (:file "translators")
                 (:file "drag-and-drop")
                 (:file "selection-object")))
   (:file "text-selection" :depends-on ("presentations"))
   (:file "completion" :depends-on ("presentations"))
   (:file "input-editing" :depends-on ("presentations" "completion"))
   (:file "standard-presentations"
    :pathname "presentations/standard-presentations"
    :depends-on ("presentations"))
   (:module "commands"
    :depends-on ("presentations" "standard-presentations")
    :serial t
    :components ((:file "parsers")
                 (:file "commands")
                 (:file "tables")
                 (:file "processor")))
   (:module "panes"
    :depends-on ("presentations" "standard-presentations")
    :serial t
    :components ((:file "theming")
                 (:file "construction")
                 (:file "layout-protocol")
                 (:file "composition")
                 (:file "stream-panes")))
   (:module "frames"
    :depends-on ("commands" "presentations" "standard-presentations" "panes")
    :serial t
    :components ((:file "frames")
                 (:file "pointer-documentation")
                 (:file "menu-frame")
                 (:file "frame-managers")
                 (:file "define-application-frame")
                 (:file "window-stream")
                 (:file "default-frame")))
   (:module "gadgets"
    :depends-on ("commands" "frames" "panes" "presentations")
    :serial t
    :components ((:file "base")
                 (:file "abstract")
                 (:file "mixins")
                 (:file "drawing-utilities")
                 (:file "concrete")
                 (:file "menu")))
   (:module "dialogs"
    :depends-on ("panes" "frames" "commands" "presentations" "standard-presentations" "gadgets")
    :components ((:file "dialog")
                 (:file "dialog-views" :depends-on ("dialog"))
                 (:file "notify-user")
                 (:file "menu-choose")))
   (:file "builtin-commands" :depends-on ("commands" "presentations" "standard-presentations"))
   (:module "input-editor"
    :components ((:file "editor")
                 (:file "edward")
                 ;(:file "text-editing-gadget")
                 ))))
