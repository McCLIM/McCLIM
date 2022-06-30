(in-package #:asdf-user)

(defsystem "clim-core"
  :depends-on ("clim-basic" "spatial-trees" (:feature :sbcl "sb-introspect"))
  :pathname "clim-core"
  :components
  ((:file "utilities")
   (:file "theming")
   (:module "extended-streams"
    :components ((:file "text-formatting") ; standard-page-layout
                 (:file "views")           ; stream-default-view
                 (:file "dead-keys")       ; dead-key merging
                 (:file "stream-output"     :depends-on ("text-formatting" "views"))
                 (:file "recording"         :depends-on ("stream-output"))
                 (:file "text-selection"    :depends-on ("recording"))
                 (:file "encapsulate"       :depends-on ("stream-output" "recording"))
                 (:file "stream-input"      :depends-on ("encapsulate" "dead-keys"))
                 (:file "gestures")
                 (:file "standard-gestures" :depends-on ("gestures"))
                 (:file "pointer-tracking"  :depends-on ("stream-output" "stream-input"))))
   (:module "incremental-redisplay"
    :serial t
    :components ((:file "cache")
                 (:file "updating-stream")
                 (:file "updating-record")
                 (:file "redisplay")
                 (:file "propagate")))
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
   (:module "formatting"
    :depends-on ("presentations" "theming")
    :components ((:file "bordered-output")
                 (:file "table-formatting")
                 (:file "graph-formatting")))
   (:file "input-editing" :depends-on ("presentations" "formatting"))
   (:file "standard-presentations"
    :pathname "presentations/standard-presentations"
    :depends-on ("input-editing" "presentations"))
   (:file "describe" :depends-on ("presentations" "standard-presentations" "formatting"))
   (:module "commands"
    :depends-on ("input-editing" "presentations" "standard-presentations")
    :serial t
    :components ((:file "parsers")
                 (:file "commands")
                 (:file "tables")
                 (:file "processor")))
   (:module "panes"
    :depends-on ("incremental-redisplay" "presentations" "standard-presentations" "input-editing" "theming")
    :serial t
    :components ((:file "construction")
                 (:file "layout-protocol")
                 (:file "composition")
                 (:file "stream-panes")))
   (:module "frames"
    :depends-on ("commands" "presentations" "standard-presentations" "incremental-redisplay" "panes")
    :serial t
    :components ((:file "frames")
                 (:file "pointer-documentation")
                 (:file "menu-frame")
                 (:file "frame-managers")
                 (:file "define-application-frame")
                 (:file "window-stream")
                 (:file "default-frame")))
   (:module "gadgets"
    :depends-on ("commands" "input-editing" "frames" "incremental-redisplay" "panes" "presentations" "theming")
    :serial t
    :components ((:file "base")
                 (:file "abstract")
                 (:file "mixins")
                 (:file "drawing-utilities")
                 (:file "concrete")
                 (:file "menu")))
   (:module "dialogs"
    :depends-on ("panes" "frames" "input-editing" "commands" "presentations" "incremental-redisplay" "formatting" "standard-presentations" "gadgets")
    :components ((:file "dialog")
                 (:file "dialog-views" :depends-on ("dialog"))
                 (:file "notify-user")
                 (:file "menu-choose")))
   (:file "builtin-commands" :depends-on ("formatting" "commands" "presentations" "standard-presentations" "input-editing"))))
