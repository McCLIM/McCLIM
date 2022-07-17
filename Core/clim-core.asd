(in-package #:asdf-user)

(defsystem "clim-core"
  :depends-on ("clim-basic" (:feature :sbcl "sb-introspect"))
  :pathname "clim-core"
  :components
  ((:module "system"
    :components ((:file "protocol-classes")
                 (:file "declarations")
                 (:file "utilities")
                 (:file "theming")))
   (:module "extended-streams"
    :components ((:file "text-selection")
                 (:file "encapsulate")
                 (:file "dead-keys")       ; dead-key merging
                 (:file "stream-input"      :depends-on ("dead-keys"))
                 (:file "gestures")
                 (:file "standard-gestures" :depends-on ("gestures"))
                 (:file "pointer-tracking"  :depends-on ("stream-input"))))
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
   (:file "input-editing" :depends-on ("presentations"))
   (:file "standard-presentations"
    :pathname "presentations/standard-presentations"
    :depends-on ("input-editing" "presentations"))
   (:file "describe" :depends-on ("presentations" "standard-presentations"))
   (:module "commands"
    :depends-on ("input-editing" "presentations" "standard-presentations")
    :serial t
    :components ((:file "parsers")
                 (:file "commands")
                 (:file "tables")
                 (:file "processor")))
   (:module "panes"
    :depends-on ("presentations" "standard-presentations" "input-editing")
    :serial t
    :components ((:file "construction")
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
    :depends-on ("commands" "input-editing" "frames" "panes" "presentations")
    :serial t
    :components ((:file "base")
                 (:file "abstract")
                 (:file "mixins")
                 (:file "drawing-utilities")
                 (:file "concrete")
                 (:file "menu")))
   (:module "dialogs"
    :depends-on ("panes" "frames" "input-editing" "commands" "presentations" "standard-presentations" "gadgets")
    :components ((:file "dialog")
                 (:file "dialog-views" :depends-on ("dialog"))
                 (:file "notify-user")
                 (:file "menu-choose")))
   (:file "builtin-commands" :depends-on ("commands" "presentations" "standard-presentations" "input-editing"))))
