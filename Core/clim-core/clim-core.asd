(in-package #:asdf-user)

(defsystem "clim-core"
  :depends-on ("clim-basic" (:feature :sbcl "sb-introspect"))
  :components
  ((:file "defresource")
   (:file "theming")
   (:module "presentations"
    :serial t
    :components ((:file "presentation-types")
                 (:file "presentation-functions")
                 (:file "presentation-type-functions")
                 (:file "presentation-histories")
                 (:file "typed-output")
                 (:file "typed-input")
                 (:file "translators")
                 (:file "drag-and-drop")))
   (:module "formatting"
    :depends-on ("presentations" "theming")
    :components ((:file "bordered-output")
                 (:file "table-formatting")
                 (:file "graph-formatting")))
   (:file "input-editing" :depends-on ("presentations" "formatting"))
   (:file "standard-presentations"
    :pathname "presentations/standard-presentations"
    :depends-on ("input-editing" "presentations"))
   (:file "incremental-redisplay" :depends-on ("standard-presentations"))
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
   (:file "dialog-views" :depends-on ("presentations" "incremental-redisplay" "formatting" "standard-presentations" "gadgets" "dialog"))
   (:file "menu-choose" :depends-on ("commands" "formatting"  "panes" "frames" "presentations"))
   (:file "dialog" :depends-on ("panes" "frames" "incremental-redisplay" "formatting" "presentations" "standard-presentations" "input-editing" "commands" "gadgets"))
   (:file "builtin-commands" :depends-on ("formatting" "commands" "presentations" "dialog" "standard-presentations" "input-editing"))))
