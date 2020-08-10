(in-package #:asdf-user)

(defsystem "clim-core"
  :depends-on ("clim-basic" #+sbcl (:require "sb-introspect"))
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
   (:file "bordered-output" :depends-on ("presentations" "theming"))
   (:file "table-formatting" :depends-on ("presentations"))
   (:file "input-editing" :depends-on ("presentations" "bordered-output" "table-formatting"))
   (:file "standard-presentations" :depends-on ("input-editing" "presentations"))
   (:file "graph-formatting")
   (:file "frames" :depends-on ("commands" "presentations" "standard-presentations" "incremental-redisplay"))
   (:file "dialog-views" :depends-on ("presentations" "incremental-redisplay" "bordered-output" "standard-presentations" "gadgets" "dialog"))
   (:module "panes"
    :depends-on ("incremental-redisplay" "presentations" "standard-presentations" "input-editing" "frames" "theming")
    :serial t
    :components ((:file "construction")
                 (:file "layout-protocol")
                 (:file "composition")
                 (:file "stream-panes")))
   (:module "gadgets"
    :depends-on ("commands" "input-editing" "frames" "incremental-redisplay" "panes" "presentations" "theming")
    :serial t
    :components ((:file "base")
                 (:file "abstract")
                 (:file "mixins")
                 (:file "drawing-utilities")
                 (:file "concrete")
                 (:file "menu")))
   (:file "describe" :depends-on ("presentations" "standard-presentations" "table-formatting"))
   (:file "commands" :depends-on ("input-editing" "presentations" "standard-presentations"))
   (:file "incremental-redisplay" :depends-on ("standard-presentations"))
   (:file "menu-choose" :depends-on ("commands" "table-formatting"  "panes" "frames" "presentations"))
   (:file "dialog" :depends-on ("panes" "frames" "incremental-redisplay" "table-formatting" "presentations" "bordered-output" "standard-presentations" "input-editing" "commands" "gadgets"))
   (:file "builtin-commands" :depends-on ("table-formatting" "commands" "presentations" "dialog" "standard-presentations" "input-editing"))))
