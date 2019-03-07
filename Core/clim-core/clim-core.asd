
(defsystem #:clim-core
  :depends-on (#:clim-basic #:clim-postscript #+sbcl (:require #:sb-introspect))
  :components
  ((:file "defresource")
   (:file "presentations")
   (:file "bordered-output" :depends-on ("presentations"))
   (:file "table-formatting" :depends-on ("presentations"))
   (:file "input-editing" :depends-on ("presentations" "bordered-output" "table-formatting"))
   (:file "pointer-tracking" :depends-on ("input-editing"))
   (:file "graph-formatting")
   (:file "frames" :depends-on ("commands" "presentations" "presentation-defs"
                                "pointer-tracking" "incremental-redisplay"))
   (:file "dialog-views" :depends-on ("presentations" "incremental-redisplay"
                                      "bordered-output" "presentation-defs" "gadgets"))
   (:file "presentation-defs" :depends-on ("input-editing" "presentations"))
   (:file "gadgets" :depends-on ("commands" "pointer-tracking" "input-editing" 
                                 "frames" "incremental-redisplay" "panes"))
   (:file "describe" :depends-on ("presentations" "presentation-defs" "table-formatting"))
   (:file "commands" :depends-on ("input-editing" "presentations"
                                  "presentation-defs"))
   (:file "incremental-redisplay" :depends-on ("presentation-defs"))
   (:file "menu-choose" :depends-on ("commands" "table-formatting" "presentation-defs"
                                     "panes" "frames" "pointer-tracking" "presentations"))
   (:file "menu" :depends-on ("panes" "commands" "gadgets" "presentations" "frames"))
   (:file "panes" :depends-on ("incremental-redisplay" "presentations" "presentation-defs"
                               "input-editing" "frames"))
   (:file "dialog" :depends-on ("panes" "frames" "incremental-redisplay"
                                "table-formatting" "presentations"
                                "bordered-output" "presentation-defs"
                                "dialog-views" "input-editing"
                                "commands" "gadgets"))
   (:file "builtin-commands" :depends-on ("table-formatting" "commands" "presentations"
                                          "dialog" "presentation-defs" "input-editing"))
   (:file "presentations-clipboard" :depends-on ("presentations"))))
