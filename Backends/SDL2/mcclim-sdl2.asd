(in-package #:asdf-user)

(defsystem "mcclim-sdl2"
  :author "Daniel Kochmański"
  :description "SDL2 backend"
  :depends-on ("mcclim" "sdl2" "log4cl" "lparallel")
  :serial t
  :components ((:file "packages")
               (:file "utilities")      ; sdl2 glue
               (:file "resources")      ; sdl2 memory
               (:file "port")           ; sdl2 <-> clim
               (:file "pointer")        ; touch, mouse, cursors
               (:file "keyboard")       ; keybaord, input methods
               (:file "window")         ; mirrors
               (:file "plain-sheet")))  ; testing
