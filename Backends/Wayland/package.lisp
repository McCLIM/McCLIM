(in-package #:common-lisp-user)

(defpackage #:mcclim-wayland
  (:use #:clim #:clim-lisp #:clim-backend)
  (:local-nicknames (#:alx #:alexandria)
                    (#:wlc #:com.andrewsoutar.cl-wayland-client)
                    (#:wl-core #:com.andrewsoutar.cl-wayland-client/core)
                    (#:wl-egl #:com.andrewsoutar.cl-wayland-client/egl)
                    (#:xdg #:com.andrewsoutar.cl-wayland-client.protocol/stable/xdg-shell/xdg-shell))
  (:import-from #:climi
                #:basic-port
                ;; slot accessors needed by ports
                #:frame-managers
                #:top-level-sheet
                #:medium-device-region
                #:height
                #:width
                #:maybe-funcall
                #:port-event-process
                #:port-grafts
                #:top-level-sheet-mixin
                #:top-level-sheet-pane
                #:sheet-mirror-geometry
                ;; events
                #:window-configuration-event
                #:window-manager-delete-event
                ;; #:%%sheet-native-transformation
                ;; #:%%set-sheet-native-transformation
                ;; #:device-transformation
                ;;
                #:get-environment-variable)
  (:export
   #:wayland-port
   #:wayland-egl-medium
   #:wayland-graft))
