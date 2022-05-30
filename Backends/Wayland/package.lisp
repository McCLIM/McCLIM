(in-package #:common-lisp-user)

(defpackage #:clim-wayland
  (:use #:clim #:clim-lisp #:clim-backend)
  (:local-nicknames (#:alx #:alexandria)
                    (#:wlc #:com.andrewsoutar.cl-wayland-client)
                    (#:wl-core #:com.andrewsoutar.cl-wayland-client/core)
                    (#:wl-egl #:com.andrewsoutar.cl-wayland-client/egl-core)
                    (#:xdg #:com.andrewsoutar.cl-wayland-client.protocol/stable/xdg-shell/xdg-shell))
  (:import-from #:climi
                #:basic-port
                ;; slot accessors needed by ports
                #:frame-managers
                #:height
                #:width
                #:port-event-process
                #:port-grafts
                ;; #:%%sheet-native-transformation
                ;; #:%%set-sheet-native-transformation
                ;; #:device-transformation
                ;;
                #:get-environment-variable)
  (:export
   #:wayland-port))
