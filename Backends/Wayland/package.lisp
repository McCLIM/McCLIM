(in-package #:common-lisp-user)

(defpackage #:clim-wayland
  (:use #:clim #:clim-lisp #:clim-backend)
  (:local-nicknames (#:alx #:alexandria)
                    (#:wlc #:com.andrewsoutar.cl-wayland-client)
                    (#:wl-egl #:com.andrewsoutar.cl-wayland-client/egl-core)
                    (#:xdg #:com.andrewsoutar.cl-wayland-client.protocol/stable/xdg-shell/xdg-shell))
  (:import-from #:climi
                #:basic-port)
  (:export
   #:wayland-port))
