;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2016 Alessandro Serra <gas2serra@gmail.com>
;;;  (c) copyright 2017-2020 Daniel Kochmański <daniel@turtleware.eu>
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Package definition for the clx-fb module.
;;;

(defpackage #:clim-clx-fb
  (:use #:clim
        #:clim-lisp
        #:clim-backend
        #:clim-clx
        #:mcclim-render-extensions)
  (:import-from #:alexandria
                #:when-let #:when-let* #:maphash-keys #:maphash-values)
  (:import-from #:climi
                #:port-grafts
                ;;
                #:pixmap
                #:top-level-sheet-mixin
                #:unmanaged-sheet-mixin
                ;;
                #:frame-managers        ; used as slot
                #:height                ; used to access a slot
                #:width                 ; ditto
                ;;
                #:make-medium)
  (:import-from #:mcclim-render-internals
                #:render-medium-mixin
                #:render-port-mixin
                #:image-mirror-image
                #:image-sheet-mixin
                #:image-pixmap-mixin
                #:image-mirror-mixin
                ;;
                #:mirror->%image
                #:%make-image)
  (:import-from #:clim-clx
                #:clx-port-display
                #:initialize-clx
                #:clx-port-screen
                #:clx-graft
                #:clx-port-window
                #:clx-mirror
                #:window
                #:mirroring
                #:class-gensym
                #:maybe-add-mirroring-superclasses)
  (:import-from #:climi
                #:standard-port))
