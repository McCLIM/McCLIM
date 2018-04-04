(in-package :common-lisp-user)

(defpackage :clim-standard
  (:use :clim :clim-lisp :clim-backend )
  (:import-from :climi
		#:native-transformation
		#:native-region
		#:device-transformation
		#:device-region
		#:graftp
		#:window-configuration-event-x
		#:window-configuration-event-y
		#:window-configuration-event-width
		#:window-configuration-event-height
		#:top-level-sheet-pane-p
		#:set-sheet-pointer-cursor
		#:port-pointer-sheet
		#:top-level-sheet-pane
		#:unmanaged-top-level-sheet-pane
		#:sheet-direct-mirror
		#:port-lookup-mirror
		#:port-register-mirror
		#:coordinate=

		#:%%sheet-native-transformation
		#:%%set-sheet-native-transformation
		#:dispatch-repaint
		#:always-repaint-background-mixin
		#:never-repaint-background-mixin
		#:selection-event
		#:selection-request-event
		#:selection-notify-event
		#:bind-selection
		#:release-selection
		#:request-selection
                climi::%sheet-mirror-region
                climi::%sheet-mirror-transformation))
