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
		#:%note-mirrored-sheet-child-grafted
		#:%note-mirrored-sheet-child-degrafted
		#:%note-mirrored-sheet-child-adopted
		#:%note-mirrored-sheet-child-disowned
		#:%note-mirrored-sheet-child-enabled
		#:%note-mirrored-sheet-child-disabled
		#:%note-mirrored-sheet-child-region-changed
		#:%note-mirrored-sheet-child-transformation-changed
		#:%note-sheet-pointer-cursor-changed
		#:%note-mirrored-sheet-child-pointer-cursor-changed
		#:%note-sheet-repaint-request
		#:%note-mirrored-sheet-child-repaint-request
		#:dispatch-repaint
		#:always-repaint-background-mixin
		#:never-repaint-background-mixin
		#:selection-event
		#:selection-request-event
		#:selection-notify-event
		#:bind-selection
		#:release-selection
		#:request-selection))
