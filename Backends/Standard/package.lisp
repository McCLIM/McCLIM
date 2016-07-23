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
		)
  )
