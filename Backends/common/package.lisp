(in-package :common-lisp-user)

(defpackage :clim-standard
  (:use :clim :clim-lisp :clim-backend)
  (:export #:font-character-width
           #:font-string-width
           #:font-text-extents
           #:font-ascent
           #:font-descent
           #:font-leading
           #:font-tracking)
  (:import-from #:climi
                #:%sheet-mirror-region
                #:%sheet-mirror-transformation
                #:port-lookup-mirror
                #:port-pointer-sheet
                #:set-sheet-pointer-cursor
                ;; Types
                #:top-level-sheet-pane
		#:unmanaged-top-level-sheet-pane))
