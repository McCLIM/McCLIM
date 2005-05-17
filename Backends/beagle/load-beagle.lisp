(format t "Ensure you have issued the commands: (require \"cocoa\") and (require \"asdf\")...~%")
(format t "~%Loading Beagle~%")
;;;(load "/Users/duncan/sandbox/evins/McCLIM/Backends/Cocoa/src/cocoa-backend.asd")
(load "/Users/duncan/sandbox/common-lisp.net/mcclim/Backends/beagle/beagle-backend.asd")
(asdf:operate 'asdf:load-op 'beagle)
;;; Use this to specify the frame manager you want to use by default (note: if you
;;; want 'beagle::beagle-aqua-frame-manager, you don't need to set this since that
;;; is the default).
;;;(setf beagle::*default-beagle-frame-manager* 'beagle::beagle-standard-frame-manager)
;;;(setf beagle::*default-beagle-frame-manager* 'beagle::beagle-aqua-frame-manager)
(setf clim:*default-frame-manager* 'beagle:beagle-aqua-frame-manager)
(format t "~%Done.~%")
