;;;; mcclim-dot package
;;;;
;;;; This file is part of the mcclim-dot extension. See
;;;; Extensions/dot/README.md and Extensions/dot/LICENSE for more information.

(cl:defpackage #:mcclim-dot
  (:use #:clim #:clim-lisp)
  (:local-nicknames (#:a #:alexandria)
                    (#:dot #:cl-dot)
                    (#:pn #:parse-number)
                    (#:ss #:split-sequence)))
