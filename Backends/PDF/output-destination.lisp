;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2000 by Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-pdf)

(defclass pdf-destination (climb:file-destination)
  ())

(defmethod climb:invoke-with-standard-output
    (continuation (destination pdf-destination))
  (call-next-method (lambda ()
                      (with-output-to-pdf-stream
                          (*standard-output* *standard-output*)
                        (funcall continuation)))
                    destination))

(climb:register-output-destination-type "PDF File" 'pdf-destination)
