;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) Copyright 2000 by Robert Strandh <strandh@labri.u-bordeaux.fr>
;;;  (c) Copyright 2002 by Tim Moore <moore@bricoworks.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-postscript)

(defclass postscript-destination (climb:file-destination)
  ())

(defmethod climb:invoke-with-standard-output
    (continuation (destination postscript-destination))
  (call-next-method (lambda ()
                      (with-output-to-postscript-stream
                          (*standard-output* *standard-output*)
                        (funcall continuation)))
                    destination))

(climb:register-output-destination-type
 "Postscript File" 'postscript-destination)
