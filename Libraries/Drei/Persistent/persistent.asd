;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; System definition for the persistent system

(defsystem "persistent"
  :components ((:file "binseq-package")
               (:file "binseq" :depends-on ("binseq-package"))
               (:file "obinseq" :depends-on ("binseq-package" "binseq"))
               (:file "binseq2" :depends-on ("binseq-package" "obinseq" "binseq"))))
