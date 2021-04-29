
(in-package #:asdf-user)

(defsystem "mcclim-emacs"
  :depends-on ("clim-basic"
               "mcclim-backend-common"
               "cl-svg"
               "flexi-streams")
  :serial t
  :components ((:file "package")
               #+nil (:file "emacs-frame-manager")
               (:file "emacs-port")
               (:file "emacs-medium")
               (:file "emacs-sheet")))

