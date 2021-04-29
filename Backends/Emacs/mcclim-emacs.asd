
(in-package #:asdf-user)

(defsystem "mcclim-emacs"
  :depends-on ("clim-basic"
               "mcclim-backend-common"
               "cl-svg"
               "flexi-streams")
  :serial t
  :components ((:file "emacs)))

