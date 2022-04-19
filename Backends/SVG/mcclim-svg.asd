(in-package #:asdf-user)

(defsystem "mcclim-svg"
  :author "Daniel Kochmański"
  :license "LGPL-2.1+"
  :description "SVG backend for McCLIM"
  :depends-on ("mcclim" "mcclim-bitmaps" "mcclim-fonts/truetype"
               "alexandria" "cl-who" "cl-base64" "flexi-streams" )
  :components ((:file "svg-backend")
               (:static-file "svg-backend-tests")))
