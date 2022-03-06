(in-package #:asdf-user)

#| dummy system to make Quicklisp happy |#
(defsystem "mcclim-fonts"
  :depends-on ("clim-basic")
  :components ((:file "common")))

(defsystem "mcclim-fonts/truetype"
  :depends-on ("clim-basic" "cl-dejavu" "zpb-ttf" "cl-vectors" "cl-paths-ttf" "cl-aa" "alexandria" "flexi-streams")
  :components ((:static-file "README.md")
               (:file "truetype-package")
               (:file "fontconfig" :depends-on ("truetype-package"))
               (:file "mcclim-native-ttf" :depends-on ("truetype-package"))
               (:file "ttf-port-mixin" :depends-on ("truetype-package" "mcclim-native-ttf"))
               (:file "ttf-medium-mixin" :depends-on ("truetype-package" "mcclim-native-ttf"))))

(defsystem "mcclim-fonts/clx-freetype"
  :depends-on ("mcclim-fonts" "mcclim-clx" "cl-freetype2" "mcclim-fontconfig" "mcclim-harfbuzz")
  :components ((:file "freetype")))
