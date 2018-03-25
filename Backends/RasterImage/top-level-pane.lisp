(in-package :mcclim-raster-image)

;;;
;;; Top level pane
;;;

(defclass raster-image-top-level-pane (;;sheet-mute-input-mixin
				       sheet-mute-repainting-mixin
				       permanent-medium-sheet-output-mixin
				       image-sheet-mixin
				       mirrored-sheet-mixin
				       unmanaged-top-level-sheet-pane)
  ())
