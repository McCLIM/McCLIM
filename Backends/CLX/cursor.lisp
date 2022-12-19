(in-package #:clim-clx)

(defvar *clx-cursor-mapping*
  `(((:default :arrow) 68)
    ((:prompt :i-beam) 152)
    ((:button :hand) 60)
    ((:busy :wait) 150)
    ((:not-allowed) 88)
    ((:position) 130)
    ((:move) 52)
    ((:arrow-we) 108)
    ((:arrow-ns) 116)
    ((:grab) 60)
    ((:help) 92)
    ;; Backward compatibility (Franz)
    ((:horizontal-scroll :horizontal-thumb) 108)
    ((:vertical-scroll :vertical-thumb) 116)
    ((:lower-left) 12)
    ((:lower-right) 14)
    ((:scroll-down) 106)
    ((:scroll-left) 110)
    ((:scroll-right) 112)
    ((:scroll-up) 114)
    ((:upper-left) 134)
    ((:upper-right) 136)
    ;; The following are not in the Franz docs, but might be useful.
    ((:vertical-pointer) 22)
    ((:pencil) 86)
    ((:rotate) 50)
    ((:choose) 60)))

(defun make-cursor-table (port)
  (declare (optimize (safety 3) (debug 3) (speed 0) (space 0)))
  (let ((font (xlib:open-font (clx-port-display port) "cursor")))
    (loop for (symbols code) in *clx-cursor-mapping*
          for cursor = (xlib:create-glyph-cursor
                        :foreground (xlib:make-color :red 0.0 :green 0.0 :blue 0.0)
                        :background (xlib:make-color :red 1.0 :green 1.0 :blue 1.0)
                        :source-font font
                        :source-char code
                        :mask-font font
                        :mask-char (1+ code))
          do (dolist (symbol symbols)
               (setf (gethash symbol (clx-port-cursor-table port)) cursor)))
    (xlib:close-font font)))
