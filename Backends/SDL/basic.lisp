(in-package :clim-sdl)

(defun init-sdl (port)
  (declare (ignore port))
  (sdl2:init :everything))

(defun quit-sdl (port)
  (declare (ignore port))
  (sdl2:quit))

(defun round-coordinate (x)
  (floor (+ x .5)))
