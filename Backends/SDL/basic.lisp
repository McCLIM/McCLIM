(in-package :clim-sdl)

(defun init-sdl (port)
  (sdl2:init :everything)
  (when clim-sys:*multiprocessing-p*
    (setf (climi::port-event-process port)
          (clim-sys:make-process (lambda ()
                                   (loop
                                     (with-simple-restart
                                         (restart-event-loop "Restart event loop")
                                       (loop
                                         do (process-next-event port)))))
                                 :name (format nil "SDL event process for port: ~s" port)))))

(defun quit-sdl (port)
  (declare (ignore port))
  (sdl2:quit))

(defun round-coordinate (x)
  (floor (+ x .5)))
