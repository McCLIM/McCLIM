(in-package clim-user)

(slim:defapp zooming ()
  (draw-rectangle* *standard-output* 10 10 90 90)
  (draw-line* *standard-output* 100 100 200 200))

(define-zooming-command (com-zoom-down :keystroke (#\q :control)) ()
  (format *debug-io* "zoom down!~%")
  (let ((medium (sheet-medium *standard-output*)))
    (setf (climi::medium-zoom medium)
          (compose-transformations
           (make-scaling-transformation 0.9 0.9)
           (climi::medium-zoom medium)))))

(define-zooming-command (com-zoom-up :keystroke (#\e :control)) ()
  (format *debug-io* "zoom up!~%")
  (let ((medium (sheet-medium *standard-output*)))
    (setf (climi::medium-zoom medium)
          (compose-transformations
           (make-scaling-transformation 1.1 1.1)
           (climi::medium-zoom medium)))))

(define-zooming-command (com-move-left :keystroke (#\a :control)) ()
  (format *debug-io* "moving left!~%")
  (let ((medium (sheet-medium *standard-output*)))
    (setf (climi::medium-zoom medium)
          (compose-transformations
           (make-translation-transformation -10 0)
           (climi::medium-zoom medium)))))

(define-zooming-command (com-move-right :keystroke (#\d :control)) ()
  (format *debug-io* "moving left!~%")
  (let ((medium (sheet-medium *standard-output*)))
    (setf (climi::medium-zoom medium)
          (compose-transformations
           (make-translation-transformation +10 0)
           (climi::medium-zoom medium)))))

(define-zooming-command (com-move-up :keystroke (#\w :control)) ()
  (format *debug-io* "moving up!~%")
  (let ((medium (sheet-medium *standard-output*)))
    (setf (climi::medium-zoom medium)
          (compose-transformations
           (make-translation-transformation 0 -10)
           (climi::medium-zoom medium)))))

(define-zooming-command (com-move-down :keystroke (#\s :control)) ()
  (format *debug-io* "moving down!~%")
  (let ((medium (sheet-medium *standard-output*)))
    (setf (climi::medium-zoom medium)
          (compose-transformations
           (make-translation-transformation 0 +10)
           (climi::medium-zoom medium)))))

(zooming)
