(in-package :common-lisp-user)

(defpackage "APP"
  (:use :clim :clim-lisp)
  (:export "APP-MAIN"))

(in-package :app)

(define-application-frame superapp ()
  ()
  (:panes
   (int :interactor)
   (lab1 :push-button-pane :label "dummy-1")
   (lab2 :label-pane :label "dummy-2" :background +blue-violet+))
  (:layouts
   (default (vertically () #|int|#
              int
              lab1 lab2))))

(defun app-main ()
  (let ((frame (make-application-frame 'superapp)))
    (values frame
            (bt:make-thread
             (lambda ()
               (run-frame-top-level frame))))))
