;;; ------------------------------------
;;; coordinate-swizzling.lisp
(cl:in-package #:clim-demo)

(clim:define-application-frame coordinate-swizzling ()
  ()
  (:panes (app :application
               :scroll-bars nil
               :display-time nil)
          (int :interactor
               :scroll-bars nil))
  (:layouts
   (:default (clim:vertically ()
               (clim:scrolling (:height 400 :scroll-bars t) app)
               (clim:scrolling (:height 50 :scroll-bars t) int)))))

(defun coordinate-swizzling ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'coordinate-swizzling)))

(define-coordinate-swizzling-command (com-fill :name t) ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'app)))
    (loop for i from 0 to 4400
          do (format pane "~4,'0d~%" i))))

(define-coordinate-swizzling-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))
