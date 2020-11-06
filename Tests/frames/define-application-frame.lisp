(cl:in-package #:clim-tests)

(def-suite* :mcclim.frames
  :in :mcclim)

(test parse-define-application.smoke
  "Test errors signaled by `define-application-frame' at
macroexpansion time."

  (finishes
    (macroexpand '(define-application-frame foo () ()
                   (:geometry :width 1 :height 2))))
  (signals error
    (macroexpand '(define-application-frame foo () ()
                   (:geometry 1))))
  (signals error
    (macroexpand '(define-application-frame foo () ()
                   (:geometry :diameter 1)))))
