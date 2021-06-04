(cl:in-package #:clim-tests)

(def-suite* :mcclim.frames
  :in :mcclim)

(test parse-define-application.smoke
  "Test errors signaled by `define-application-frame' at
macroexpansion time."

  ;; Valid.
  (finishes
    (macroexpand '(define-application-frame foo () ()
                   (:geometry :width 1 :height 2)
                   (:panes (foo :label) (bar :label))
                   (:layouts (default foo) (alternate bar)))))
  ;; Invalid argument type  for `:geometry' option.
  (signals error
    (macroexpand '(define-application-frame foo () ()
                   (:geometry 1))))
  ;; Invalid argument count  for `:geometry' option.
  (signals error
    (macroexpand '(define-application-frame foo () ()
                   (:geometry :diameter 1))))
  ;; Invalid argument type for `:panes' option.
  (signals error
    (macroexpand '(define-application-frame foo () ()
                   (:panes (1)))))
  ;; Repeated pane name in `:panes' option.
  (signals error
    (macroexpand '(define-application-frame foo () ()
                   (:panes (foo :label) (foo :abel)))))
  ;; Invalid argument type for `:layouts' option.
  (signals error
    (macroexpand '(define-application-frame foo () ()
                   (:layouts (1)))))
  ;; Repeated pane name in `:layouts' option.
  (signals error
    (macroexpand '(define-application-frame foo () ()
                   (:layouts (foo bar) (foo baz))))))
