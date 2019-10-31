(defpackage #:slim
  (:use #:clim-lisp)
  (:export #:+golden-ratio+
           #:defapp
	   #:with-table #:row #:col #:cell #:*pane*))
(in-package #:slim)

(defparameter +golden-ratio+ #. (/ (+ 1 (sqrt 5)) 2)
  "Golden Ratio constant.")


;;; application frames
(defmacro defapp (name-and-opts args &body body)
  "Macro allows defining a single-pane application frame as if it were
a function.

NAME-AND-OPTS is a list where first element is a symbol being the
frame and the function name. The rest of a list are arguments passed
to the application pane as the initialization arguments. NAME-AND-OPTS
may be a symbol, then it is treated as a list with a single element.

Args are function arguments. They are available in the body. Bindings
are closed over the display function.

Examples:

  (defapp example-1 (x y z)
    (clim:draw-rectangle* *standard-output* x y (+ x 100) (+ y 200))
    (format t \"~&Z is ~s\" z))

  (defapp (example-2 :text-margins `(:left 20 :top 10 :right 20 :bottom 10))
      (x y z)
    (clim:draw-rectangle* *standard-output* x y (+ x 100) (+ y 200))
    (format t \"~&Z is ~s\" z))

  (example-1 100 100 \"jackdaniel\")
  (example-2  50  50 \"another\")
"
  (alexandria:with-gensyms (display-fn)
    (destructuring-bind (name &rest pane-options)
        (alexandria:ensure-list name-and-opts)
      `(progn
         (clim:define-application-frame ,name ()
           ((,display-fn :initarg :display-function
                         :reader display-function))
           (:pane :application
            :display-function (display-function clim:*application-frame*)
            ,@pane-options))
         (defun ,name ,args
           (flet ((,display-fn (clim:*application-frame* *standard-output*)
                    ,@body))
             (declare (dynamic-extent #',display-fn))
             (clim:run-frame-top-level
              (clim:make-application-frame ',name :display-function #',display-fn))))))))


;;; context
(defvar *pane*)

(defmacro with-pane ((pane) &body body)
  `(let* ((*pane* ,pane)
          (*standard-output* *pane*))
     ,@body))


;;; with-table
(defmacro with-table ((&optional (pane *pane*) &rest options)
                      &body body)
  `(with-pane (,pane)
     (clim:formatting-table (*pane* ,@options)
       ,@body)))

(defmacro row  (&body body) `(clim:formatting-row    (*pane*) ,@body))
(defmacro col  (&body body) `(clim:formatting-column (*pane*) ,@body))
(defmacro cell (&body body) `(clim:formatting-cell   (*pane*) ,@body))
