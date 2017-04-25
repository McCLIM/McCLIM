(defpackage slim
  (:use #:clim-lisp)
  (:export #:+golden-ratio+
	   #:with-table #:row #:cell #:*pane*))
(in-package slim)

(defparameter +golden-ratio+ #. (/ (+ 1 (sqrt 5)) 2)
  "Golden Ratio constant.")


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

(defmacro row  (&body body) `(clim:formatting-row  (*pane*) ,@body))
(defmacro cell (&body body) `(clim:formatting-cell (*pane*) ,@body))
