(defpackage slim
  (:use #:cl)
  (:export #:+golden-ratio+
	   #:with-table #:row #:cell #:*table*))
(in-package slim)

(defparameter +golden-ratio+ #. (/ (+ 1 (sqrt 5)) 2)
  "Golden Ratio constant.")


(defvar *table*)

(defmacro with-table ((pane &rest options) &body body)
  `(let ((*table* ,pane))
     (clim:formatting-table (*table* ,@options)
       ,@body)))

(defmacro row (&body body)
  `(clim:formatting-row (*table*) ,@body))

(defmacro cell (&body body)
  `(clim:formatting-cell (*table*) ,@body))
