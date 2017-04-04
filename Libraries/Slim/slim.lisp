(defpackage slim
  (:use #:cl)
  (:export #:with-table #:row #:cell #:*table*))
(in-package slim)

(defvar *table*)

(defmacro with-table ((pane) &body body)
  `(let ((*table* ,pane))
     (clim:formatting-table (*table*)
       ,@body)))

(defmacro row (&body body)
  `(clim:formatting-row (*table*) ,@body))

(defmacro cell (&body body)
  `(clim:formatting-cell (*table*) ,@body))
