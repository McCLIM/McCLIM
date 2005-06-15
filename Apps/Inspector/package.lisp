(defpackage :clouseau
  (:use :clim-lisp :clim)
  (:export #:inspector
	   #:inspect-object
	   #:inspect-object-briefly
	   #:define-inspector-command
	   #:inspector-table
	   #:inspector-table-row))
