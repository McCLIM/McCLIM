(in-package :clim-demo)

;;; Some simple examples from the Franz user manual. You can run
;;; these from the listener.

(defun accepting-interval (&key (min -1.0) (max 1.0) (stream
  *query-io*))
  (clim:accepting-values (stream :resynchronize-every-pass t)
    (fresh-line stream)
    (setq min (clim:accept 'real :default min :prompt "Min":stream stream))
    (fresh-line stream)
    (setq max (clim:accept 'real :default max :prompt "Max" :stream stream))
    (when (< max min)
      (rotatef min max)))
  (values min max))

(defun accepting-square (&key (xmin -1.0) (xmax 1.0) (ymin -1.0) (ymax 1.0)
			 (stream *query-io*))
  (let (xmin-changed xmax-changed ymin-changed ymax-changed ptype)
    (accepting-values (stream :resynchronize-every-pass t)
      (fresh-line stream)
      (multiple-value-setq (xmin ptype xmin-changed)
	(accept 'real :default xmin :prompt "Xmin" :stream stream))
      (fresh-line stream)
      (multiple-value-setq (xmax ptype xmax-changed)
	(accept 'real :default xmax :prompt "Xmax" :stream stream))
      (fresh-line stream)
      (multiple-value-setq (ymin ptype ymin-changed)
	(accept 'real :default ymin :prompt "Ymin" :stream stream))
      (fresh-line stream)
      (multiple-value-setq (ymax ptype ymax-changed)
	(accept 'real :default ymax :prompt "Ymax" :stream stream))
      (cond ((or xmin-changed xmax-changed)
	     (let ((y-center (/ (+ ymax ymin) 2.0))
		   (x-half-width (/ (- xmax xmin) 2.0)))
	       (setq ymin (- y-center x-half-width)
		     ymax (+ y-center x-half-width)))
	     (setq xmin-changed nil xmax-changed nil))
	    ((or ymin-changed ymax-changed)
	     (let ((x-center (/ (+ xmax xmin) 2.0))
		   (y-half-width (/ (- ymax ymin) 2.0)))
	       (setq xmin (- x-center y-half-width)
		     xmax (+ x-center y-half-width)))
	     (setq ymin-changed nil
		   ymax-changed nil)))))
  (values xmin xmax ymin ymax))

;;; Test of McCLIM extension

(defun accept-popup (seq &key (stream *query-io*))
  (let ((val (elt seq 0))
	(ptype `(completion ,seq)))
    (accepting-values (stream)
      (setq val (accept ptype :stream stream :view climi::+pop-up-menu-view+
			:prompt "Choose one:" :default val)))
    val))