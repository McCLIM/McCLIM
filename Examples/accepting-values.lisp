(in-package :clim-demo)

;;; Some simple examples from the Franz user manual. You can run
;;; these from the listener.

(defun accepting-interval (&key (min -1.0) (max 1.0) (stream *query-io*) (ow t))
  (clim:accepting-values (stream :resynchronize-every-pass t :own-window ow)
    (fresh-line stream)
    (setq min (clim:accept 'real :default min :prompt "Min":stream stream))
    (fresh-line stream)
    (setq max (clim:accept 'real :default max :prompt "Max" :stream stream))
    (when (< max min)
      (rotatef min max)))
  (values min max))

(defun accepting-square
    (&key (xmin -1.0) (xmax 1.0) (ymin -1.0) (ymax 1.0) (stream *query-io*) (ow t))
  (let (xmin-changed xmax-changed ymin-changed ymax-changed ptype)
    (accepting-values (stream :resynchronize-every-pass t :own-window ow)
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

(defun reset-clock-1 (&key (stream *query-io*) (ow t))
  (multiple-value-bind (second minute hour day month)
      (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (restart-case
	(progn
	  (clim:accepting-values (stream :own-window ow)
	    (format stream "Enter the time~%")
	    (setq month (clim:accept 'integer :stream stream
				      :default month :prompt "Month"))
	    (terpri stream)
	    (setq day (clim:accept 'integer :stream stream
				    :default day :prompt "Day"))
	    (terpri stream)
	    (setq hour (clim:accept 'integer :stream stream
				     :default hour :prompt "Hour"))
	    (terpri stream)
	    (setq minute (clim:accept 'integer :stream stream
				       :default minute :prompt "Minute")))
	  ;; This could be code to reset the time, but instead
	  ;; we’re just printing it out
	  (format nil "New values: Month: ~D, Day: ~D, Time: ~D:~2,'0D."
		  month day hour minute))
      (abort () (format nil "Time not set")))))

(defun reset-clock-2 (&key (stream *query-io*) (ow t))
  (multiple-value-bind (second minute hour day month)
      (decode-universal-time (get-universal-time))
    (restart-case
	(progn
	  (clim:accepting-values (stream :own-window ow)
	    (format stream "Enter the time~%")
	    (setq month (clim:accept 'integer :stream stream
				     :default month :prompt "Month"))
	    (terpri stream)
	    (setq day (clim:accept 'integer :stream stream
				   :default day :prompt "Day"))
	    (terpri stream)
	    (setq hour (clim:accept 'integer :stream stream
				    :default hour :prompt "Hour"))
	    (terpri stream)
	    (setq minute (clim:accept 'integer :stream stream
				      :default minute :prompt "Minute"))
	    (terpri stream)
	    (clim:accept-values-command-button (stream) "Zero seconds"
	      (setq second 0)))
	  ;; This could be code to reset the time, but
	  ;; instead we’re just printing it out
	  (format nil "New values: Month: ~D, Day: ~D, Time: ~D:~2,'0D:~2,'0D."
		  month day hour minute second))
      (abort () (format t "Time not set")))))

(defun accepting-tag (&key (stream *query-io*) (ow t))
  (let (a b c)
    (accepting-values
	(stream :initially-select-query-identifier 'the-tag :own-window ow)
      (terpri stream)
      (setq a (accept 'pathname :prompt "A pathname" :stream stream))
      (terpri stream)
      (setq b (accept 'integer :prompt "A number"
		       :query-identifier 'the-tag :stream stream))
      (terpri stream)
      (setq c (accept 'string :prompt "A string" :stream stream)))
    (values a b c)))

;; menu choose

(defun menu-choose-1 ()
  (menu-choose '(("One" :value 1 :documentation "the loneliest number")
		 ("Two" :value 2 :documentation "for tea")
		 ("Seventeen" :documentation "what can be said about this?"))))

(defun menu-choose-2 ()
  (menu-choose '(1 2 17)
	       :printer #'(lambda (item stream) (format stream "~R" item))))

(defun menu-choose-3 ()
  (menu-choose '(circle square triangle)
	       :printer #'(lambda (item stream)
			    (clim:with-drawing-options (stream :ink clim:+yellow+)
			      (case item
				(circle (clim:draw-circle* stream 0 0 10))
				(square (clim:draw-polygon*
					 stream '(-8 -8 -8 8 8 8 8 -8)))
				(triangle (clim:draw-polygon*
					   stream '(10 8 0 -10 -10 8))))))))
(defun menu-choose-4 ()
  (clim:menu-choose
   '(("Class: Osteichthyes"
      :documentation "Bony fishes"
      :style (nil :italic nil))
     ("Class: Chondrichthyes"
      :documentation "Cartilagenous fishes"
      :style (nil :italic nil)
      :items (("Order: Squaliformes" :documentation "Sharks")
	      ("Order: Rajiformes" :documentation "Rays")))
     ("Class: Mammalia"
      :documentation "Mammals"
      :style (nil :italic nil)
      :items (("Order Rodentia"
	       :items ("Family Sciuridae"
		       "Family Muridae"
		       "Family Cricetidae"
		       ("..." :value nil)))
	      ("Order Carnivora"
	       :items ("Family: Felidae"
		       "Family: Canidae"
		       "Family: Ursidae"
		       ("..." :value nil)))
	      ("..." :value nil)))
     ("..." :value nil))))

;;; Test of McCLIM extension

(defun accept-popup (seq &key (stream *query-io*))
  (let ((val (elt seq 0))
	(ptype `(completion ,seq)))
    (accepting-values (stream)
      (setq val (accept ptype :stream stream :view climi::+pop-up-menu-view+
			:prompt "Choose one:" :default val)))
    val))
