;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by Robert Strandh (strandh@labri.u-bordeaux.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :CLIM-INTERNALS)

(eval-when (eval load compile)

  (defclass design ()
    ())
  )

(defconstant +foreground-ink+ (make-instance 'design))
(defconstant +background-ink+ (make-instance 'design))
(defconstant +flipping-ink+ (make-instance 'design))

;;; Color class

(defclass color ()
  ((red :initarg :red
	:initform 0.0)
   (green :initarg :green
	  :initform 0.0)
   (blue :initarg :blue
	 :initform 0.0)
   ))

(defun colorp (x)
  (typep x 'color))

(defmethod print-object ((color color) stream)
  (with-slots (red green blue) color
    (print-unreadable-object (color stream :type t :identity t)
      (format stream "~,4F ~,4F ~,4F" red green blue))))

(defclass named-color (color)
  ((name :initarg :name
	 :initform "Unnamed color")
   ))

(defmethod print-object ((color named-color) stream)
  (with-slots (name) color
    (print-unreadable-object (color stream :type nil :identity t)
      (format stream "COLOR ~S" name))))

(defvar *color-hash-table* (make-hash-table :test #'eql))

(defun compute-color-key (red green blue)
  (+ (ash (round (* 255 red)) 16)
     (ash (round (* 255 green)) 8)
     (round (* 255 blue))))

(defun make-rgb-color (red green blue)
  (let ((key (compute-color-key red green blue)))
    (declare (type fixnum key))
    (or (gethash key *color-hash-table*)
	(setf (gethash key *color-hash-table*)
	      (make-instance 'named-color :red red :green green :blue blue)))))

(defun make-named-color (name red green blue)
  (let* ((key (compute-color-key red green blue))
	 (entry (gethash key *color-hash-table*)))
    (declare (type fixnum key))
    (cond (entry
	   (when (string-equal (slot-value entry 'name) "Unnamed color")
	     (setf (slot-value entry 'name) name))
	   entry)
	  (t (setf (gethash key *color-hash-table*)
		   (make-instance 'named-color :name name :red red :green green :blue blue))))))

;;; For ihs to rgb conversion, we use the formula 
;;;  i = (r+g+b)/3
;;;  s = 1-min(r,g,b)/i
;;;  h =     60(g-b)/(max(r,g,b)-min(r,g,b)) if r >= g,b
;;;      120+60(b-r)/(max(r,g,b)-min(r,g,b)) if g >= r,b
;;;      240+60(r-g)/(max(r,g,b)-min(r,g,b)) if b >= r,g
;;; First, we introduce colors x, y, z such that x >= y >= z
;;; We compute x, y, and z and then determine the correspondance
;;; between x, y, and z on the one hand and r, g, and b on the other. 
(defun make-ihs-color (i h s)
  (assert (and (<= 0 i 1)
	       (<= 0 s 1)
	       (<= 0 h 360)))
  (let ((ah (/ (abs (cond ((<= h 60) h)
			  ((<= h 180) (- h 120))
			  ((<= h 300) (- h 240))
			  (t (- h 360))))
	       60)))
    (let* ((z (* i (- 1 s)))
	   (y (/ (+ (* ah (- (* 3 i) (* 2 z))) z) (+ 1 ah)))
	   (x (- (* 3 i) y z)))
      (assert (and (<= 0 x 1)
		   (<= 0 y 1)
		   (<= 0 z 1)))
      (cond ((<= h 60) (make-rgb-color x y z))
	    ((<= h 120) (make-rgb-color y x z))
	    ((<= h 180) (make-rgb-color z x y))
	    ((<= h 240) (make-rgb-color z y x))
	    ((<= h 300) (make-rgb-color y z x))
	    (t (make-rgb-color x z y))))))

(defun make-gray-color (intensity)
  (make-rgb-color intensity intensity intensity))

(defmethod color-rgb ((color color))
  (with-slots (red green blue) color
    (values red green blue)))

(defmethod color-ihs ((color color))
  (multiple-value-bind (r g b) (color-rgb color)
    (let ((max (max r g b))
	  (min (min r g b))
	  (intensity (/ (+ r g b) 3)))
      (if (= max min)
	  (values intensity 0 0)
	  (let* ((saturation (- 1 (/ min intensity)))
		 (diff (- max min))
		 (hue (* 60 (cond ((= max r) (/ (- g b) diff))
				  ((= max g) (+ 2 (/ (- b r) diff)))
				  (t (+ 4 (/ (- r g) diff)))))))
	    (when (< hue 0)
	      (incf hue 360))
	    (values intensity hue saturation))))))

(defun make-contrasting-inks (n &optional k)
  (declare (special +contrasting-colors+))
  (if (> n (length +contrasting-colors+))
      (error "The argument N is out of range [1-~D]" (length +contrasting-colors+)))
  (if (null k)
      (subseq +contrasting-colors+ n)
    (aref +contrasting-colors+ k)))

#|
;;; I used this function to generate the predefined colors and names - mikemac@mikemac.com

(defun generate-named-colors ()
  (with-open-file (out "X11-colors.lisp" :direction :output :if-exists :supersede)
    (with-open-file (in "/usr/X11/lib/X11/rgb.txt" :direction :input)
      (format out ";;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-~%~%")
      (format out "(in-package :CLIM-INTERNALS)~%~%")
      (loop with names = nil
	  for line = (read-line in nil nil)
	  until (null line)
	  do (if (eql (aref line 0) #\!)
		 (format out ";~A~%" (subseq line 1))
	       (multiple-value-bind (red index) (parse-integer line :start 0 :junk-allowed t)
		 (multiple-value-bind (green index) (parse-integer line :start index :junk-allowed t)
		   (multiple-value-bind (blue index) (parse-integer line :start index :junk-allowed t)
		     (let ((name (substitute #\- #\Space (string-trim '(#\Space #\Tab #\Newline) (subseq line index)))))
		       (format out "(defconstant +~A+ (make-named-color ~S ~,4F ~,4F ~,4F))~%" name name (/ red 255.0) (/ green 255.0) (/ blue 255.0))
		       (setq names (nconc names (list name))))))))
	  finally (format out "~%(defconstant +contrasting-colors+ (vector +black+ +red+ +green+ +blue+ +cyan+ +magenta+ +yellow+ +white+))~%~%")
		  (format out "(eval-when (eval compile load)~%  (export '(")
		  (loop for name in names
		      for count = 1 then (1+ count)
		      do (format out "+~A+ " name)
			 (when (= count 4)
			   (format out "~%            ")
			   (setq count 0)))
		  (format out "~%           )))~%")
	     ))))

|#
