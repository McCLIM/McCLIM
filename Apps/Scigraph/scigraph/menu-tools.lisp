;;; -*- Syntax: Common-lisp; Package: TOOL -*-
#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(in-package :tool)

(eval-when (compile load eval)
  (export '(several-choose choose-character-style window-edit-text string-size)))

(defun string-size (stream style format-string &rest format-args)
  ;; A bad implementation of this can really slow down graph generation.
  (unless (stringp format-string)
    (setq format-string (string format-string)))
  (when format-args
    ;; Typically, the formatter is not needed.
    (setq format-string (apply #'format nil format-string format-args)))
  #+clim-1.0
  (when (eq style clim:*null-text-style*)
    ;; A kludge for sure.  
    (setq style clim:*default-text-style*))
  #+clim
  (let* ((return #.(aref (format nil "~%") 0))
	 (line-cnt (1+ (count return format-string :test #'char=))))
    ;; This is 2-3 times faster than using continuation-output-size.
    (multiple-value-bind (x xmax)
	(stream-string-width stream format-string :text-style style)
      (declare (ignore x))
      (values xmax (* line-cnt (stream-line-height stream style)))))
  #-clim
  (continuation-output-size #'(lambda (s)
				(if style
				    (with-character-style (style s)
				      (format s format-string))
				  (format s format-string)))
			    stream))

;;; These things are needed mainly for annotations, but they are kept in a separate
;;; file to minimize the clutter in the annotations code.

(defun draw-radio-button (stream object text selected-p &optional size)
  (declare (ignore object))
  (or size (setq size (stream-line-height stream)))
  (let* ((rad (values (truncate (- size 2) 2)))
	 (offset rad))
    ;; Since clim insists on flipping the coordinate system...
    #+(or clim-1.0 clim-2) (setq offset (- offset))
;;    #+clim-0.9 (terpri stream)
    (multiple-value-bind (x y) (stream-cursor-position* stream)
      #+clim-0.9
      (stream-set-cursor-position* stream (setq x (+ x size)) y)
      (draw-circle (+ x offset) (+ y offset)
		   rad :stream stream :filled selected-p)
      (if selected-p
	  (with-character-face (:bold stream)
	    (draw-string text (+ x (* size 2))
			 (+ y (* offset 2))
			 :stream stream))
          (draw-string text (+ x (* size 2))
		       (+ y (* offset 2))
		       :stream stream)))))

#+clim-2
(define-presentation-type-abbreviation button-subset (&key alist (test 'equal))
  `(subset-alist ,alist :test ,test))

#+(or (not clim) clim-0.9 clim-1.0)
(define-presentation-type button-subset (&key alist)  
  :parser ((stream)
	   (accept `(sequence (alist-member :alist ,alist))
		   :stream stream :prompt nil))
  :printer ((object stream)
	    (present object `(sequence (alist-member :alist ,alist))
		     :stream stream))
  :typep ((object)
	  (block testem
	    (dolist (element object)
	      (or (find element alist :key #'dwim::menu-execute-no-side-effects)
		  (return-from testem nil)))
	    t))
  :describer ((stream)
	      (write-string "any of " stream)
	      (let (length
		    (name-key #'dwim::token-element-string)
		    (rest-of-elements alist))
		(loop
		  (or rest-of-elements (return))
		  (setq length (length rest-of-elements))
		  (format stream "~A" (funcall name-key (car rest-of-elements)))
		  (cond ((> length 2)
			 (write-string ", " stream))
			((= length 2)
			 (write-string " or " stream))))))
  :accept-values-displayer
  ((stream object query-identifier)
   ;; OBJECT is the currently chosen subset.
   (accept-values-choose-from-sequence
     stream alist object query-identifier
     :select-action
     #'(lambda (new list)
	 (cond ((not (listp list)) (list new))
	       ((member new list)  (remove new list))
	       (t (adjoin new list))))
     :selection-test #'member
     :n-columns 1
     :drawer
     #'(lambda (stream object text selected-p)
	 (draw-radio-button stream object text selected-p)))))

(defun SEVERAL-CHOOSE (ITEM-LIST
		       &key highlighted-values (label "Choose Several")
			    (stream *standard-output*) (own-window t))
  "Lets you select several choices."
  (declare (values choices abort-p))
  ;; Used by choose-descriptors to produce interval annotations.
  ;;
  ;; item-list is a list whose elements are either:
  ;;   a.  atoms
  ;;   b.  lists of length 2 whose CAR is the pretty name and whose CADR is the
  ;;        actual value.
  (labels ((stringify (thing)
	     (typecase thing
	       (string thing)
	       (symbol (symbol-name thing))
	       (otherwise (format nil "~A" thing)))))
    (let ((ptype `(button-subset
		   :alist
		   ,(mapcar #'(lambda (item)
				(if (atom item)
				    (list (stringify item)
					  :value item)
				  (list (stringify (car item))
					:value (cadr item))))
			    item-list))))
      (if (eq :abort
	      (accepting-values (stream :own-window own-window
					:label "Choose")
				(format stream label)
				(terpri stream)
				(setq highlighted-values
				  #+clim-2
				  (accept
				   ptype
				   :default highlighted-values
				   :view
				   '(check-box-view :orientation :vertical)
				   :prompt "Choose Several"
				   :stream stream)
				  #-clim-2
				  (accept ptype
					  :default highlighted-values
					  :prompt "Choose Several"
					  :stream stream))
				(terpri stream)))
	  (values nil t)
	(nreverse highlighted-values)))))

(defun test-chooser ()
  (several-choose '(apples oranges pears)))

(defun character-style-choices (family)
  (mapcar
   #'(lambda (style)
       `(,(apply #'format nil "~A ~A ~A" style)	:value ,style :style ,style))
   (mapcar #'(lambda (face-size) (cons family face-size))
	   (mapcan #'(lambda (size)
		       `((:bold-italic ,size)
			 (:bold ,size)
			 (:italic ,size)
			 (:roman ,size)))
		   '(:very-large :large :normal :small :very-small)))))

(defun CHOOSE-CHARACTER-STYLE ()
  (let* ((family (menu-choose
		  (mapcar #'(lambda (fam)
			      `(,fam :value ,fam :style (,fam :roman :normal)))
			  '(:fix #+clim :serif #-clim :dutch :sans-serif))
		  :prompt "Family"))
	 (style (when family (menu-choose (character-style-choices family)
					  :prompt "Character Styles"))))
    style))

(defvar *EDIT-DELIMITER* #-clim #\end #+clim #\return)
(defvar *min-window-height* 100)
(defvar *min-window-width* 220)

#-clim
(let ((edit-window nil))
  (defun FIND-EDIT-WINDOW (stream)
    (let ((window edit-window))
      (unless window
	(setq window
	      (tv:make-window 'tv:pop-up-text-window
			      :deexposed-typeout-action :permit
			      :more-p nil
			      :save-bits t
			      :superior stream
			      :label (format nil
					     "Edit text (~C or ~C):~%"
					     *edit-delimiter* #\abort) ))
	(setq edit-window window))
      (funcall #'(setf sheet-parent) stream window)	; +++ Unresolved genera problem.
      window)))

#-clim
(defmacro with-edit-window ((symbol superior) &body body)
  `(let ((,symbol (find-edit-window ,superior))) ,@body))

#-clim
(defun WINDOW-EDIT-TEXT (window left top right bottom &optional string)
  "Edit text in the given region of the window."
  (if (> top bottom) (psetq bottom top top bottom))
  (if (> left right) (psetq right left left right))
  (with-edit-window (ed-window window)
    (multiple-value-bind (ml mto mr mb) (scl:send ed-window :margins)
      (let* ((extra-width (+ ml mr (values (truncate (* (- right left) .5)))))
	     (extra-height (+ mto mb (* (stream-line-height ed-window) 2)))
	     width height)
	(setq width (max *min-window-width* (+ (- right left) extra-width)))
	(setq height (max *min-window-height* (+ (- bottom top) extra-height)))
	(change-size ed-window width height)
	(dw::position-window-near-carefully ed-window '(:mouse)))
      (scl:send ed-window :clear-window)
      (tv:with-window-shadowed-for-selection
	((sys:console-selected-window (tv:sheet-console window)) ed-window)
	(tv:window-call (ed-window :deactivate)
	  (scl:with-input-editing-options ((:initial-input string))
	    (scl:read-delimited-string *edit-delimiter* ed-window)))))))

#+clim
(defun WINDOW-EDIT-TEXT (window left top right bottom &optional string)
  "Edit text in the given region of the window."
  ;; This only reads a single line...	  
  ;; Note that clim 0.9 ignores the default string.
  (if (> top bottom) (psetq bottom top top bottom))
  (if (> left right) (psetq right left left right))
  (multiple-value-bind (x y) (stream-cursor-position* window)
    (let* ((prompt "Input a string")
	   (prompt-width (string-size window nil "~A: " prompt))
	   (cursor-x (max 0 (- left prompt-width)))
	   (cursor-y top)
	   string-width)
      (unwind-protect
	  (catch #+clim-0.9 'ci::abort-gesture-seen #-clim-0.9 :abort
	    (stream-set-cursor-position* window cursor-x cursor-y)
	    (with-output-recording-disabled (window)
	      (setq string (accept 'string :stream window :default string
				   :prompt prompt))
	      (setq string-width (string-size window nil string))
	      ;; erase editor typeout
	      (let* ((right (+ cursor-x prompt-width string-width))
		     (bottom (+ top (stream-line-height window)))
		     (rect (make-rectangle* cursor-x top right bottom)))
		(draw-rectangle cursor-x right top bottom
				:stream window
				:filled t
				:alu %erase)
		#-clim-2
		(output-recording-stream-replay window rect)
		#+clim-2
		(stream-replay window rect))
	      ))
	(stream-set-cursor-position* window x y))))
  string)

#+someday
(defun read-note (&optional default-note)
  (with-menu (stream)
    ;; Set up the pop-up window the way we want to see it
    (setf (clim::cursor-visibility (clim::stream-text-cursor stream)) :off)
    (clim::window-set-inside-size  stream
      (* 60 (stream-character-width stream #\Space))
      (* 10 (stream-line-height stream)))
    (write-string "Enter a note:" stream)
    (fresh-line stream)
    (setf (stream-text-margin stream) (bounding-rectangle-width (window-viewport stream)))
    (window-expose stream)
    (unwind-protect
	(with-input-editing (stream)
	  ;; Put the default note into the input buffer and ensure that
	  ;; we never do it again
	  (when default-note
	    (replace-input stream default-note :rescan t)
	    (setq default-note nil))
	  ;; Now get the input from the user
	  (with-activation-characters ('(#\Newline) :override t)
	    (unwind-protect
		(read-token stream)
	      ;; Eat the activation character
	      (read-gesture :stream stream :timeout 0))))
      (setf (clim::cursor-visibility (clim::stream-text-cursor stream)) :inactive))))
