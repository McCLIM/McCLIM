;;; -*- Mode: Lisp; Package: CLOUSEAU -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2005 by
;;;           Vincent Arkesteijn
;;;  (c) copyright 2005 by
;;;           Peter Scott (sketerpot@gmail.com)

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

;;; CLIM inspector application

(in-package :clouseau)

(define-modify-macro togglef () not)

(define-application-frame inspector ()
  ((dico :initform (make-hash-table) :reader dico)
   (cons-cell-dico :initform (make-hash-table) :reader cons-cell-dico)
   (disassembly-dico :initform (make-hash-table) :reader disassembly-dico
		     :documentation "A hash table specifying which
functions should display disassembly")
   (print-length :initform (make-hash-table) :reader print-length
		 :documentation "A hash table mapping list objects to
their specific print lengths, if they have one.")
   (obj :initarg :obj :reader obj
	:documentation "The object being inspected"))
  (:pointer-documentation t)
  (:panes
   (app :application :width 600 :min-height 500
        :height :compute
	:scroll-bars nil
	:text-style (make-text-style :sans-serif :roman :normal)
	:display-function 'display-app)
   (int :interactor :width 600 :height 100 :max-height 100))
  (:layouts
   (default (vertically () (scrolling () app) int))))

(defmethod initialize-instance :after ((frame inspector) &rest args)
  (declare (ignore args))
  (setf (gethash (obj frame) (dico frame)) t))

;; Remember the scrolling state between redisplays.
(defmethod redisplay-frame-panes :around ((frame inspector) &key force-p)
  (declare (ignore force-p))
  ;; `Make-clim-stream-pane' creates bizarro object hierarchies, so
  ;; getting the actual scrollable is not obvious.
  (let* ((scrollable-pane (sheet-parent (find-pane-named frame 'app)))
         (viewport (pane-viewport scrollable-pane)))
    (multiple-value-bind (x-displacement y-displacement)
        (transform-position (sheet-transformation scrollable-pane) 0 0)
      (call-next-method)
      (scroll-extent scrollable-pane
                     (min (- x-displacement)
                          (- (bounding-rectangle-width scrollable-pane)
                             (bounding-rectangle-width viewport)))
                     (min (- y-displacement)
                          (- (bounding-rectangle-height scrollable-pane)
                             (bounding-rectangle-height viewport)))))))

(defun inspector (obj &key (new-process nil))
  (flet ((run ()
	   (let ((*print-length* 10)
		 (*print-level* 10))
	     (run-frame-top-level
	      (make-application-frame 'inspector :obj obj)))))
    
    (when (typep *application-frame* 'inspector)
      (restart-case (error "Clouseau called from inside Clouseau, possibly infinite recursion")
        (continue ()
         :report "Continue by starting a new Clouseau instance")
        (abort-clouseau ()
         :report "Abort this call to Clouseau"
         (return-from inspector))))
    (if new-process
	(clim-sys:make-process #'run
         :name (format nil "Inspector Clouseau: ~S"
                       obj))
	(run))
    obj))

(defparameter *inspected-objects* '()
  "A list of objects which are currently being inspected with
INSPECT-OBJECT")

(defgeneric inspect-object-briefly (object pane)
  (:documentation "Inspect an object in a short form, displaying this
on PANE. For example, rather than displaying all the slots of a class,
only the class name would be shown."))

(defgeneric inspect-object (object pane)
  (:documentation "Inspect an object, displaying it on PANE. This can
be as verbose as you like; the important thing is that all the
information is present."))

(defmethod inspect-object :around (object pane)
  (cond ((member object *inspected-objects*)
         (with-output-as-presentation
             (pane object (presentation-type-of object))
           (princ "===" pane)))		; Prevent infinite loops
        ((not (gethash object (dico *application-frame*)))
         (inspect-object-briefly object pane))
        (t
	 (let ((*inspected-objects* (cons object *inspected-objects*))
               (*print-length* (or (gethash object (print-length
						    *application-frame*))
                                   *print-length*)))
           (call-next-method)))))

;; This behavior should be overridden by methods for specific object
;; types that have a more informative short representation. For
;; example, the symbol FOO would be printed as "FOO" instead of "...",
;; since that's just as short and more informative. When it's clicked
;; on, it can then go to a more verbose view.
(defmethod inspect-object-briefly (object pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (princ "..." pane)))

(defmethod inspect-object (object pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (prin1 object pane)))


(define-presentation-type settable-slot ()
  :inherit-from t)
(define-presentation-type cons ()
  :inherit-from t)
(define-presentation-type long-list-tail ()
  :inherit-from t)

(define-presentation-method present (object (type settable-slot)
					    stream
					    (view textual-view)
					    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format stream "~s" (cdr object)))

(defmacro with-heading-style ((stream) &body body)
  "Cause text output from BODY to be formatted in a heading font. This
could be boldface, or a different style, or even another font."
  `(with-text-face (,stream :bold)
    ,@body))

(defmacro inspector-table ((object pane) header &body body)
  "Present OBJECT in tabular form on PANE, with HEADER evaluated to
print a label in a box at the top. BODY should output the rows of the
table, possibly using INSPECTOR-TABLE-ROW."
  (let ((evaluated-pane (gensym "pane"))
	(evaluated-object (gensym "object")))
    `(let ((,evaluated-pane ,pane)
	   (,evaluated-object ,object))
      (with-output-as-presentation
	  (pane ,evaluated-object
		(presentation-type-of ,evaluated-object)
                :single-box t)
	(formatting-table (,evaluated-pane)
	  (formatting-column (,evaluated-pane)
	    (formatting-cell (,evaluated-pane)
	      (surrounding-output-with-border (,evaluated-pane)
		(with-heading-style (,evaluated-pane)
		  ,header)))
	    (formatting-cell (,evaluated-pane)
	      (formatting-table (,evaluated-pane)
		,@body))))
	(print-documentation (if (eql (class-of ,evaluated-object)
				      (find-class 'standard-class))
				 ,evaluated-object
				 (class-of ,evaluated-object))
			     ,evaluated-pane)))))

(defmacro inspector-table-row ((pane) left right)
  "Output a table row with two items, produced by evaluating LEFT and
RIGHT, on PANE. This should be used only within INSPECTOR-TABLE."
  (let ((evaluated-pane (gensym "pane")))
    `(let ((,evaluated-pane ,pane))
      (formatting-row (,evaluated-pane)
	(formatting-cell (,evaluated-pane :align-x :right)
	  (with-heading-style (,evaluated-pane)
	    ,left))
	(formatting-cell (,evaluated-pane)
	  ,right)))))

(defmacro inspector-table-rows ((pane) &body rows)
  "Output a bunch of rows with INSPECTOR-TABLE-ROW on PANE. Each row
is a list of a label and a value."
  (let ((evaluated-pane (gensym "pane")))
    `(let ((,evaluated-pane ,pane))
      ,@(loop for row in rows
	      collect (destructuring-bind (label value) row
			`(inspector-table-row (,evaluated-pane)
			  (princ ,label ,evaluated-pane)
			  (inspect-object ,value ,evaluated-pane)))))))

;; The error handler shouldn't be necessary, but it works around an
;; ACL bug and shouldn't mess anything up on other lisps. The warning
;; handler is there in case DOCUMENTATION raises a warning, to tell
;; lisp that we don't care and it shouldn't go alarming the user.
(defun print-documentation (object pane)
  "Print OBJECT's documentation, if any, to PANE"
  (when (handler-case (documentation object t)
	  (error ())
	  (warning ()))
    (with-heading-style (pane)
      (format pane "~&Documentation: "))
    (princ (documentation object t) pane)))

(defun display-class-superclasses (class pane)
  "Display the superclasses of CLASS with an INSPECTOR-TABLE-ROW"
  (when (c2mop:class-direct-superclasses class)
    (inspector-table-row (pane)
	(princ "Superclasses" pane)
      (inspect-vertical-list (c2mop:class-direct-superclasses class)
			     pane))))

(defun display-class-subclasses (class pane)
  "Display the subclasses of CLASS with an INSPECTOR-TABLE-ROW"
  (when (c2mop:class-direct-subclasses class)
    (inspector-table-row (pane)
	(princ "Subclasses" pane)
      (inspect-vertical-list (c2mop:class-direct-subclasses class)
			     pane))))

(defun display-object-slot (object slot pane &key display-lists-vertically)
  "Display a slot of OBJECT onto PANE in the way normally used when
inspecting standard objects. SLOT must be a MOP SLOT-DEFINITION
object. If DISPLAY-LISTS-VERTICALLY is t and the slot value is a list,
it will be displayed with INSPECT-VERTICAL-LIST."
  (let ((slot-name (c2mop:slot-definition-name slot)))
    (inspector-table-row (pane)
	(with-output-as-presentation
	    (pane (cons object slot-name) 'settable-slot)
	  (format pane "~a:" slot-name))
      (if (slot-boundp object slot-name)
	  (let ((slot-value (slot-value object slot-name)))
	    (if (and display-lists-vertically
		     (listp slot-value))
		(inspect-vertical-list slot-value pane
				       :honor-dico t)
		(inspect-object slot-value pane)))
	  (format pane "#<unbound slot>")))))

(defun inspect-structure-or-object (object pane)
  "Inspect a structure or an object. Since both can be inspected in
roughly the same way, the common code is in this function, which is
called by the INSPECT-OBJECT methods for both standard objects and
structure objects."
  (let ((class (class-of object)))
    (inspector-table (object pane)
	(print (class-name class) pane)
      ;; Display superclasses and subclasses
      (display-class-superclasses class pane)
      (display-class-subclasses class pane)
      (dolist (slot (reverse (c2mop:class-slots class)))
	(display-object-slot object slot pane)))))

(defun inspect-standard-class (object pane)
  "Inspect a STANDARD-CLASS. This works almost the same way as
inspecting a standard object, but with a few differences. This should
also be used to inspect BUILD-IN-CLASSes."
  (let ((class (class-of object)))
    (inspector-table (object pane)
	(print (class-name class) pane)
      ;; Display superclasses and subclasses
      (display-class-superclasses class pane)
      (display-class-subclasses class pane)
      (dolist (slot (reverse (c2mop:class-slots class)))
	(display-object-slot object slot pane
			     :display-lists-vertically t)))))

;; Try to print the normal, textual representation of an object, but
;; if that's too long, make an abbreviated "instance of ~S" version.
;; FIXME: should this be removed? It's really ugly.
(defparameter *object-representation-max-length* 300
  "Maximum number of characters of an object's textual representation
that are allowed before abbreviation kicks in")

(defun inspect-structure-or-object-briefly (object pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (with-text-family (pane :fix)
      (handler-case
	  (let ((representation (with-output-to-string (string)
				  (prin1 object string))))
	    (if (< (length representation) *object-representation-max-length*)
		(princ representation pane)
		(format pane "#<~S ...>" (class-name (class-of object)))))
	(error ()
	  (format pane "#<unprintable ~S>" (class-name (class-of object))))))))

(defmethod inspect-object-briefly ((object standard-object) pane)
  (inspect-structure-or-object-briefly object pane))

(defmethod inspect-object-briefly ((object structure-object) pane)
  (inspect-structure-or-object-briefly object pane))

(defmethod inspect-object-briefly ((object condition) pane)
  (inspect-structure-or-object-briefly object pane))

(defmethod inspect-object ((object standard-object) pane)
  (inspect-structure-or-object object pane))

(defmethod inspect-object ((object structure-object) pane)
  (inspect-structure-or-object object pane))

(defmethod inspect-object ((object standard-class) pane)
  (inspect-standard-class object pane))

(defmethod inspect-object ((object built-in-class) pane)
  (inspect-standard-class object pane))

(defmethod inspect-object ((object condition) pane)
  (inspect-structure-or-object object pane))

(defun inspect-cons-as-cells (object pane)
  "Inspect a cons cell in a fancy graphical way. The inconvenient part
is that this necessarily involves quite a bit of clicking to show a
moderately-sized list."
  (if (null (cdr object))
      (formatting-table (pane)
	(formatting-column (pane)
	  (formatting-cell (pane)
            (with-output-as-presentation
                (pane object 'cons)
              (draw-rectangle* pane 0 0 20 10 :filled nil))
	    (draw-line* pane 10 0 10 10)
	    (draw-arrow* pane 5 5 5 30)
	    (draw-line* pane 10 10 20 0))
	  (formatting-cell (pane)
	    (inspect-object (car object) pane))))
      (formatting-table (pane)
	(formatting-row (pane)
	  (formatting-cell (pane)
	    (formatting-table (pane)
	      (formatting-column (pane)
		(formatting-cell (pane)
		  (with-output-as-presentation
		      (pane object 'cons)
		    (draw-rectangle* pane 0 0 20 10 :filled nil))
		  (draw-line* pane 10 0 10 10)
		  (draw-arrow* pane 5 5 5 30)
		  (draw-arrow* pane 15 5 40 5))
		(formatting-cell (pane)
		  (inspect-object (car object) pane)))))
	  (formatting-cell (pane)
	    (inspect-object (cdr object) pane))))))

(defun inspect-vertical-list (object pane &key honor-dico)
  "Inspect a list without the parentheses, putting each element on a
new line. This is useful for showing things like direct class
subclasses, since displaying those as a plain list looks ugly and is
inconvenient to use. If HONOR-DICO is t, this will respect DICO and
display '...' if OBJECT is not in DICO."
  ;; Ordinarily this would be taken care of in the :around method for
  ;; INSPECT-OBJECT, but since this is not a normal inspection view,
  ;; we need to do it ourselves. Yes, it would be better if we could
  ;; find another way to do this.
  (let ((*print-length* (or (gethash object (print-length
					     *application-frame*))
			    *print-length*)))
    (if (and honor-dico
	     (not (gethash object (dico *application-frame*))))
	(inspect-object-briefly object pane)
	(with-output-as-presentation
	    (pane object 'cons)
	  (formatting-table (pane)
	    (formatting-column (pane)
	      (do
	       ((length 0 (1+ length))
		(cdr (cdr object) (cdr cdr))
		(car (car object) (car cdr)))
	       ((cond ((eq nil cdr)
		       (formatting-cell (pane) (inspect-object car pane))
		       t)
		      ((not (consp cdr))
		       (formatting-cell (pane) (inspect-object car pane))
		       (formatting-cell (pane) (princ "." pane))
		       (formatting-cell (pane) (inspect-object cdr pane))
		       t)
		      ((and *print-length* (>= length *print-length*))
		       (with-output-as-presentation
			   (pane object 'long-list-tail)
			 (formatting-cell (pane) (princ "..." pane)))
		       t)
		      (t nil)))
		(formatting-cell (pane) (inspect-object car pane)))))))))

(defun inspect-cons-as-list (object pane)
  "Inspect a cons cell in a traditional, plain-text format. The only
difference between this and simply using the Lisp printer is that this
code takes advantage of CLIM's tables and presentations to make the
list as interactive as you would expect."
  (with-output-as-presentation
      (pane object 'cons)
    (formatting-table (pane)
      (formatting-row (pane)
        (formatting-cell (pane)
          (princ "(" pane))
        (do
	 ((length 0 (1+ length))
	  (cdr (cdr object) (cdr cdr))
	  (car (car object) (car cdr)))
	 ((cond ((eq nil cdr)
		 (formatting-cell (pane) (inspect-object car pane))
		 (formatting-cell (pane) (princ ")" pane))
		 t)
		((not (consp cdr))
		 (formatting-cell (pane) (inspect-object car pane))
		 (formatting-cell (pane) (princ "." pane))
		 (formatting-cell (pane) (inspect-object cdr pane))
		 (formatting-cell (pane) (princ ")" pane))
		 t)
		((and *print-length* (>= length *print-length*))
		 (with-output-as-presentation (pane object 'long-list-tail)
		   (formatting-cell (pane) (princ "...)" pane)))
		 t)
		(t nil)))
          (formatting-cell (pane) (inspect-object car pane)))))))

(defmethod inspect-object ((object cons) pane)
  ;; Decide how to display the cons by looking in cons-cell-dico
  (if (gethash object (cons-cell-dico *application-frame*))
      (inspect-cons-as-cells object pane)
      (inspect-cons-as-list object pane)))

(defun show-hash-table-status (hash pane &key (message "Usage Graph"))
  "Show a hash table's status graphically on a given
pane. Display a given message, which defaults to 'Usage Graph'."
  (with-room-for-graphics (pane :height 20)
    (let* ((my-beige (make-rgb-color 0.9372549 0.8862745 0.8862745))
	   (used-color (make-rgb-color 0.43529412 0.7921569 0.87058824))
	   (text-color (make-rgb-color 0.7176471 0.29803923 0.2))
	   (pattern (make-rectangular-tile
		     (make-pattern #2A((0 1 0 0 0)
				       (1 0 0 0 0)
				       (0 0 0 0 1)
				       (0 0 0 1 0)
				       (0 0 1 0 0))
				   (list my-beige +black+)) 5 5)))
      (draw-rectangle* pane 0 0 150 20 :filled t :ink my-beige)
      (draw-rectangle* pane 0 0 (* 150 (/ (hash-table-count hash)
					  (hash-table-size hash)))
		       20 :filled t :ink used-color :line-thickness 0)
      (draw-rectangle* pane (* 150 (hash-table-rehash-threshold hash)) 0 150 20
		       :filled t :ink pattern :line-thickness 0)
      (draw-rectangle* pane 0 0 150 20 :filled nil :ink +black+)
      (draw-text* pane message 7 10 :align-y :center :align-x :left
		  :text-size :small :ink text-color :text-face :italic))))

(defmethod inspect-object-briefly ((object hash-table) pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (show-hash-table-status object pane :message "Hash table")))
(defmethod inspect-object ((object hash-table) pane)
  (inspector-table (object pane)
      (progn (format pane "~A (test: ~A)  " 'hash-table (hash-table-test object))
	     (show-hash-table-status object pane))
    (loop for key being the hash-keys of object
       do (formatting-row (pane)
	    (formatting-cell (pane :align-x :right)
	      (inspect-object key pane))
	    (formatting-cell (pane) (princ "=" pane))
	    (formatting-cell (pane)
	      (inspect-object (gethash key object) pane))))))

(defmethod inspect-object ((object generic-function) pane)
  (inspector-table (object pane)
      (format pane "Generic Function: ~s"
	      (c2mop:generic-function-name object))
    (dolist (method (c2mop:generic-function-methods object))
      (with-output-as-presentation
	  (pane method (presentation-type-of method))
	(formatting-row (pane)
	  (formatting-cell (pane)
	    (with-text-family (pane :fix)
	      (print (method-qualifiers method) pane)))
	  (loop for specializer in (c2mop:method-specializers method)
		do (formatting-cell (pane)
		     (if (typep specializer 'c2mop:eql-specializer)
			 (progn
			   (princ "(EQL " pane)
			   (inspect-object
			    (c2mop:eql-specializer-object
			     specializer)
			    pane)
			   (princ ")" pane))
			 (inspect-object (class-name specializer)
					 pane)))))))))

(defun pretty-print-function (fun)
  "Print a function in a readable way, returning a string. On most
implementations this just uses the standard Lisp printer, but it can
use implementation-specific functions to be more informative."
  (flet ((generic-print (fun)
	   (with-output-to-string (string)
	     (prin1 fun string))))
    ;; If we have SBCL, try to do fancy formatting. If anything goes
    ;; wrong with that, fall back on ugly standard PRIN1.
    #+sbcl
    (unless (typep fun 'generic-function)
      (let ((fun (sb-kernel:%closure-fun fun)))
	(handler-case (format nil "~A ~S"
			      (sb-kernel:%simple-fun-name fun)
			      (sb-kernel:%simple-fun-arglist fun))
	  (error () (generic-print fun)))))
    ;; FIXME: Other Lisp implementations have ways of getting this
    ;; information. If you want a better inspector on a non-SBCL Lisp,
    ;; please add code for it and send patches.
    #-sbcl (generic-print fun)))

;; This is ugly. I think CLIM requires there to be a presentation type
;; for every class, so we should use FUNCTION---but I'm not sure how
;; well that will work.
(define-presentation-type inspected-function ()
  :inherit-from t)

(defmethod inspect-object ((object function) pane)
  (with-output-as-presentation
      (pane object 'inspected-function)
    (with-heading-style (pane)
      (princ "Function: " pane))
    (with-text-family (pane :fix)
      (princ (pretty-print-function object) pane))
    #+sbcl
    (unless (typep object 'generic-function)
      (with-heading-style (pane)
	(format pane "~&Type: "))
      (with-text-family (pane :fix)
	(princ (sb-kernel:%simple-fun-type (sb-kernel:%closure-fun object))
	       pane)))
    (print-documentation object pane)
    (when (gethash object (disassembly-dico *application-frame*))
      (display-disassembly object pane))))

(defmethod inspect-object-briefly ((object package) pane)
  ;; Display as 'Package: "PACKAGE-NAME"'. We're doing something a
  ;; little unusual here by not bolding the "Package:" part. This may
  ;; be a tad inconsistent, but the other way looks very odd.
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (princ "Package: " pane)
    (with-text-family (pane :fix)
      (princ (package-name object) pane))))

(defun package-exported-symbols (package)
  "Return a list of all symbols exported by PACKAGE"
  (let (symbols)
    (do-external-symbols (symbol package symbols)
      (push symbol symbols))))

(defmethod inspect-object ((object package) pane)
  (inspector-table (object pane)
      (format pane "Package: ~S" (package-name object))
    (inspector-table-row (pane)
	(princ "Name:" pane)
      (inspect-object (package-name object) pane))
    (inspector-table-row (pane)
	(princ "Nicknames:" pane)
      (inspect-vertical-list (package-nicknames object) pane))
    (inspector-table-row (pane)
	(princ "Used by:" pane)
      (inspect-vertical-list (package-used-by-list object) pane))
    (inspector-table-row (pane)
	(princ "Uses:" pane)
      (inspect-vertical-list (package-use-list object) pane))
    (inspector-table-row (pane)
	(princ "Exports:" pane)
      (inspect-vertical-list (package-exported-symbols object) pane))))

(defmethod inspect-object ((object vector) pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (formatting-table (pane)
      (formatting-row (pane)
        (formatting-cell (pane)
          (princ "#(" pane))
        (dotimes (i (length object))
          (formatting-cell (pane)
            (inspect-object (aref object i) pane)))
        (formatting-cell (pane)
          (princ ")" pane))))))

(defmethod inspect-object-briefly ((object string) pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (prin1 object pane)))

(defmethod inspect-object-briefly ((object number) pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (prin1 object pane)))

(defun inspect-complex (object pane)
  "Inspect a complex number. Since complex numbers should be inspected
the same way briefly and fully, this function can be called by both of
them."
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (formatting-table (pane)
      (formatting-row (pane)
        (formatting-cell (pane)
          (princ "#C(" pane))
        (formatting-cell (pane)
          (inspect-object (realpart object) pane))
        (formatting-cell (pane)
          (inspect-object (imagpart object) pane))
        (formatting-cell (pane)
          (princ ")" pane))))))

(defmethod inspect-object-briefly ((object complex) pane)
  (inspect-complex object pane))

(defmethod inspect-object ((object complex) pane)
  (inspect-complex object pane))

(defmethod inspect-object ((object float) pane)
  (inspector-table (object pane)
      (format pane "Float ~S" object)
    (multiple-value-bind (significand exponent sign)
        (decode-float object)
      (inspector-table-rows (pane)
	("sign:" sign)
	("significand:" significand)
	("exponent:" exponent)))
    (inspector-table-rows (pane)
      ("radix:" (float-radix object)))))

(defun iso-8601-format (time)
  "Return the given universal time in ISO 8601 format. This will raise
an error if the given time is not a decodable universal time."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
	    year month date hour min sec)))

(defmethod inspect-object ((object integer) pane)
  (flet ((present-in-base (base &key (radix t) (family :fix))
	   (with-text-family (pane family)
	     (formatting-cell (pane)
	       (with-output-as-presentation
		   (pane object (presentation-type-of object))
		 (write object :radix radix :base base :stream pane)))))
	 (print-equals-cell ()
	   (formatting-cell (pane)
	     (princ "=" pane))))
    (inspector-table (object pane)
	(format pane "Integer ~S" object)
      (inspector-table-row (pane)
	  (princ "value:" pane)
	(formatting-table (pane)
	  (formatting-row (pane)
	    ;; Base 10 should be displayed normally, without the
	    ;; fixed-width font and without the radix.
	    (present-in-base 10 :radix nil :family :sans-serif)
	    (print-equals-cell)		; =
	    (present-in-base 16)	; Hexadecimal
	    (print-equals-cell)		; =
	    (present-in-base 8)		; Octal
	    (print-equals-cell)		; =
	    (present-in-base 2))))	; Binary
      (when (<= 0 object 255)
	(inspector-table-row (pane)
	    (princ "character:" pane)
	  (inspect-object (code-char object) pane)))
      (inspector-table-row (pane)
	  (princ "length:" pane)
	(inspect-object (integer-length object) pane))
      ;; Sometimes we get numbers that can't be interpreted as a
      ;; time. Those throw an error, and this just isn't printed.
      (ignore-errors
	(inspector-table-row (pane)
	    (princ "as time:" pane)
	  (with-text-family (pane :fix)
	    (with-output-as-presentation
		(pane object (presentation-type-of object))
	      (princ (iso-8601-format object) pane))))))))

(defmethod inspect-object-briefly ((object symbol) pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (with-text-family (pane :fix)
      (prin1 object pane))))

(defmethod inspect-object ((object symbol) pane)
  (inspector-table (object pane)
      (format pane "Symbol ~S" (symbol-name object))
    (inspector-table-row (pane)
	(princ "value:" pane)
      (if (boundp object)
	  (inspect-object (symbol-value object) pane)
	  (princ "unbound" pane)))
    (inspector-table-row (pane)
	(princ "function:" pane)
      (if (fboundp object)
	  (inspect-object (symbol-function object) pane)
	  (princ "unbound" pane)))
    ;; This is not, strictly speaking, a property of the
    ;; symbol. However, this is useful enough that I think it's worth
    ;; including here, since it can eliminate some minor annoyances.
    (inspector-table-row (pane)
	(princ "class:" pane)
      (if (find-class object nil)
	  (inspect-object (find-class object) pane)
	  (princ "unbound" pane)))
    (inspector-table-row (pane)
	(princ "package:" pane)
      (inspect-object (symbol-package object) pane))
    (inspector-table-row (pane)
	(princ "propery list:" pane)
      (dolist (property (symbol-plist object))
	(inspect-object property pane)))))

;; Characters are so short that displaying them as "..."  takes almost
;; as much space as just showing them, and this way is more
;; informative.
(defmethod inspect-object-briefly ((object character) pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (print object pane)))
(defmethod inspect-object ((object character) pane)
  (inspector-table (object pane)
      (format pane "Character ~S" object)
    (inspector-table-rows (pane)
      ("code:" (char-code object))
      ("int:"  (char-int object))
      ("name:" (char-name object)))))

(defmethod inspect-object ((object pathname) pane)
  (inspector-table (object pane)
      (princ (if (wild-pathname-p object)
		 "Wild pathname"
		 "Pathname"))
    (inspector-table-rows (pane)
      ("namestring:" (namestring object))
      ("host:"       (pathname-host object))
      ("device:"     (pathname-device object))
      ("directory:"  (pathname-directory object))
      ("name:"       (pathname-name object))
      ("type:"       (pathname-type object))
      ("version:"    (pathname-version object)))
    (unless (or (wild-pathname-p object)
		(not (probe-file object)))
      (inspector-table-row (pane)
	  (princ "truename:" pane)
	(inspect-object (truename object) pane)))))

(defun display-app (frame pane)
  "Display the APP frame of the inspector"
  (inspect-object (obj frame) pane))

(define-inspector-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-inspector-command (com-inspect :name t) ()
  (let ((obj (accept t :prompt "Select an object"))
        (*application-frame* nil))      ; To get around security.
    (inspector obj :new-process t)))

(define-inspector-command (com-toggle-show-list-cells :name t)
    ((obj 'cons :gesture :select :prompt "Select a cons or list"))
  (togglef (gethash obj (cons-cell-dico *application-frame*))))

(define-inspector-command (com-show-10-more-items :name t)
    ((obj 'long-list-tail :gesture :select :prompt "Select a truncated list"))
  (if (gethash obj (print-length *application-frame*))
      (incf (gethash obj (print-length *application-frame*)) 10)
      (setf (gethash obj (print-length *application-frame*))
	    (+ 10 *print-length*))))

(define-inspector-command (com-toggle-inspect :name t)
    ((obj t :gesture :select :prompt "Select an object"))
  (unless (or (eq obj (obj *application-frame*))
	      (null obj))
    (togglef (gethash obj (dico *application-frame*)))))

(define-inspector-command (com-remove-method :name t)
    ((obj 'method :gesture :delete :prompt "Remove method"))
  (remove-method (c2mop:method-generic-function obj) obj))

(define-inspector-command (com-set-slot :name t)
    ((slot 'settable-slot :gesture :select :prompt "Set slot"))
  (handler-case (setf (slot-value (car slot) (cdr slot))
		      (accept t :prompt "New slot value"))
    (simple-parse-error ()
      (format (get-frame-pane *application-frame* 'int)
	      "~&Command canceled; slot value not set~%"))))

(defun slot-documentation (class slot)
  "Returns the documentation of a slot of a class, or nil. There is,
unfortunately, no portable way to do this, but the MOP is
semi-portable and we can use it. To complicate things even more, some
implementations have unpleasant oddities in the way they store slot
documentation. For example, in SBCL slot documentation is only
available in direct slots."
  (let ((slot-object (find slot (c2mop:class-direct-slots class)
			   :key #'c2mop:slot-definition-name)))
    (if slot-object
	(documentation slot-object t)
	(when (c2mop:class-direct-superclasses class)
	  (find-if #'identity
		   (mapcar #'(lambda (class)
			       (slot-documentation class slot))
			   (c2mop:class-direct-superclasses class)))))))

(define-inspector-command (com-describe-slot :name t)
    ((slot 'settable-slot :gesture :describe :prompt "Describe slot"))
  (destructuring-bind (object . slot-name) slot
    (let* ((stream (get-frame-pane *application-frame* 'int))
	   (class (class-of object))
	   (documentation (handler-bind ((warning #'muffle-warning))
			    (slot-documentation class slot-name)))
	   (slot-object (or (find slot-name (c2mop:class-direct-slots class)
                                  :key #'c2mop:slot-definition-name)
                            (find slot-name (c2mop:class-slots class)
                                  :key #'c2mop:slot-definition-name))))
      (when documentation
	(with-heading-style (stream)
	  (format stream "~&Documentation: "))
	(format stream "~A~%" documentation))
      (with-heading-style (stream)
	(format stream "~&Type: "))
      (format stream "~S~%" (c2mop:slot-definition-type slot-object))
      (with-heading-style (stream)
	(format stream "~&Allocation: "))
      (format stream "~S~%" (c2mop:slot-definition-allocation slot-object))
      ;; slot-definition-{readers,writers} only works for direct slot
      ;; definitions
      (let ((readers (c2mop:slot-definition-readers slot-object)))
        (when readers
	  (with-heading-style (stream)
	    (format stream "~&Readers: "))
          (present readers (presentation-type-of readers) :stream stream)))
      (let ((writers (c2mop:slot-definition-writers slot-object)))
        (when writers
          (with-heading-style (stream)
	    (format stream "~&Writers: "))
          (present writers (presentation-type-of writers) :stream stream))))))

(define-inspector-command (com-disassemble :name t)
    ((obj 'inspected-function
	  :prompt "Select a function"))
  (when (typep obj 'function)
    (togglef (gethash obj (disassembly-dico *application-frame*)))))

(define-presentation-to-command-translator disassemble-function
    (inspected-function com-disassemble inspector
			:documentation "Toggle Disassembly"
			:gesture :menu
			:menu t)
    (object)
  (list object))

(defun tracedp (symbol)
  "Is SYMBOL currently traced?"
  (member symbol (trace)))

(define-inspector-command (com-trace :name t)
    ((obj 'symbol
	  :prompt "Select an fbound symbol"))
  (when (fboundp obj)
    (eval `(trace ,obj))))

(define-inspector-command (com-untrace :name t)
    ((obj 'symbol
	  :prompt "Select an fbound symbol"))
  (when (fboundp obj)
    (eval `(untrace ,obj))))

(define-presentation-to-command-translator trace-symbol
    (symbol com-trace inspector
	    :documentation "Trace"
	    :gesture :menu
	    :menu t
	    :tester ((object) (and object
				   (fboundp object)
				   (not (tracedp object)))))
    (object)
  (list object))

(define-presentation-to-command-translator untrace-symbol
    (symbol com-untrace inspector
	    :documentation "Untrace"
	    :gesture :menu
	    :menu t
	    :tester ((object) (and object
				   (fboundp object)
				   (tracedp object))))
    (object)
  (list object))

;; FIXME: This is a horrible hack to gloss over issues that I don't
;; properly understand. See
;; <http://common-lisp.net/pipermail/mcclim-devel/2005-February/003700.html>
(defmethod clim:presentation-type-of ((object standard-generic-function))
  'clim:expression)
