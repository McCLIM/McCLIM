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
   (print-length :initform (make-hash-table) :reader print-length
		 :documentation "A hash table mapping list objects to
their specific print lengths, if they have one.")
   (obj :initarg :obj :reader obj
	:documentation "The object being inspected"))
  (:pointer-documentation t)
  (:panes
   (app :application :width 600 :height 500
	:scroll-bars nil
	:text-style (make-text-style :sans-serif :roman :normal)
	:display-function 'display-app)
   (int :interactor :width 600 :height 100 :max-height 100))
  (:layouts
   (default (vertically () (scrolling () app) int))))

(defmethod initialize-instance :after ((frame inspector) &rest args)
  (declare (ignore args))
  (setf (gethash (obj frame) (dico frame)) t))

(defmethod redisplay-frame-pane :after ((frame inspector)
					(pane application-pane)
					&key force-p)
  (declare (ignore force-p))
  (change-space-requirements
   pane
   :height (bounding-rectangle-height (stream-output-history pane))))

(defun inspector (obj &key (new-process nil))
  (flet ((run ()
	   (let ((*print-length* 10)
		 (*print-level* 10))
	     (run-frame-top-level
	      (make-application-frame 'inspector :obj obj)))))
    (if new-process
	(clim-sys:make-process #'run
			       :name (format nil "Inspector Clouseau: ~S"
					     obj))
	(run))))

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
           (princ "===")))		; Prevent infinite loops
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
    (princ "...")))

(defmethod inspect-object (object pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))  
    (print object pane)))


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

(defmacro inspector-table (header &body body)
  "Present OBJECT (captured from environment) in tabular form, with
HEADER printed in a box at the top. BODY should output the rows of the
table using INSPECTOR-TABLE-ROW. Also capured from the macro's
environment is PANE, which is the pane on which the table will be
drawn."
  `(with-output-as-presentation
       (pane object (presentation-type-of object))
     (formatting-table (pane)
       (formatting-column (pane)
         (formatting-cell (pane)
           (surrounding-output-with-border (pane)
             ,header))
         (formatting-cell (pane)
           (formatting-table (pane)
             ,@body))))
    (print-documentation object pane)))

(defmacro inspector-table-row (left right)
  "Output a table row with two items, LEFT and RIGHT, in the
environment created by INSPECTOR-TABLE."
  `(formatting-row (pane)
     (formatting-cell (pane :align-x :right)
       ,left)
     (formatting-cell (pane)
       ,right)))

(defun print-documentation (object pane)
  "Print OBJECT's documentation, if any, to PANE"
  (when (handler-bind ((warning #'muffle-warning))
	  (documentation object t))
    (format pane "~&Documentation: ~A" (documentation object t))))

(defun inspect-structure-or-object (object pane)
  "Inspect a structure or an object. Since both can be inspected in
roughly the same way, the common code is in this function, which is
called by the INSPECT-OBJECT methods for both standard objects and
structure objects."
  (let ((class (class-of object)))
    (inspector-table
     (print (class-name class) pane)
     (when (clim-mop:class-direct-superclasses class)
       (inspector-table-row
	(format pane "Superclasses")
	(dolist (superclass (clim-mop:class-direct-superclasses class))
	  (inspect-object superclass pane)
	  (terpri pane))))
     (when (clim-mop:class-direct-subclasses class)
       (inspector-table-row
	(format pane "Subclasses")
	(dolist (subclass (clim-mop:class-direct-subclasses class))
	  (inspect-object subclass pane)
	  (terpri pane))))
     (loop for slot in (reverse (clim-mop:class-slots class))
	   do (let ((slot-name (clim-mop:slot-definition-name slot)))
		(inspector-table-row
		 (with-output-as-presentation
		     (pane (cons object slot-name) 'settable-slot)
		   (format pane "~a:" slot-name))
		 (if (slot-boundp object slot-name)
		     (inspect-object (slot-value object slot-name) pane)
		     (format pane "#<unbound slot>"))))))))

;; FIXME: should this be removed? It's really ugly.
(defparameter *object-representation-max-length* 300
  "Maximum number of characters of an object's textual representation
that are allowed before abbreviation kicks in")

(defun inspect-structure-or-object-briefly (object pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (handler-case
	(let ((representation (with-output-to-string (string)
				(prin1 object string))))
	  (if (< (length representation) *object-representation-max-length*)
	      (princ representation pane)
	      (format pane "#<~S ...>" (class-name (class-of object)))))
      (error ()
	(format pane "#<unprintable ~S>" (class-name (class-of object)))))))

;; Try to print the normal, textual representation of an object, but
;; if that's too long, make an abbreviated "instance of ~S" version.

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
  (if (gethash object (cons-cell-dico *application-frame*))
        (inspect-cons-as-cells object pane)
        (inspect-cons-as-list object pane)))


(defmethod inspect-object-briefly ((object hash-table) pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (princ 'hash-table pane)))
(defmethod inspect-object ((object hash-table) pane)
  (inspector-table
      (format pane "~A (test: ~A)" 'hash-table (hash-table-test object))
    (loop for key being the hash-keys of object
          do (formatting-row (pane)
	       (formatting-cell (pane :align-x :right)
		 (inspect-object key pane))
	       (formatting-cell (pane) (princ "=" pane))
	       (formatting-cell (pane)
		 (inspect-object (gethash key object) pane))))))

(defmethod inspect-object ((object generic-function) pane)
  (inspector-table
      (format pane "Generic Function: ~s"
	      (clim-mop:generic-function-name object))
    (loop for method in (clim-mop:generic-function-methods object)
          do (with-output-as-presentation
                 (pane method (presentation-type-of method))
               (formatting-row (pane)
                 (formatting-cell (pane)
                   (print (method-qualifiers method)))
                 (loop for specializer in (clim-mop:method-specializers method)
                    do (formatting-cell (pane)
			 (if (typep specializer 'clim-mop:eql-specializer)
			     (progn
			       (princ "(EQL " pane)
			       (inspect-object
				(clim-mop:eql-specializer-object
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

(defmethod inspect-object ((object function) pane)
  (with-output-as-presentation
       (pane object (presentation-type-of object))
    (format pane "Function: ~A"
	    (pretty-print-function object))
    #+sbcl
    (unless (typep object 'generic-function)
      (format pane "~&Type: ~A"
	      (sb-kernel:%simple-fun-type (sb-kernel:%closure-fun object))))
    (print-documentation object pane)))

(defmethod inspect-object-briefly ((object package) pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (format pane "Package: ~S" (package-name object))))
(defmethod inspect-object ((object package) pane)
  (inspector-table
    (format pane "Package: ~S" (package-name object))
    (inspector-table-row
      (princ "Name:" pane)
      (inspect-object (package-name object) pane))
    (inspector-table-row
      (princ "Nicknames:" pane)
      (dolist (nick (package-nicknames object))
        (inspect-object nick pane)))
    (inspector-table-row
      (princ "Used by:")
      (dolist (used-by (package-used-by-list object))
          (inspect-object used-by pane)))
    (inspector-table-row
      (princ "Uses:")
      (dolist (uses (package-use-list object))
          (inspect-object uses pane)))))

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
    (print object)))

(defmethod inspect-object-briefly ((object number) pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (print object)))

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
  (inspector-table
    (format pane "float ~S" object)
    (multiple-value-bind (significand exponent sign)
        (decode-float object)
      (inspector-table-row
        (princ "sign:")
        (inspect-object sign pane))
      (inspector-table-row
        (princ "significand:")
        (inspect-object significand pane))
      (inspector-table-row
        (princ "exponent:")
        (inspect-object exponent pane)))
    (inspector-table-row
       (princ "radix:")
       (inspect-object (float-radix object) pane))))

(defmethod inspect-object-briefly ((object symbol) pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (prin1 object)))

(defmethod inspect-object ((object symbol) pane)
  (inspector-table
    (format pane "Symbol ~S" (symbol-name object))
    (inspector-table-row
       (princ "value:")
       (if (boundp object)
         (inspect-object (symbol-value object) pane)
         (princ "unbound")))
    (inspector-table-row
       (princ "function:")
       (if (fboundp object)
         (inspect-object (symbol-function object) pane)
         (princ "unbound")))
    ;; This is not, strictly speaking, a property of the
    ;; symbol. However, this is useful enough that I think it's worth
    ;; including here, since it can eliminate some minor annoyances.
    (inspector-table-row
       (princ "class:")
       (if (find-class object nil)
         (inspect-object (find-class object) pane)
         (princ "unbound")))
    (inspector-table-row
       (princ "package:")
       (inspect-object (symbol-package object) pane))
    (inspector-table-row
       (princ "propery list:")
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
  (inspector-table
    (format pane "Character ~S" object)
    (inspector-table-row
       (princ "code:" pane)
       (inspect-object (char-code object) pane))
    (inspector-table-row
       (princ "int:" pane)
       (inspect-object (char-int object) pane))
    (inspector-table-row
       (princ "name:" pane)
       (inspect-object (char-name object) pane))))

(defun display-app (frame pane)
  "Display the APP frame of the inspector"
  (inspect-object (obj frame) pane))

(define-inspector-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-inspector-command (com-inspect :name t) ()
  (let ((obj (accept t :prompt "Select an object")))
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
  (remove-method (clim-mop:method-generic-function obj) obj))

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
  (let ((slot-object (find slot (clim-mop:class-direct-slots class)
			   :key #'clim-mop:slot-definition-name)))
    (if slot-object
	(documentation slot-object t)
	(when (clim-mop:class-direct-superclasses class)
	  (find-if #'identity
		   (mapcar #'(lambda (class)
			       (slot-documentation class slot))
			   (clim-mop:class-direct-superclasses class)))))))

(define-inspector-command (com-describe-slot :name t)
    ((slot 'settable-slot :gesture :describe :prompt "Describe slot"))
  (destructuring-bind (object . slot-name) slot
    (let* ((stream (get-frame-pane *application-frame* 'int))
	   (class (class-of object))
	   (documentation (handler-bind ((warning #'muffle-warning))
			    (slot-documentation class slot-name)))
	   (slot-object (or (find slot-name (clim-mop:class-direct-slots class)
                                  :key #'clim-mop:slot-definition-name)
                            (find slot-name (clim-mop:class-slots class)
                                  :key #'clim-mop:slot-definition-name))))
      (when documentation
	(format stream "~&Documentation: ~A~%" documentation))
      (format stream "~&Type: ~S~%"
	      (clim-mop:slot-definition-type slot-object))
      (format stream "~&Allocation: ~S~%"
	      (clim-mop:slot-definition-allocation slot-object))
      ;; slot-definition-{readers,writers} only works for direct slot
      ;; definitions
      (let ((readers (clim-mop:slot-definition-readers slot-object)))
        (when readers
          (format stream "~&Readers: ")
          (present readers (presentation-type-of readers) :stream stream)))
      (let ((writers (clim-mop:slot-definition-writers slot-object)))
        (when writers
          (format stream "~&Writers: ")
          (present writers (presentation-type-of writers) :stream stream))))))

