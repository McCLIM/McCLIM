;;; -*- Mode: Lisp; Package: INSPECTOR -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2005 by
;;;           Vincent Arkesteijn

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

(in-package :inspector)

(define-application-frame inspector ()
  ((dico :initform (make-hash-table :test #'eq) :reader dico)
   (obj :initarg :obj :reader obj))
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

(defun inspector (obj)
  (let ((*print-length* 10)
	(*print-level* 10))
    (run-frame-top-level
     (make-application-frame 'inspector :obj obj))))

(defparameter *inspected-objects* '())

(defun currently-viewable (obj)
  (multiple-value-bind (value present)
      (gethash obj (dico *application-frame*))
    (if present
        value
        (setf (gethash obj
                       (dico *application-frame*))
              (or (symbolp obj)
                  (numberp obj)
                  (stringp obj))))))

(defgeneric inspect-object (object pane))

(defmethod inspect-object :around (object pane)
  (cond ((not (currently-viewable object))
	 (with-output-as-presentation
	     (pane object (presentation-type-of object)) 
	   (princ "...")))
	((member object *inspected-objects*)
	 (with-output-as-presentation
	     (pane object (presentation-type-of object)) 
	   (princ "===")))
	(t
	 (let ((*inspected-objects* (cons object *inspected-objects*)))
	   (call-next-method)))))

(defmethod inspect-object (object pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))  
    (print object pane)))

(defun class-slots (class)
  #+sbcl (sb-mop:class-slots class)
  #+openmcl (ccl:class-slots class)
  #-(or sbcl openmcl) (error "no MOP"))

(defun slot-definition-name (slot)
  #+sbcl (sb-mop:slot-definition-name slot)
  #+openmcl (ccl:slot-definition-name slot)
  #-(or sbcl openmcl) (error "no MOP"))

(defun generic-function-name (generic-function)
  #+sbcl (sb-mop:generic-function-name generic-function)
  #+openmcl (ccl:generic-function-name generic-function)
  #-(or sbcl openmcl) (error "no MOP"))

(defun generic-function-methods (generic-function)
  #+sbcl (sb-mop:generic-function-methods generic-function)
  #+openmcl (ccl:generic-function-methods generic-function)
  #-(or sbcl openmcl) (error "no MOP"))

(defun method-specializers (method)
  #+sbcl (sb-mop:method-specializers method)
  #+openmcl (ccl:method-specializers method)
  #-(or sbcl openmcl) (error "no MOP"))

(defun method-generic-function (method)
  #+sbcl (sb-mop:method-generic-function method)
  #+openmcl (ccl:method-generic-function method)
  #-(or sbcl openmcl) (error "no MOP"))

(define-presentation-type settable-slot ()
  :inherit-from t)

(define-presentation-method present (object (type settable-slot) 
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format stream "~s" (cdr object)))

(defmethod inspect-object ((object standard-object) pane)
  (let ((class (class-of object)))
    (with-output-as-presentation
	(pane object (presentation-type-of object))  
      (formatting-table (pane)
	(formatting-row (pane)
	  (formatting-cell (pane)
	    (surrounding-output-with-border (pane)
	      (print (class-name class) pane))))
	(formatting-row (pane)
	  (formatting-cell (pane)
	    (formatting-table (pane)
	      (loop for slot in (reverse (class-slots class))
		    do (let ((slot-name (slot-definition-name slot)))
			 (formatting-row (pane)
			   (formatting-cell (pane :align-x :right)
			     (with-output-as-presentation
				 (pane (cons object slot-name) 'settable-slot)
			       (format pane "~a:" slot-name)))
			   (formatting-cell (pane)
			     (inspect-object (slot-value object slot-name) pane))))))))))))

(defmethod inspect-object ((object cons) pane)
  (if (null (cdr object))
      (formatting-table (pane)
	(formatting-column (pane)
	  (formatting-cell (pane)
            (with-output-as-presentation
                (pane object (presentation-type-of object))
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
		      (pane object (presentation-type-of object))
		    (draw-rectangle* pane 0 0 20 10 :filled nil))
		  (draw-line* pane 10 0 10 10)
		  (draw-arrow* pane 5 5 5 30)
		  (draw-arrow* pane 15 5 40 5))
		(formatting-cell (pane)
		  (inspect-object (car object) pane)))))
	  (formatting-cell (pane)
	    (inspect-object (cdr object) pane))))))

(defmethod inspect-object ((object hash-table) pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (formatting-table (pane)
      (formatting-column (pane)
        (formatting-cell (pane)
          (surrounding-output-with-border (pane)
            (format pane "~A (test: ~A)" 'hash-table (hash-table-test object))))
        (formatting-cell (pane)
          (formatting-table (pane)
            (loop for key being the hash-keys of object
               do (formatting-row (pane)
                    (formatting-cell (pane :align-x :right)
                      (inspect-object key pane)
                      (princ "=" pane))
                    (formatting-cell (pane)
                      (inspect-object (gethash key object) pane))))))))))

(defmethod inspect-object ((object generic-function) pane)
  (with-output-as-presentation
      (pane object (presentation-type-of object))
    (formatting-table (pane)
      (formatting-row (pane)
	(formatting-cell (pane)
	  (surrounding-output-with-border (pane)
	    (format pane "Generic Function: ~s" (generic-function-name object)))))
      (formatting-row (pane)
	(formatting-cell (pane)
	  (formatting-table (pane)
	    (loop for method in (generic-function-methods object)
		  do (with-output-as-presentation
			 (pane method (presentation-type-of method))
		       (formatting-row (pane)
			 (formatting-cell (pane)
			   (print (method-qualifiers method)))
			 (loop for specializer in (method-specializers method)
			       do (formatting-cell (pane)
				    (format pane "~s " (class-name specializer)))))))))))))

(defun display-app (frame pane)
  (inspect-object (obj frame) pane))

(define-inspector-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-inspector-command (com-inspect :name t) ()
  (let ((obj (accept t :prompt "Select an object")))
    (clim-sys:make-process #'(lambda () (inspector obj))
			   :name "inspector")))

(define-inspector-command (com-toggle-inspect :name t)
    ((obj t :gesture :select :prompt "Select an object"))
  (unless (eq obj (obj *application-frame*))
  (setf (gethash obj (dico *application-frame*))
        (not (gethash obj (dico *application-frame*))))))

(define-inspector-command (com-remove-method :name t)
    ((obj 'method :gesture :delete :prompt "Remove method"))
  (remove-method (method-generic-function obj) obj))

(define-inspector-command (com-set-slot :name t)
    ((slot 'settable-slot :gesture :select :prompt "Set slot"))
  (setf (slot-value (car slot) (cdr slot))
	(accept t :prompt "New slot value")))
