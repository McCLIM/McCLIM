(in-package #:climi)

;;;; Early special variables

(defvar *application-frame* nil)
(defvar *pointer-documentation-output* nil)

;;; Command name utilities that are useful elsewhere.

(defun command-name-from-symbol (symbol)
  (let ((name (symbol-name symbol)))
    (string-capitalize
     (substitute
      #\Space #\-
      (subseq name (if (string= '#:com- name :end2 (min (length name) 4))
                       4
                       0))))))

(defun keyword-arg-name-from-symbol (item)
  (format nil "~(:~a~)" item))

;;; Move this early so it can be used in presentations.lisp, which comes before
;;; commands.lisp.
(defmacro do-command-table-inheritance ((command-table-var command-table)
                                        &body body)
  `(apply-with-command-table-inheritance
    #'(lambda (,command-table-var)
        ,@body)
    (find-command-table ,command-table)))

;;; Convenience macro
(defmacro gesture-case (event &body cases)
  (once-only (event)
    (flet ((make-match (match body)
             `((event-matches-gesture-name-p ,event ,match) ,@body)))
      `(cond ,@(loop for (match . body) in cases
                     appending (if (atom match)
                                   (list (if (member match '(otherwise t))
                                             `(t ,@body)
                                             (make-match match body)))
                                   (loop for match in match
                                         collect (make-match match body))))))))

;;; Utilities
(defun parse-method (description)
  (loop
    for (qualifier-or-ll . body) on description
    until (listp qualifier-or-ll)
    collect qualifier-or-ll into qualifiers
    finally (return
              (values qualifiers
                      (c2mop:extract-specializer-names qualifier-or-ll)
                      (c2mop:extract-lambda-list qualifier-or-ll)
                      body))))

(defun get-body-declarations (body)
  "Collect all declaration forms from a body of forms that may have
 declarations at its top. Returns as values a list of the declarations and the
 rest of the body."
  (loop for bod on body
        for (form) = bod
        if (and (consp form) (eq (car form) 'declare))
          collect form into decls
        else
          return (values decls bod)
        finally         (return (values decls nil)))) ;It's all (declare ...)


