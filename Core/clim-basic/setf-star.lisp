;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2001 by Tim Moore <moore@bricoworks.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

(defun setf-name-p (name)
  (typep name '(cons (eql setf) (cons symbol null))))

;;; Many implementations complain if a defsetf definition and a setf function
;;; exist for the same place. Time to stop fighting that...

(defun make-setf*-gfn-name (function-name)
  (let* ((name-sym (cadr function-name)))
    `(setf ,(intern (format nil ".~A-~A."
                            (symbol-name name-sym)
                            (symbol-name '#:star))
                    (symbol-package name-sym)))))

(defmacro defgeneric* (fun-name lambda-list &body options)
  "Defines a SETF* generic function.  FUN-NAME is a SETF function
name.  The last required argument is the single argument to the function in a
SETF place form; the other arguments are values collected from the
SETF new value form."
  (unless (setf-name-p fun-name)
    (error "~S is not a valid name for a SETF* generic function." fun-name))
  (multiple-value-bind (required optional rest keys allow-other-keys aux keyp)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (when aux
      (error "The use of &aux is not allowed in generic function lambda list."))
    (let ((setf-name (second fun-name))
          (gf (make-setf*-gfn-name fun-name))
          (args (butlast required))
          (place (alexandria:lastcar required))
          (fun-opts (gensym "FUN-OPTS"))
          (fun-keys (gensym "FUN-KEYS")))
      (multiple-value-bind (opt-args call-opt)
          (loop for (opt) in optional
                for suppliedp = (gensym)
                collect `(,opt nil ,suppliedp) into opt-args
                collect `(and ,suppliedp `(,,opt)) into call-opt
                finally (return (values opt-args call-opt)))
        (multiple-value-bind (key-args call-key)
            (loop for ((key arg)) in keys
                  for suppliedp = (gensym)
                  collect `((,key ,arg) nil ,suppliedp) into key-args
                  collect `(and ,suppliedp `(,,key ,,arg)) into call-key
                  finally (return (values key-args call-key)))
          `(progn
             (defsetf ,setf-name
                 (,place
                  ,@(and opt-args `(&optional ,@opt-args))
                  ,@(and rest `(&rest ,rest))
                  ,@(and keyp `(&key ,@key-args))
                  ,@(and allow-other-keys `(&allow-other-keys)))
                 ,args
               (let ((,fun-opts (append ,@call-opt))
                     (,fun-keys (append ,@call-key)))
                 `(funcall (function ,',gf) ,,@args ,,place ,@,fun-opts ,@,fun-keys)))
             (defgeneric ,gf ,lambda-list ,@options)))))))

(defmacro defmethod* (name lambda-list &body body)
  "Defines a SETF* method.  NAME is a SETF function name.  Otherwise,
like DEFMETHOD except there must exist a corresponding DEFGENERIC* form."
  (unless (setf-name-p name)
    (error "~S is not a valid name for a SETF* generic function." name))
  `(defmethod ,(make-setf*-gfn-name name) ,lambda-list ,@body))
