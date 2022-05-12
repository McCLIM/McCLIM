;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;; This file contains CLIM macros. When If it is necessary to use the macro
;;; before a certain module is implemented, then that macro should be put
;;; here. General utilities do not belong here.

(in-package #:clim-internals)

;;; Move this early so it can be used in presentations.lisp, which comes
;;; before commands.lisp.
(defmacro do-command-table-inheritance ((command-table-var command-table)
                                        &body body)
  `(apply-with-command-table-inheritance
    #'(lambda (,command-table-var)
        ,@body)
    (find-command-table ,command-table)))

;;; Repaint protocol may use that.
(defmacro with-output-buffered ((medium &optional (buffer-p t)) &body body)
  (with-gensyms (cont)
    `(flet ((,cont () ,@body))
       (declare (dynamic-extent (function ,cont)))
       (invoke-with-output-buffered ,medium (function ,cont) ,buffer-p))))
