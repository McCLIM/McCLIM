;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2005 Robert Strandh <strandh@labri.fr>
;;;  (c) copyright 2007,2008 Troels Henriksen <thenriksen@common-lisp.net>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Syntax for unknown buffer contents.  Parse contents into lines.

(in-package #:drei-fundamental-syntax)

;;; Every syntax must have a command table.

(define-syntax-command-table fundamental-table
    :errorp nil)

;;; The syntax object and misc stuff.

(define-syntax fundamental-syntax (syntax)
  ()
  (:command-table fundamental-table)
  (:name "Fundamental"))

(setf *default-syntax* 'fundamental-syntax)

;;; Update syntax

(defmethod update-syntax values-max-min ((syntax fundamental-syntax) prefix-size suffix-size
                                         &optional begin end)
  (declare (ignore begin end))
  ;; We do nothing. Technically, Fundamental syntax always parses the
  ;; entire buffer, though.
  (values 0 (size (buffer syntax))))

;;; Redisplay
;;;
;;; Just uses the default buffer-view redisplay behavior.

(defmethod pump-state-for-offset-with-syntax ((view textual-drei-syntax-view)
                                              (syntax fundamental-syntax) (offset integer))
  (buffer-view-pump-state-for-offset view offset))

(defmethod stroke-pump-with-syntax ((view textual-drei-syntax-view)
                                    (syntax fundamental-syntax) stroke
                                    pump-state)
  (buffer-view-stroke-pump view stroke pump-state))

;;; Exploit the parse

;;; do this better
(defmethod syntax-line-indentation ((syntax fundamental-syntax) mark tab-width)
  0)
