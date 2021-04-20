;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2001 by Tim Moore <moore@bricoworks.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-demo)

(define-application-frame summation ()
  ((total :accessor summation-total :initarg :total :initform 0))
  (:menu-bar nil)
  (:panes
   (tester :interactor))
  (:layouts
   (default (vertically () tester)))
  (:top-level (summation-top-level)))

(defun summation-top-level (frame &key
                            command-parser command-unparser
                            partial-command-parser prompt)
  (declare (ignore command-parser command-unparser
                   partial-command-parser prompt))
  (let ((*standard-output* (frame-standard-output frame))
        (*standard-input* (frame-standard-input frame))
        (*print-pretty* nil))
    (setf (cursor-visibility (stream-text-cursor *standard-input*)) t)
    (present "Hallo" 'string)
    (loop
     (climi::catch-abort-gestures ("Return to ~A command level"
                                   (frame-pretty-name frame))
       (present (summation-total frame) 'real)
       (fresh-line)
       (let ((new-val (accept 'real
                              :default (summation-total frame)
                              :default-type 'real)))
         (fresh-line)
         (incf (summation-total frame) new-val))))))
