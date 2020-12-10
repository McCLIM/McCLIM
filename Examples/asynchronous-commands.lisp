;;; ---------------------------------------------------------------------------
;;;   License: BSD-2-Clause.
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; A testing application for the function EXECUTE-FRAME-COMMAND.
;;;

(defpackage #:clim-demo.execute-frame-command
  (:use #:clim-lisp #:clim)
  (:export #:run #:homogenous))
(in-package #:clim-demo.execute-frame-command)

(defun display (frame pane)
  (format pane "- left click on a frame to increase its counter~%")
  (format pane "- middle click on a frame to increase its counter after 1s~%")
  (terpri pane)
  (formatting-table (pane)
    (formatting-row (pane)
      (with-drawing-options (pane :ink +dark-red+)
        (formatting-cell (pane) (format pane "This frame"))
        (formatting-cell (pane) (present frame 'homogenous :stream pane))))
    (alexandria:when-let ((parent (parent frame)))
      (formatting-row (pane)
        (with-drawing-options (pane :ink +dark-blue+)
          (formatting-cell (pane) (format pane "Parent"))
          (formatting-cell (pane) (present parent 'homogenous :stream pane)))))
    (alexandria:when-let ((children (children frame)))
      (formatting-row (pane)
        (formatting-cell (pane)
          (format pane "Children"))
        (formatting-cell (pane)
          (format-textual-list children
                               (lambda (item stream)
                                 (present item 'homogenous :stream stream))
                               :separator #\newline))))))

(define-gesture-name :immediate-incf :pointer-button-press (:left))
(define-gesture-name :delayed-incf :pointer-button-press (:middle))

(define-application-frame homogenous ()
  ((counter  :initform 0   :accessor counter)
   (children :initform nil :accessor children)
   (parent   :initform nil :accessor parent :initarg :parent))
  (:geometry :width 800 :height 600)
  (:pane
   :application
   :display-function 'display
   :scroll-bars nil :borders nil))

(defmethod disown-frame :before (fm (frame homogenous))
  (mapc (lambda (child)
          (setf (parent child) nil)
          (execute-frame-command child '(com-refresh)))
        (children frame))
  (alexandria:when-let ((parent (parent frame)))
    (setf (children parent) (delete frame (children parent)))
    (execute-frame-command parent '(com-refresh))))

(defmethod print-object ((object homogenous) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~2d" (counter object))))

(define-homogenous-command com-refresh ())

(define-homogenous-command (com-add :name "New child" :menu t) ()
  (let* ((parent-frame *application-frame*)
         (frame (make-application-frame 'homogenous :parent parent-frame)))
    (push frame (children parent-frame))
    (clim-sys:make-process (lambda () (run-frame-top-level frame)))))

(define-homogenous-command com-real-incf
    ((dx 'integer))
  (let ((frame *application-frame*))
    (incf (counter frame) dx)
    (mapc (lambda (child)
            (execute-frame-command child '(com-refresh)))
          (children frame))
    (alexandria:when-let ((parent (parent frame)))
      (execute-frame-command parent '(com-refresh)))))

(define-homogenous-command com-incf
    ((frame 'homogenous :gesture :immediate-incf))
  (execute-frame-command frame `(com-real-incf 1)))

(defclass scheduled-incf-event (clim:device-event) ())

(defmethod handle-event ((sheet clime:top-level-sheet-mixin)
                         (event scheduled-incf-event))
  (execute-frame-command (pane-frame sheet) `(com-real-incf 1)))

(define-homogenous-command com-delayed-incf
    ((frame 'homogenous :gesture :delayed-incf))
  (clime:schedule-event
   (frame-top-level-sheet frame)
   (make-instance 'scheduled-incf-event
                  :sheet (frame-top-level-sheet frame))
   1))

(defun run (&key (new-process t))
  (let* ((frame (make-application-frame 'homogenous))
         (start (alexandria:curry #'run-frame-top-level frame)))
    (if new-process
        (clim-sys:make-process start)
        (funcall start))
    frame))
