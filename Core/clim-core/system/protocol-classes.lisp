;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2006 by Tim Moore <moore@bricoworks.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Collect all the class definitions in the Spec in one file that is compiled
;;; and loaded early.
;;;
(in-package #:clim-internals)

;;; 23.2 Presentations
(define-protocol-class presentation ())

;;; 24.1.1 The Input Editing Stream Protocol
(define-protocol-class input-editing-stream ())


;;;; Part VII: Building Applications

;;; 27.2 Command Tables
(define-protocol-class command-table ())

;;; 28.2 Application Frames
(define-protocol-class application-frame ())

;;; 28.5 Frame Managers
(define-protocol-class frame-manager () ())

;;; 29.2 Basic Pane Construction
(define-protocol-class pane (sheet))

;;; 30.3 Basic Gadget Classes
(define-protocol-class gadget (pane))
