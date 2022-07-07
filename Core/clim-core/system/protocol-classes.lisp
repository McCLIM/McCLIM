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

;;;; Part V: Extended Stream Output Facilities

;; CLIM Specification says that E-O-S is a subclass of OUTPUT-STREAM,
;; but it does not say what is it. We infer it is a base class for
;; all CLIM output streams (output-recording-stream included).
(defclass output-stream (fundamental-character-output-stream) ())

;;; 15.2 Extended Output Streams
(define-protocol-class extended-output-stream
    (output-stream)
  ())

;;; 15.3 The Text Cursor
(define-protocol-class cursor ())

;;; 16.2 Output Records
(define-protocol-class output-record (bounding-rectangle)
  ())

(define-protocol-class displayed-output-record (output-record)
  ())

;;; 16.3.2 Graphics Displayed Output Records
(define-protocol-class graphics-displayed-output-record
    (displayed-output-record)
  ())

;;; 16.3.3 Text Displayed Output Record
(define-protocol-class text-displayed-output-record (displayed-output-record)
  ())

;;; 16.4 Output Recording Streams
(define-protocol-class output-recording-stream (output-stream)
  ())

;;; 17.3.1 Table Formatting Protocol
(define-protocol-class table-output-record (output-record))

;;; 17.3.2 Row and Column Formatting Protocol
(define-protocol-class row-output-record (output-record))
(define-protocol-class column-output-record (output-record))

;;; 17.3.3 Cell Formatting Protocol
(define-protocol-class cell-output-record (output-record))

;;; 17.3.4 Item List Formatting Protocol
(define-protocol-class item-list-output-record ()
  ())

;;; 18.2 The Graph Formatting Protocol
(define-protocol-class graph-output-record (output-record))
(define-protocol-class graph-node-output-record (output-record))

;;; 21.3 Incremental Redisplay Protocol
(define-protocol-class updating-output-record (output-record))


;;;; Part VI: Extended Stream Input Facilities

;; CLIM Specification says that E-I-S is a subclass of INPUT-STREAM,
;; but it does not say what is it. We infer it is a base class for
;; all CLIM input streams (standard-input-stream included).
(defclass input-stream (fundamental-input-stream) ())

;;; 22.2 Extended Input Streams

(define-protocol-class extended-input-stream (input-stream))

;;; 22.4 The Pointer Protocol

(define-protocol-class pointer ())

;;; 23.2 Presentations
(define-protocol-class presentation ())

;;; 23.6 Views
(define-protocol-class view ())

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


;;;; Part VIII: Appendices

;;; C.1 Encapsulating Streams
(define-protocol-class encapsulating-stream ())
