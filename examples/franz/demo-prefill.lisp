;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: demo-prefill.lisp,v 1.9 92/12/16 16:47:29 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."

;;; This file prefills generic function dispatch caches at load time so that
;;; there won't be so much delay starting things up the first time the application
;;; is run.  This file contains the things that aren't in CLIM:CLIM;PREFILL because
;;; they pertain to particular demos.


;;; (generate-prefill-dispatch-caches 'bounding-rectangle)

(prefill-dispatch-caches
  (accept-method
    (clim-demo::output t t t t textual-view))
  (clim-demo::add-new-object
    (clim-demo::cad-demo clim-demo::output)
    (clim-demo::cad-demo clim-demo::logic-zero)
    (clim-demo::cad-demo clim-demo::and-gate)
    (clim-demo::cad-demo clim-demo::input)
    (clim-demo::cad-demo clim-demo::or-gate)
    (clim-demo::cad-demo clim-demo::logic-one))
  (add-output-record
    (t clim-demo::cad-demo))
  (bounding-rectangle*
    (clim-demo::cad-demo)
    (clim-demo::output)
    (clim-demo::input)
    (clim-demo::or-gate)
    (clim-demo::and-gate)
    (clim-demo::logic-zero)
    (clim-demo::logic-one))
  (bounding-rectangle-set-edges
    (clim-graphics-editor::box t t t t))
  ((setf clim-graphics-editor::box-arrow-in)
    (t clim-graphics-editor::box))
  ((setf clim-graphics-editor::box-arrow-out)
    (t clim-graphics-editor::box))
  (command-enabled
    (t clim-demo::cad-demo))
  (clim-graphics-editor::compute-object-handles
    (clim-graphics-editor::box))
  (clim-demo::connection-component
    (clim-demo::input)
    (clim-demo::output))
  (clim-demo::connection-early-p
    (clim-demo::input))
  (clim-demo::connection-other-connections
    (clim-demo::output)
    (clim-demo::input))
  ((setf clim-demo::connection-other-connections)
    (t clim-demo::output))
  (clim-demo::connection-value
    (clim-demo::output)
    (clim-demo::or-gate)
    (clim-demo::input)
    (clim-demo::logic-one)
    (clim-demo::and-gate)
    (clim-demo::logic-zero))
  (default-frame-top-level
    (clim-demo::cad-demo))
  (clim-graphics-editor::delete-object
    (clim-graphics-editor::graphics-editor clim-graphics-editor::arrow)
    (clim-graphics-editor::graphics-editor clim-graphics-editor::box))
  (disable-frame
    (clim-demo::cad-demo))
  (displayed-output-record-p
    (clim-demo::input)
    (clim-demo::or-gate)
    (clim-demo::and-gate)
    (clim-demo::output))
  (clim-demo::draw-body
    (clim-demo::or-gate t)
    (clim-demo::and-gate t)
    (clim-demo::logic-zero t)
    (clim-demo::logic-one t)
    (clim-demo::output t)
    (clim-demo::input t))
  (clim-demo::draw-connections
    (clim-demo::or-gate t)
    (clim-demo::and-gate t)
    (clim-demo::logic-zero t)
    (clim-demo::logic-one t))
  (clim-graphics-editor::draw-object
    (clim-graphics-editor::box t)
    (clim-graphics-editor::arrow t))
  (clim-graphics-editor::draw-object-handles
    (clim-graphics-editor::box t))
  (clim-demo::draw-self
    (clim-demo::output t)
    (clim-demo::input t)
    (clim-demo::or-gate t)
    (clim-demo::and-gate t)
    (clim-demo::logic-zero t)
    (clim-demo::logic-one t))
  (clim-demo::draw-wires
    (clim-demo::or-gate t)
    (clim-demo::and-gate t)
    (clim-demo::logic-zero t)
    (clim-demo::logic-one t))
  (enable-frame
    (clim-demo::cad-demo))
  (execute-frame-command
    (clim-demo::cad-demo t))
  (find-or-make-pane-named
    (clim-demo::cad-demo t))
  (frame-command-table
    (clim-demo::cad-demo))
  (frame-document-highlighted-presentation
    (clim-demo::cad-demo t t t t t t))
  (frame-error-output
    (clim-demo::cad-demo))
  (frame-exit
    (clim-demo::cad-demo))
  (frame-find-innermost-applicable-presentation
    (clim-demo::cad-demo t t t t))
  (frame-input-context-button-press-handler
    (clim-demo::cad-demo t t))
  (frame-maintain-presentation-histories
    (clim-demo::cad-demo))
  (frame-panes
    (clim-demo::cad-demo))
  ((setf frame-panes)
    (t clim-demo::cad-demo))
  (frame-pointer-documentation-output
    (clim-demo::cad-demo))
  (frame-pretty-name
    (clim-demo::cad-demo))
  (frame-properties
    (clim-demo::cad-demo))
  (frame-replay
    (clim-demo::cad-demo t))
  ((setf frame-shell)
    (t clim-demo::cad-demo))
  (frame-standard-input
    (clim-demo::cad-demo))
  (frame-standard-output
    (clim-demo::cad-demo))
  (frame-top-level-sheet
    (clim-demo::cad-demo))
  ((setf frame-top-level-sheet)
    (t clim-demo::cad-demo))
  (frame-wrapper
    #+Genera (genera-clim::genera-frame-manager clim-demo::cad-demo t))
  (generate-panes
    #+Genera (genera-clim::genera-frame-manager clim-demo::cad-demo))
  (get-frame-pane
    (clim-demo::cad-demo t))
  (graft
    (clim-demo::cad-demo))
  (highlight-output-record
    (clim-demo::input t t)
    (clim-demo::or-gate t t)
    (clim-demo::and-gate t t)
    (clim-demo::output t t))
  (highlight-presentation-method
    (clim-graphics-editor::box t t t t t t)
    (clim-graphics-editor::arrow t t t t t t)
    (clim-graphics-editor::object-handle t t t t t t)
    (clim-demo::input t t t t t t)
    (clim-demo::component t t t t t t)
    (clim-demo::output t t t t t t))
  (layout-frame
    (clim-demo::cad-demo))
  (map-over-output-records-containing-position
    (t clim-demo::cad-demo t t)
    (t clim-demo::input t t)
    (t clim-demo::or-gate t t)
    (clim-demo::and-gate t t)
    (t clim-demo::output t t))
  (map-over-output-records-overlapping-region
    (t clim-demo::cad-demo t))
  (clim-demo::move
    (clim-demo::logic-one t t)
    (clim-demo::output t t)
    (clim-demo::logic-zero t t)
    (clim-demo::and-gate t t)
    (clim-demo::input t t)
    (clim-demo::or-gate t t))
  (clim-graphics-editor::move-handle
    (clim-graphics-editor::object-handle t t))
  (clim-graphics-editor::move-object
    (clim-graphics-editor::box t t))
  (note-frame-disabled
    #+Genera (genera-clim::genera-frame-manager clim-demo::cad-demo))
  (note-frame-enabled
    #+Genera (genera-clim::genera-frame-manager clim-demo::cad-demo))
  (clim-graphics-editor::object-handles
    (clim-graphics-editor::box))
  (clim-graphics-editor::object-style
    (clim-graphics-editor::box))
  ((setf clim-graphics-editor::object-style)
    (t clim-graphics-editor::box))
  #+Genera (clos-internals:operation-handled-p
	     (clim-demo::cad-demo t))
  (output-record-parent
    (clim-demo::input)
    (clim-demo::cad-demo)
    (clim-demo::or-gate)
    (clim-demo::and-gate)
    (clim-demo::output))
  (output-record-refined-position-test
    (clim-demo::input t t)
    (clim-demo::or-gate t t)
    (clim-demo::and-gate t t)
    (clim-demo::output t t))
  (output-record-set-position
    (clim-demo::or-gate t t)
    (clim-demo::and-gate t t))
  (output-record-start-cursor-position
    (clim-demo::cad-demo)
    (clim-demo::input)
    (clim-demo::or-gate)
    (clim-demo::and-gate)
    (clim-demo::output))
  (point-position
    (clim-graphics-editor::object-handle))
  (port
    (clim-demo::cad-demo))
  (present-method
    (clim-demo::output t t t t t pointer-documentation-view))
  (presentation-object
    (clim-demo::input)
    (clim-demo::or-gate)
    (clim-demo::and-gate)
    (clim-demo::output))
  (presentation-single-box
    (clim-demo::input)
    (clim-demo::or-gate)
    (clim-demo::and-gate)
    (clim-demo::output))
  (presentation-type
    (clim-demo::input)
    (clim-demo::or-gate)
    (clim-demo::and-gate)
    (clim-demo::output))
  (presentation-type-history-method
    (clim-demo::output t t))
  (presentationp
    (clim-demo::cad-demo)
    (clim-demo::input)
    (clim-demo::or-gate)
    (clim-demo::and-gate)
    (clim-demo::output))
  (print-object
    (clim-demo::output t))
  #+Genera (clos-internals::print-self
	     (clim-demo::output t t t))
  (read-frame-command
    (clim-demo::cad-demo))
  (region-contains-position-p
    (standard-bounding-rectangle t t)
    (clim-demo::output t t)
    (clim-demo::input t t)
    (clim-demo::or-gate t t)
    (clim-demo::and-gate t t)
    (clim-demo::logic-zero t t)
    (clim-demo::logic-one t t))
  (replay-output-record
    (clim-demo::cad-demo t)
    (clim-demo::output t)
    (clim-demo::input t)
    (clim-demo::or-gate t)
    (clim-demo::and-gate t)
    (clim-demo::logic-zero t)
    (clim-demo::logic-one t))
  (clim-graphics-editor::reshape-object
    (clim-graphics-editor::box t t t))
  (run-frame-top-level
    (clim-demo::cad-demo))
  #+Genera (clos-internals::send-if-handles
	     (gadget-output-record t))
  (shared-initialize
    (clim-demo::cad-demo t))
  (clim-demo::thing-position
    (clim-demo::output)
    (clim-demo::input))
  (clim-graphics-editor::tick-object
    (clim-graphics-editor::box)
    (clim-graphics-editor::arrow))
  (tree-recompute-extent
    (clim-demo::or-gate)
    (clim-demo::and-gate))
  #+Genera (clos-internals::which-operations
	     (clim-demo::output)))


;;; (generate-prefill-dispatch-caches 'application-frame)

(prefill-dispatch-caches
  (clim-graphics-editor::accept-graphics-editor-options
    (clim-graphics-editor::graphics-editor t))
  (clim-demo::add-new-object)
  (clim-demo::catch-ball
    (clim-demo::ico-frame))
  (command-enabled
    (t clim-graphics-editor::graphics-editor)
    (t clim-demo::ico-frame)
    (t clim-demo::plot-demo)
    (t clim-demo::thinkadot)
    (t clim-demo::address-book)
    (t clim-demo::puzzle)
    (t clim-demo::graphics-demo)
    (t clim-demo::lisp-listener)
    (t clim-demo::flight-planner))
  (default-frame-top-level
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::flight-planner))
  (clim-graphics-editor::delete-object)
  (clim-graphics-editor::deselect-object
    (clim-graphics-editor::graphics-editor t))
  (disable-frame
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  (clim-demo::display-current-address
    (clim-demo::address-book t))
  (clim-demo::display-data
    (clim-demo::plot-demo t))
  (clim-demo::display-graph
    (clim-demo::plot-demo t))
  (clim-demo::display-names
    (clim-demo::address-book t))
  (clim-graphics-editor::display-objects
    (clim-graphics-editor::graphics-editor t))
  (clim-demo::display-options
    (clim-demo::plot-demo t))
  (clim-demo::display-options-pane
    (clim-demo::ico-frame t))
  (clim-demo::draw-puzzle
    (clim-demo::puzzle t))
  (clim-demo::draw-the-display
    (clim-demo::thinkadot t))
  (enable-frame
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  (execute-frame-command
    (clim-graphics-editor::graphics-editor t)
    (clim-demo::ico-frame t)
    (clim-demo::thinkadot t)
    (clim-demo::address-book t)
    (clim-demo::puzzle t)
    (clim-demo::graphics-demo t)
    (clim-demo::flight-planner t))
  (find-or-make-pane-named
    (clim-graphics-editor::graphics-editor t)
    (clim-demo::ico-frame t)
    (clim-demo::plot-demo t)
    (clim-demo::thinkadot t)
    (clim-demo::address-book t)
    (clim-demo::puzzle t)
    (clim-demo::graphics-demo t)
    (clim-demo::lisp-listener t)
    (clim-demo::flight-planner t))
  (frame-command-table
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  (frame-document-highlighted-presentation
    (clim-graphics-editor::graphics-editor t t t t t t)
    (clim-demo::ico-frame t t t t t t)
    (clim-demo::plot-demo t t t t t t)
    (clim-demo::thinkadot t t t t t t)
    (clim-demo::address-book t t t t t t)
    (clim-demo::puzzle t t t t t t)
    (clim-demo::graphics-demo t t t t t t)
    (clim-demo::lisp-listener t t t t t t)
    (clim-demo::flight-planner t t t t t t))
  (frame-error-output
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::flight-planner))
  (frame-exit
    (clim-graphics-editor::graphics-editor)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  (frame-find-innermost-applicable-presentation
    (clim-graphics-editor::graphics-editor t t t t)
    (clim-demo::ico-frame t t t t)
    (clim-demo::plot-demo t t t t)
    (clim-demo::thinkadot t t t t)
    (clim-demo::address-book t t t t)
    (clim-demo::puzzle t t t t)
    (clim-demo::graphics-demo t t t t)
    (clim-demo::lisp-listener t t t t)
    (clim-demo::flight-planner t t t t))
  (frame-input-context-button-press-handler
    (clim-graphics-editor::graphics-editor t t)
    (clim-demo::ico-frame t t)
    (clim-demo::plot-demo t t)
    (clim-demo::thinkadot t t)
    (clim-demo::address-book t t)
    (clim-demo::puzzle t t)
    (clim-demo::graphics-demo t t)
    (clim-demo::lisp-listener t t)
    (clim-demo::flight-planner t t))
  (frame-maintain-presentation-histories
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  (frame-manager
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::flight-planner))
  (frame-panes
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  ((setf frame-panes)
    (t clim-graphics-editor::graphics-editor)
    (t clim-demo::ico-frame)
    (t clim-demo::plot-demo)
    (t clim-demo::thinkadot)
    (t clim-demo::address-book)
    (t clim-demo::puzzle)
    (t clim-demo::graphics-demo)
    (t clim-demo::lisp-listener)
    (t clim-demo::flight-planner))
  (frame-pointer-documentation-output
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  (frame-pretty-name
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  (frame-properties
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  (frame-query-io
    (clim-demo::cad-demo)
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  (frame-replay
    (clim-graphics-editor::graphics-editor t)
    (clim-demo::flight-planner t))
  (clim-graphics-editor::frame-selected-object
    (clim-graphics-editor::graphics-editor))
  ((setf frame-shell)
    (t clim-graphics-editor::graphics-editor)
    (t clim-demo::ico-frame)
    (t clim-demo::plot-demo)
    (t clim-demo::thinkadot)
    (t clim-demo::address-book)
    (t clim-demo::puzzle)
    (t clim-demo::graphics-demo)
    (t clim-demo::lisp-listener)
    (t clim-demo::flight-planner))
  (frame-standard-input
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  (frame-standard-output
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::flight-planner))
  (clim-demo::frame-target
    (clim-demo::ico-frame))
  (frame-top-level-sheet
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  ((setf frame-top-level-sheet)
    (t clim-graphics-editor::graphics-editor)
    (t clim-demo::ico-frame)
    (t clim-demo::plot-demo)
    (t clim-demo::thinkadot)
    (t clim-demo::address-book)
    (t clim-demo::puzzle)
    (t clim-demo::graphics-demo)
    (t clim-demo::lisp-listener)
    (t clim-demo::flight-planner))
  (frame-wrapper
    #+Genera (genera-clim::genera-frame-manager clim-graphics-editor::graphics-editor t)
    #+Genera (genera-clim::genera-frame-manager clim-demo::ico-frame t)
    #+Genera (genera-clim::genera-frame-manager clim-demo::plot-demo t)
    #+Genera (genera-clim::genera-frame-manager clim-demo::thinkadot t)
    #+Genera (genera-clim::genera-frame-manager clim-demo::address-book t)
    #+Genera (genera-clim::genera-frame-manager clim-demo::puzzle t)
    #+Genera (genera-clim::genera-frame-manager clim-demo::graphics-demo t)
    #+Genera (genera-clim::genera-frame-manager clim-demo::lisp-listener t)
    #+Genera (genera-clim::genera-frame-manager clim-demo::flight-planner t))
  (generate-panes
    #+Genera (genera-clim::genera-frame-manager clim-graphics-editor::graphics-editor)
    #+Genera (genera-clim::genera-frame-manager clim-demo::ico-frame)
    #+Genera (genera-clim::genera-frame-manager clim-demo::plot-demo)
    #+Genera (genera-clim::genera-frame-manager clim-demo::thinkadot)
    #+Genera (genera-clim::genera-frame-manager clim-demo::address-book)
    #+Genera (genera-clim::genera-frame-manager clim-demo::puzzle)
    #+Genera (genera-clim::genera-frame-manager clim-demo::graphics-demo)
    #+Genera (genera-clim::genera-frame-manager clim-demo::lisp-listener)
    #+Genera (genera-clim::genera-frame-manager clim-demo::flight-planner) 
    #+Genera (genera-clim::genera-frame-manager menu-frame))
  (get-frame-pane
    (clim-graphics-editor::graphics-editor t)
    (clim-demo::ico-frame t)
    (clim-demo::plot-demo t)
    (clim-demo::thinkadot t)
    (clim-demo::puzzle t)
    (clim-demo::graphics-demo t)
    (clim-demo::flight-planner t))
  (graft
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  ((setf clim-demo::ico-process)
    (t clim-demo::ico-frame))
  (clim-demo::initialize-puzzle
    (clim-demo::puzzle))
  (layout-frame
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  (map-over-output-records-containing-position)
  (map-over-output-records-overlapping-region)
  (note-frame-disabled
    #+Genera (genera-clim::genera-frame-manager clim-graphics-editor::graphics-editor)
    #+Genera (genera-clim::genera-frame-manager clim-demo::ico-frame)
    #+Genera (genera-clim::genera-frame-manager clim-demo::plot-demo)
    #+Genera (genera-clim::genera-frame-manager clim-demo::thinkadot)
    #+Genera (genera-clim::genera-frame-manager clim-demo::address-book)
    #+Genera (genera-clim::genera-frame-manager clim-demo::puzzle)
    #+Genera (genera-clim::genera-frame-manager clim-demo::graphics-demo)
    #+Genera (genera-clim::genera-frame-manager clim-demo::lisp-listener)
    #+Genera (genera-clim::genera-frame-manager clim-demo::flight-planner))
  (note-frame-enabled
    #+Genera (genera-clim::genera-frame-manager clim-graphics-editor::graphics-editor)
    #+Genera (genera-clim::genera-frame-manager clim-demo::ico-frame)
    #+Genera (genera-clim::genera-frame-manager clim-demo::plot-demo)
    #+Genera (genera-clim::genera-frame-manager clim-demo::thinkadot)
    #+Genera (genera-clim::genera-frame-manager clim-demo::address-book)
    #+Genera (genera-clim::genera-frame-manager clim-demo::puzzle)
    #+Genera (genera-clim::genera-frame-manager clim-demo::graphics-demo)
    #+Genera (genera-clim::genera-frame-manager clim-demo::lisp-listener)
    #+Genera (genera-clim::genera-frame-manager clim-demo::flight-planner))
  (clim-graphics-editor::object-selected-p
    (clim-graphics-editor::graphics-editor t))
  #+Genera (clos-internals:operation-handled-p)
  (output-record-parent)
  (output-record-start-cursor-position)
  (panep)
  (port
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  (presentationp)
  (print-object)
  #+Genera (clos-internals::print-self)
  (clim-demo::puzzle-puzzle
    (clim-demo::puzzle))
  (read-frame-command
    (accept-values)
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::flight-planner)
    (accept-values-own-window))
  (replay-output-record)
  (reset-frame)
  (run-frame-top-level
    (clim-graphics-editor::graphics-editor)
    (clim-demo::ico-frame)
    (clim-demo::plot-demo)
    (clim-demo::thinkadot)
    (clim-demo::address-book)
    (clim-demo::puzzle)
    (clim-demo::graphics-demo)
    (clim-demo::lisp-listener)
    (clim-demo::flight-planner))
  (clim-graphics-editor::select-object
    (clim-graphics-editor::graphics-editor t))
  (shared-initialize)
  (clim-demo::throw-ball
    (clim-demo::ico-frame))
  #+Genera (clos-internals::which-operations))


;;; (generate-prefill-dispatch-caches 'view)

(prefill-dispatch-caches
  (accept-method
    (clim-demo::airport t t t t textual-view))
  (accept-present-default-method)
  (display-exit-boxes)
  (frame-manager-exit-box-labels)
  (gadget-includes-prompt-p-method)
  (initialize-instance)
  (invoke-accept-values-command-button)
  (present-method
    ((presentation-type clim-demo::y-label) t t t t t textual-menu-view)
    ((presentation-type clim-demo::address-number) t t t t t textual-view)
    ((presentation-type clim-demo::printer) t t t t t textual-dialog-view)
    ((presentation-type clim-demo::printer) t t t t t textual-view)
    (clim-demo::named-intersection t t t t t iconic-view)
    (clim-demo::airport t t t t t textual-view)
    (clim-demo::airport t t t t t pointer-documentation-view)
    (clim-demo::airport t t t t t iconic-view)
    (clim-demo::vor t t t t t pointer-documentation-view)
    (clim-demo::vor t t t t t iconic-view)
    (clim-demo::ndb t t t t t iconic-view)
    (clim-demo::visual-checkpoint t t t t t iconic-view)
    (clim-demo::route t t t t t textual-view)
    (clim-demo::route t t t t t iconic-view)
    (clim-demo::aircraft t t t t t textual-dialog-view)
    (clim-demo::named-position t t t t t textual-view))
  (prompt-for-accept)
  (shared-initialize))
