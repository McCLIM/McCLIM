;;; -*- Syntax: Common-lisp; Package: GRAPH -*-
#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(in-package :graph)

;;; All the exports of graph in one place.

;;; Functions and methods
(eval-when (compile load eval)
  (export '(make-demo-frame
	    view-graphs
	    display-graph
	    save-postscript-graph
	    display-graphs
	    graph-presentation-type
	    presentation
	    graph-under-presentation
	    present-self-p
	    fill-window-with-graphs
	    graphs-for-slider
	    autoscale-graphs
	    auto-scale-needed
	    auto-scale
	    graph-auto-scale-limits
	    display-data
	    display-datum
	    displayed?
	    datum-position
	    device-draw-line
	    line-style
	    thickness
	    symbologies
	    graph-p
	    graph-data-p
	    map-data
	    map-data-xy
	    missing-data-threshold
	    display
	    erase
	    move
	    refresh
	    zoom-stack
	    copy
	    dump-forms
	    final-dump
	    duplicator-methods
	    duplicate-slots
	    pop-accept-items
	    pop-accept-label
	    popup-accept-forms
	    popup-accept
	    popup-accept-standard-loop
	    graph-under-mouse
	    add-dataset
	    datasets
	    data
	    define-graph-command
	    duplicator-methods
	    xy-inside
	    set-xy-inside
	    xy-to-uv
	    xy-to-stream
	    uv-to-xy
	    screen-to-uv
	    uv-to-screen
	    name
	    annotation
	    point-annotation
	    interval-annotation
	    annotate
	    annotate-graph
	    annotate-interval
	    annotate-point
	    annotate-data-interval
	    annotate-data-point
	    description-choices
	    default-text-style
	    x-label
	    y-label
	    )
	  'graph))

;;; color stuff
(eval-when (compile load eval)
  (export '(color
	    color-presentation
	    alu
	    alu-for-color
	    alu-for-stream
	    *color-specifications*
	    device-draw-aluf
	    device-line-thickness
	    device-line-end-shape
	    device-line-joint-shape 
	    device-filled-p
	    device-fill-pattern
	    %draw
	    %erase
	    %flip
	    initialize-color-system)))

;;; classes of data
(eval-when (compile load eval)
  (export '(graph-data
	    timeseries-data
	    presentable-data-mixin
	    graph-data-limits-mixin
	    graph-data-auto-scale-mixin
	    graph-data-color-mixin
	    graph-data-symbology-mixin
	    graph-data-add-datum-mixin
	    presentable-graph-data-legend-mixin
	    graph-data-legend-mixin
	    basic-list-datum-mixin
	    graph-data-list-map-mixin
	    essential-graph-data-map-mixin
	    basic-graph-data
	    equation-data
	    sample-data
	    histogram-data
	    MULTIDIMENSIONAL-DATA)
	  'graph))

;;; classes of graphs
(eval-when (compile load eval)
  (export '(graph
	    annotated-graph
	    presentable-graph-mixin
	    graph-datasets-ob-mixin
	    graph-datasets-mixin
	    graph-legend-mixin
	    graph-relative-size-mixin
	    graph-zoom-mixin
	    graph-slider-interaction-mixin
	    graph-slider-mixin
	    graph-handle-mouse-mixin
	    graph-mouse-resolution-mixin
	    graph-auto-scale-ob-mixin
	    graph-auto-scale-extensions-ob-mixin
	    graph-auto-scale-extensions-mixin
	    graph-limits-mixin			
	    graph-auto-scale-mixin
	    graph-grid-ob-mixin
	    graph-grid-mixin
	    horizontal-y-border-mixin
	    vertical-y-border-mixin
	    graph-border-ob-mixin
	    graph-border-mixin
	    basic-graph-ob-mixin
	    basic-graph
	    graph-with-reselectable-axes
	    named-object
	    named-mixin)
	  'graph))


