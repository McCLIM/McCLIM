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

(duplicator-methods (raw-graph-data)
  ((duplicate-slots data)
   ))

(duplicator-methods (accessor-datum-mixin)
  ((duplicate-slots x-accessor y-accessor)))

(duplicator-methods (graph-data-x-offset-mixin)
  ((duplicate-slots x-offset)))

(duplicator-methods (graph-data-y-offset-mixin)
  ((duplicate-slots y-offset)))

(duplicator-methods (graph-data-xy-offset-mixin)
  ((duplicate-slots x-offset y-offset)))

(duplicator-methods (graph-data-dither-mixin)
  ((duplicate-slots x-dither y-dither)))

(duplicator-methods (graphics-style-mixin)
  ((duplicate-set pattern pattern)
   (duplicate-slots thickness line-end-shape)))

(duplicator-methods (basic-graph-datum-symbology-mixin)
  ((duplicate-slots symbologies)))

(duplicator-methods (graph-datum-line-symbology-mixin)
  ((duplicate-slots line-style)))

(duplicator-methods (graph-datum-bar-symbology-mixin)
  ((duplicate-slots bar-width)))

(duplicator-methods (graph-datum-scatter-symbology-mixin)
  ((duplicate-slots data-symbol symbol-height)))

(duplicator-methods (graph-data-color-mixin)
  ((duplicate-slots color)))

(duplicator-methods (graph-data-auto-scale-mixin)
  ((duplicate-slots auto-scale?)))

;;; Copying or dumping the alu is unnecessary if dataset has a COLOR that
;;; determines the ALU so provide your own versions of these if you need to.
(defmethod dump-forms append ((self basic-graph-data))
  (with-slot-dumping (self) (dump-slots graph)))

;;;******************************************************************
;;; From BASIC-GRAPH.lisp.
;;;******************************************************************
(duplicator-methods (uv-box)
  ((duplicate-slots ull vll uur vur)))

(duplicator-methods (xy-box)
  ((duplicate-slots xll yll xur yur)))

(duplicator-methods (basic-graph-coordinates-mixin)
  ((duplicate-slots x-u-scale y-v-scale)))

(defmethod copy-inner-class progn
	   ((SELF essential-display-mixin) COPY-OBJECT COPY-HTABLE)
  (with-slot-copying (COPY-OBJECT COPY-HTABLE self)
    (copy-slots DISPLAYED?))) 

(defmethod dump-forms append ((SELF essential-display-mixin))
  (with-slot-dumping (self) (dump-set-slot DISPLAYED? nil)))

(duplicator-methods (basic-graph)
  ((duplicate-slots edge-spec)))

(duplicator-methods (named-object)
  ((duplicate-slots name)))

(defmethod copy-inner-class progn ((SELF equation-data) COPY-OBJECT COPY-HTABLE)
  (with-slots (DATA-FUNCTION) SELF
    (with-slot-copying (COPY-OBJECT COPY-HTABLE self)
      (copy-slots VARIABLE MIN MAX INCREMENT PARAMETERS EQUATION)
      (copy-set-slot DATA-FUNCTION DATA-FUNCTION))))

(defmethod dump-forms append ((SELF equation-data))
  (with-slot-dumping (self)
    (dump-slots VARIABLE MIN MAX INCREMENT PARAMETERS EQUATION)))

(defmethod copy-inner-class progn ((SELF histogram-data) COPY-OBJECT COPY-HTABLE)
  (with-slot-copying (COPY-OBJECT COPY-HTABLE self)
    (copy-slots MIN MAX BIN-COUNT BIN-SIZE BINS)))

(defmethod dump-forms append ((SELF histogram-data))
  (with-slot-dumping (self)
    (dump-slots MIN MAX BIN-COUNT BIN-SIZE)))

(duplicator-methods (line-mixin)
  ((duplicate-slots line-a line-b line-c)))

(duplicator-methods (graph-mouse-resolution-mixin)
  ((duplicate-slots dx-mouse dy-mouse)))

(duplicator-methods (graph-border-mixin)
  ((duplicate-slots show-border tick-size title
		    x-label x-digits x-auto-tick x-dtick
		    y-label y-digits y-auto-tick y-dtick)))

(duplicator-methods (graph-grid-mixin)
  ((duplicate-slots show-grid x-auto-grid x-dgrid y-auto-grid y-dgrid)))

(duplicator-methods (graph-datasets-mixin)
  ((duplicate-slots datasets)))

(duplicator-methods (graph-auto-scale-mixin)
  ((duplicate-slots auto-scale-needed auto-scale)))

(duplicator-methods (graph-auto-scale-extensions-mixin)
  ((duplicate-slots auto-scale-extensions)))

(duplicator-methods (show-legend-mixin)
  ((duplicate-slots show-legend)))

(duplicator-methods (basic-slider)
  ((duplicate-slots graphs bounds x-increment y-increment x y dx dy
		lastx lasty x? y?)))

