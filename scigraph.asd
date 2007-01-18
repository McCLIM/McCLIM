;;; -*- lisp -*-

(defpackage :scigraph.system
  (:use :cl :asdf))

(in-package :scigraph.system)

;;; This won't load in SBCL, either. I have really crappy code to
;;; extract dependency information from :serial t ASDF systems, but
;;; this comment is too narrow to contain it.
(defsystem :scigraph
    :depends-on (:mcclim)
    ;; The DWIM part of SCIGRAPH
    :serial t
    :components
    (
     (:file "Apps/Scigraph/dwim/package")
     (:file "Apps/Scigraph/dwim/feature-case")
     (:file "Apps/Scigraph/dwim/macros")
     (:file "Apps/Scigraph/dwim/tv")
     (:file "Apps/Scigraph/dwim/draw")
     (:file "Apps/Scigraph/dwim/present")
     (:file "Apps/Scigraph/dwim/extensions")
     (:file "Apps/Scigraph/dwim/wholine")
     (:file "Apps/Scigraph/dwim/export")
     ;; The Scigraph part
     (:file "Apps/Scigraph/scigraph/package")
     (:file "Apps/Scigraph/scigraph/copy")
     (:file "Apps/Scigraph/scigraph/dump")
     (:file "Apps/Scigraph/scigraph/duplicate")
     (:file "Apps/Scigraph/scigraph/random")
     (:file "Apps/Scigraph/scigraph/menu-tools")
     (:file "Apps/Scigraph/scigraph/basic-classes")
     (:file "Apps/Scigraph/scigraph/draw")
     (:file "Apps/Scigraph/scigraph/mouse")
     (:file "Apps/Scigraph/scigraph/color")
     (:file "Apps/Scigraph/scigraph/basic-graph")
     (:file "Apps/Scigraph/scigraph/graph-mixins")
     (:file "Apps/Scigraph/scigraph/axis")
     (:file "Apps/Scigraph/scigraph/moving-object")
     (:file "Apps/Scigraph/scigraph/symbol")
     (:file "Apps/Scigraph/scigraph/graph-data")
     (:file "Apps/Scigraph/scigraph/legend")
     (:file "Apps/Scigraph/scigraph/graph-classes")
     (:file "Apps/Scigraph/scigraph/present")
     (:file "Apps/Scigraph/scigraph/annotations")
     (:file "Apps/Scigraph/scigraph/annotated-graph")
     (:file "Apps/Scigraph/scigraph/contour")
     (:file "Apps/Scigraph/scigraph/equation")
     (:file "Apps/Scigraph/scigraph/popup-accept")
     (:file "Apps/Scigraph/scigraph/popup-accept-methods")
     (:file "Apps/Scigraph/scigraph/duplicate-methods")
     (:file "Apps/Scigraph/scigraph/frame")
     (:file "Apps/Scigraph/scigraph/export")
     (:file "Apps/Scigraph/scigraph/demo-frame")))