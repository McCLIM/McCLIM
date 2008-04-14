;;; -*- lisp -*-

(defpackage :clim-examples.system
  (:use :cl :asdf))

(in-package :clim-examples.system)

;;; CLIM-Examples depends on having at least one backend loaded.
(defsystem :clim-examples
    :depends-on (:mcclim)
    :components
    ((:module "Examples"
              :components
              ((:file "calculator")
               (:file "colorslider")
	       (:file "menutest") ; extra
               (:file "address-book")
               (:file "traffic-lights")
               (:file "clim-fig")
               (:file "postscript-test")
               (:file "puzzle")
               (:file "transformations-test")
	       (:file "demodemo" :depends-on ("tabdemo"))
               (:file "stream-test")
               (:file "presentation-test")
               (:file "dragndrop")
	       (:file "gadget-test")
               (:file "accepting-values")
               (:file "method-browser")
	       (:file "stopwatch")
	       (:file "dragndrop-translator")
               (:file "draggable-graph")
               (:file "text-size-test")
               (:file "drawing-benchmark")
               (:file "logic-cube")
               (:file "views")
               (:file "font-selector")
               (:file "tabdemo")
               (:file "bordered-output-examples")
               (:file "misc-tests")
               (:file "image-viewer")))
     (:module "Goatee"
	      :components
	      ((:file "goatee-test")))))
