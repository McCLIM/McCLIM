;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-
;; $fiHeader: palette.lisp,v 1.4 1993/07/27 01:45:51 colin Exp $

(in-package :clim-demo)

(define-application-frame gadget-demo ()
    ()
  (:panes
    (slider1 slider :min-value 0 :max-value 10 :orientation :horizontal)
    (slider2 slider :min-value 0 :max-value 10 :orientation :vertical :height 100)
    (push-button1 push-button :label "Press Me")
    (toggle-button1 toggle-button :label "Toggle Me")
    (radio-box radio-box :choices '("Lisp" "Haskell" "Smalltalk"))
    (check-box check-box :choices '("Top-down" "Bottom-up"))
    (text-field text-field :height 20 :value "This is a text field")
    (text-editor (scrolling () 
		   (make-pane 'text-editor 
			      :value "This is a scrolling
text field gadget"
			      :ncolumns 20 :nlines 4)))
    (list-pane list-pane
	       :value "Franz" :items '("Franz" "Lucid" "Symbolics")
	       :test 'string=)
    (option-pane option-pane
		 :items '("eenie" "meanie" "minie")
		 :value "minie"
		 :test 'string=
		 :label "moo"))
  (:layouts
    (:default
      (vertically ()
	slider1 
	(horizontally ()
	  slider2 
	  (vertically () push-button1 toggle-button1 radio-box check-box))
	text-field text-editor list-pane option-pane))))


(define-demo "Gadget demo" gadget-demo)
