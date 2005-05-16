
;;; Simple test; create a canvas and draw lines on it. Apply different
;;; joint and cap shapes to the lines, draw in different thicknesses
;;; and test line dashes too.

;;; See specification section 10.??, the line-style protocol.

(in-package :cl-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Package definition
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :drawing-tests
  (:use :clim :clim-lisp)
  (:nicknames :dt)
  (:export "DRAWING-TESTS"))

(in-package :drawing-tests)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define application frame (will become the CLOS class for the application)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-application-frame drawing-tests ()
  ()
  (:panes
   (app :application
	:display-time t
	:scroll-bars t
	:end-of-line-action :allow
	:height 500 :width 600)
   (int :interactor
	:height 75  :width 600))
  (:layouts
   (default (vertically () app int))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-drawing-tests-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-drawing-tests-command (com-test-show-sheet-hierarchy :name t) ()
  (let ((pane (find-pane-named *application-frame* 'app)))
    (display-sheet-hierarchy *application-frame* pane)))

(define-drawing-tests-command (com-test-simple-graph :name t) ()
  (let ((pane (find-pane-named *application-frame* 'app)))
    (display-simple-graph *application-frame* pane)))

(define-drawing-tests-command (com-test-line-styles :name t) ()
  (let ((pane (find-pane-named *application-frame* 'app)))
    (display-line-styles *application-frame* pane)))

(define-drawing-tests-command (com-clear-screen :name t) ()
  (let ((pane (find-pane-named *application-frame* 'app)))
    (window-clear pane)))

(define-drawing-tests-command (com-test-patterns :name t) ()
  (let ((pane (find-pane-named *application-frame* 'app)))
    (display-patterns *application-frame* pane)))

(define-drawing-tests-command (com-many-lines :name t) ()
  (let ((pane (find-pane-named *application-frame* 'app)))
    (time (draw-many-lines *application-frame* pane))))

(define-drawing-tests-command (com-many-strings :name t) ()
  (let ((pane (find-pane-named *application-frame* 'app)))
    (time (draw-many-strings *application-frame* pane))))

(define-drawing-tests-command (com-text-position :name t) ()
  (let ((pane (get-frame-pane *application-frame* 'app)))
    (draw-text-position *application-frame* pane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Supporting functions; actually 'do stuff'.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Run the application - use (run-drawing-tests :new-process t) to run
;;; as a stand-alone process.
(defun drawing-tests (&key (new-process nil)
			   (process-name "Tests"))
  (flet ((run ()
	      (run-frame-top-level (make-application-frame 'drawing-tests))))
    (if new-process
	(clim-sys:make-process #'run :name process-name)
      (run))))

;;; Draw some lines and shapes with different line styles applied.
(defmethod display-line-styles ((frame drawing-tests) stream)
  ;; Check different thicknesses and line ends
  (draw-line* stream 50 20 550 20 :line-thickness 1)
  (draw-line* stream 50 40 550 40 :line-thickness 5)
  (draw-line* stream 50 70 550 70 :line-thickness 10 :line-cap-shape :round)
  (draw-line* stream 50 100 550 100 :line-thickness 10 :line-cap-shape :square)
  (draw-line* stream 50 130 550 130 :line-thickness 10 :line-cap-shape :butt)
  ;; Check different joint types
  (draw-rectangle* stream 50 200 150 300
		   :filled nil
		   :line-thickness 10
		   :line-joint-shape :miter)
  (draw-rectangle* stream 350 200 450 300
		   :filled nil
		   :line-thickness 10
		   :line-joint-shape :bevel)
  (draw-rectangle* stream 200 200 300 300
		   :filled nil
		   :line-thickness 10
		   :line-joint-shape :round)
  ;; Check different line dash styles
  (draw-line* stream 50 370 550 370 :line-dashes nil)     ;nil= [solid]
  (draw-line* stream 50 390 550 390 :line-dashes t)       ;t  = [clim picks]
  ;; pattern should look like: | ---  - |
  (let ((seq #(10.0 30.0 10.0 10.0)))
    (draw-line* stream 50 410 550 410 :line-dashes seq)   ;seq= [user-specified]
    (draw-line* stream 50 430 550 430 :line-dashes seq :line-thickness 5))
  ;; pattern should look like: | - - - -| (identical to :line-dashes t)
  (let ((seq #(5.0 5.0 5.0 5.0)))
    (draw-line* stream 50 450 550 450 :line-dashes seq)   ;seq= [user-specified]
    (draw-line* stream 50 470 550 470 :line-dashes seq :line-thickness 5))
  ;; All that remains is to test the different units...
  )

(defmethod display-patterns ((frame drawing-tests) stream)
  ;; Should use the extensions for reading patterns that are described in the spec.
  (let ((pattern1 (climi::xpm-parse-file
		   #p"/Users/duncan/sandbox/common-lisp.net/mcclim/Apps/Listener/icons/object.xpm"))
	(pattern2 (climi::xpm-parse-file
		   #p"/Users/duncan/sandbox/common-lisp.net/mcclim/Apps/Listener/icons/design.xpm"))
	(pattern3 (climi::xpm-parse-file
		   #p"/Users/duncan/sandbox/common-lisp.net/mcclim/Apps/Listener/icons/lambda.xpm")))
    ;; (setf (medium-ink stream) pattern)   <- need to test this way too...
  (draw-rectangle* stream 50 200 150 300
		   :ink (make-rectangular-tile pattern1
					       (pattern-width pattern1)
					       (pattern-height pattern1))
		   :filled t
		   :line-thickness 10
		   :line-joint-shape :miter)
  (draw-rectangle* stream 350 200 450 300
		   :ink (make-rectangular-tile pattern2
					       (pattern-width pattern2)
					       (pattern-height pattern2))
		   :filled t
		   :line-thickness 10
		   :line-joint-shape :bevel)
  (draw-rectangle* stream 200 200 300 300
		   :ink (make-rectangular-tile pattern3
					       (pattern-width pattern3)
					       (pattern-height pattern3))
		   :filled t
		   :line-thickness 10
		   :line-joint-shape :round)))


;;; Doing some graphing; display ports, grafts and sheets. At some point, clicking
;;; on the graph nodes will generate full information on the window identified,
;;; possibly with accepting-values to change stuff about the sheets. Needs moving
;;; into a Peek application ;-)

(defun make-node (&key name children)
  (if (null children)
      (list name)
    (list* name children)))

(defun node-name (node)
  (car node))

(defun node-children (node)
  (cdr node))

;;; 'simple' test out of specification

(defmethod display-simple-graph ((frame drawing-tests) stream)
  (fresh-line stream)
  (macrolet ((make-node (&key name children)
               `(list* ,name ,children)))
    (flet ((node-name (node)
             (car node))
           (node-children (node)
             (cdr node)))
      (let* ((2a (make-node :name "2A"))
             (2b (make-node :name "2B"))
             (2c (make-node :name "2C"))
             (1a (make-node :name "1A" :children (list 2a 2b)))
             (1b (make-node :name "1B" :children (list 2b 2c)))
             (root (make-node :name "0" :children (list 1a 1b))))
        (format-graph-from-roots
          (list root)
          #'(lambda (node s)
              (write-string (node-name node) s))
          #'node-children
          :orientation :horizontal
          :stream stream)))))

;;; Display a graph of the port / graph / sheet (frame?) hierarchy that McCLIM knows
;;; about when this is invoked.


(defmethod display-sheet-hierarchy ((frame drawing-tests) stream)

  ;; Hrm. When this method is used on any sizable pane hierarchy, the graph goes outside the
  ;; edge of the window, but we don't get to be able to scroll, even though a scroll-bar is
  ;; shown. Dunno what's up with that.
  
  ;; Doesn't appear to do anything...
  (fresh-line stream)

  ;; Structure is as follows:

  ;; SCREEN            (make-node :name "MAIN SCREEN" :children (list port port2 portn))
  ;;   PORT            (make-node :name "PORT NAME" :children (list graft graft2))
  ;;     GRAFT         (make-node :name "GRAFT" :children (list sheet sheet))
  ;;       SHEET       (make-node :name "SHEET" :children (list sheet sheet))
  ;;         SHEET     (make-node :name "SHEET")
  ;;         SHEET     (make-node :name "SHEET")
  ;;       SHEET       (make-node :name "SHEET")
  ;;     GRAFT         (make-node :name "GRAFT")
  ;;   PORT            (make-node :name "PORT" :children (list graft))
  ;;     GRAFT         (make-node :name "GRAFT")

  ;; There doesn't appear to be an option to get a 'text' tree output though, maybe we need
  ;; an extension? OTOH I don't suppose it *has* to look like peek output ;-)

  ;; Still todo: make output presentations, with a default command that actually does
  ;; something (display medium info, region, transformation info).
  ;; Make the output look more like peek's output.
  ;; Move into an initial peek incarnation.
  (let ((ports (get-list-of-ports)))
    (format-simple-text-tree ports
			     #'(lambda (node stream)
				 (write-string (node-name node) stream))
			     #'node-children
			     :stream stream)))

;;; Uncomment the following and comment out 'format-simple-text-tree' if you want
;;; a 'pretty graph'.
#||
    (format-graph-from-roots ports
			     #'(lambda (node stream)
                                 (write-string (node-name node) stream))
                             #'node-children
                             :stream stream
                             :orientation :vertical
                             :graph-type :tree)))
||#

;;;;
;;;; AIM: to produce output similar to the following:
;;;;
;;;; Given a tree (must be an acyclic tree) such as the following:
;;;;
;;;; screen - port -+-- graft-1
;;;;                 \
;;;;                  - graft-2 -+-- sheet-1
;;;;                             +-- sheet-2
;;;;                             +-- sheet-3
;;;;
;;;; Produce output:-
;;;;
;;;; SCREEN
;;;;     PORT
;;;;         GRAFT-1
;;;;         GRAFT-2
;;;;             SHEET-1
;;;;             SHEET-2
;;;;             SHEET-3
;;;; 
;;;; Need to look at 'format-graph-from-roots' and see if this type could
;;;; be added to that method - better (more CLIM-like) API.
;;;;

(defun format-simple-text-tree (roots display-fn inferiors-fn
        &key stream (indent 0) (indent-increment 4))

  ;; Don't do anything if stream is nil, or if there aren't any roots.

  ;; TODO: make use of cutoff depth, test for circular lists...
  ;;       should 'funcall's in following be 'apply's?

  (unless (or (null stream) (null roots))
    (loop for node in roots
              do (progn  ; using 'indent' instead of 'indnt' breaks it!
		   (terpri stream)
		   (indenting-output (stream (list indent :character))
		     (funcall display-fn node stream))
		   (format-simple-text-tree (funcall inferiors-fn node)
					    display-fn
					    inferiors-fn
					    :stream stream
					    :indent (+ indent
						       indent-increment))))))


;;; Must be able to do this in a nicer manner...
(defun get-list-of-ports ()
  (let ((nodes ()))
    (dolist (port climi::*all-ports*)
      (setf nodes (push (make-node-for-port port) nodes)))
    (nreverse nodes)))

;;; Can we make what we output be presentations? Probably, check Listener code.
;;; Create a macro to bring these together.
(defun make-node-for-port (in-port)
  (make-node :name (princ-to-string in-port)
	     :children (if (climi::port-grafts in-port)
			   (let ((child-nodes nil))
			     (dolist (graft (climi::port-grafts in-port))
			       (let ((node (make-node-for-graft graft)))
				 (setf child-nodes (push node child-nodes))))
			     (nreverse child-nodes))
			 nil)))


(defun make-node-for-graft (in-graft)
  (make-node :name (princ-to-string in-graft)
	     :children (if (sheet-children in-graft)
			   (let ((child-nodes nil))
			     (dolist (sheet (sheet-children in-graft))
			       (let ((node (make-node-for-sheet sheet)))
				 (setf child-nodes (push node child-nodes))))
			     (nreverse child-nodes))
			 nil)))


(defun make-node-for-sheet (in-sheet)
  (make-node :name (princ-to-string in-sheet)
	     :children (if (sheet-children in-sheet)
			   (let ((child-nodes nil))
			     (dolist (sheet (sheet-children in-sheet))
			       (let ((node (make-node-for-sheet sheet)))
				 (setf child-nodes (push node child-nodes))))
			     (nreverse child-nodes))
			 nil)))

(defmethod draw-many-lines ((frame drawing-tests) stream)
  (let ((xrange 400)
	(yrange 500))
    (loop for count from 1 to 2000   ; 100000
	  do (let ((x1 (* (random 1.0d0) xrange))
		   (x2 (* (random 1.0d0) xrange))
		   (y1 (* (random 1.0d0) yrange))
		   (y2 (* (random 1.0d0) yrange)))
;;	       (format *trace-output* "Drawing line (~a, ~a) to (~a, ~a)~%"
;;		       (+ 50 x1) (+ 50 y1) (+ 50 x2) (+ 50 y2))
	       (draw-line* stream (+ 50 x1) (+ 50 y1) (+ 50 x2) (+ 50 y2))))))

(defmethod draw-many-strings ((frame drawing-tests) stream)
  (let ((xrange 380)
	(yrange 500)
	(text "Hello from Lisp"))
    (loop for count from 1 to 2000 ; 100000
	  do (let ((x1 (* (random 1.0d0) xrange))
		   (y1 (* (random 1.0d0) yrange)))
;;	       (format *trace-output* "Drawing text at (~a, ~a)~%" (+ 50 x1) (+ 50 y1))
	       (draw-text* stream text (+ 50 x1) (+ 50 y1))))))

(defmethod draw-text-position ((frame drawing-tests) stream)
  (let ((medium (sheet-medium stream)))
    (with-text-size (medium :huge)
      (draw-text* stream "AaBbCcDdEeFfGgHhIiJjKkLlMm" 10 100)   ; increment x? +0.5 seems to help, a little.
      (draw-text* stream "NnOoPpQqRrSsTtUuVvWwXxYyZz" 10 300)
      (multiple-value-bind (width height final-x final-y baseline)
	  (text-size medium "AaBbCcDdEeFfGgHhIiJjKkLlMm")
	;; red: add baseline, width, etc. to x + y that text is drawn.
	(with-drawing-options (medium :ink +red+ :line-style (make-line-style :thickness 2))
	  (draw-rectangle* stream 10 100 (+ 10 width) (+ 100 height) :filled nil)
          (draw-line* stream 10 (+ 100 baseline) (+ 10 width) (+ 100 baseline)))
	;; blue: subtract baseline, width, etc. from x + y that text is drawn.
        (with-drawing-options (medium :ink +blue+ :line-style (make-line-style :thickness 2))
	  (draw-rectangle* stream 10 100 (+ 10 width) (- 100 height) :filled nil)
          (draw-line* stream 10 (- 100 baseline) (+ 10 width) (- 100 baseline)))
	;; green: put baseline on x, y that text is drawn and draw box accordingly.
        (with-drawing-options (medium :ink +green+ :line-style (make-line-style :thickness 2))
	  ;; bottom of rect is below the baseline; top is above.
	  ;; From our point of view, the form below is the bounding rectangle of the text.
	  ;; I have a feeling this *isn't* how McCLIM calculates the bounding area though.
	  (draw-rectangle* stream 10           (+ 100 (- height baseline))
				  (+ 10 width) (- 100 baseline) :filled nil)
	  ;; Text is drawn *on the baseline*.
          (draw-line* stream 10 100 (+ 10 width) 100)))))
  (draw-text* stream "#!@$%^&*()_+{}=\\~?/:;[]<>" 10 200)
  (draw-line* stream 10 100 570 100)
  (draw-line* stream 10 10 10 400)
  (draw-line* stream 10 300 570 300)
  (draw-line* stream 10 200 570 200))
