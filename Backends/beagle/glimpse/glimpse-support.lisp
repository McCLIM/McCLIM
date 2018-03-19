
(in-package :glimpse)


;;; Doing some graphing; display ports, grafts and sheets. Turn nodes into
;;; structures so we can pass more information along with them (such as a
;;; presentation for the CLIM UI!)

(defstruct node
  name
  children
  object)
  
;;; Display a graph of the port / graph / sheet (frame?) hierarchy that McCLIM knows
;;; about when this is invoked.


(defmethod display-sheet-hierarchy ((frame glimpse) stream)
  (declare (special *tree-output-type*
		    *tree-output-indent*
		    *tree-indent-increment*))
  
  ;; Hrm. When this method is used on any sizable pane hierarchy, the graph goes outside the
  ;; edge of the window. Introduce (scrolling () ...) layout.
  
  ;; Structure is as follows:

  ;; SCREEN            (make-node :name "MAIN SCREEN" :children (list port port2 portn) :object screen)
  ;;   PORT            (make-node :name "PORT NAME" :children (list graft graft2) :object port)
  ;;     GRAFT         (make-node :name "GRAFT" :children (list sheet sheet) :object graft)
  ;;       SHEET       (make-node :name "SHEET" :children (list sheet sheet) :object sheet)
  ;;         SHEET     (make-node :name "SHEET" :object sheet)
  ;;         SHEET     (make-node :name "SHEET" :object sheet)
  ;;       SHEET       (make-node :name "SHEET" :object sheet)
  ;;     GRAFT         (make-node :name "GRAFT" :object graft)
  ;;   PORT            (make-node :name "PORT" :children (list graft) :object port)
  ;;     GRAFT         (make-node :name "GRAFT" :object graft)

  (let ((ports (get-list-of-ports)))
    (if (eq *tree-output-type* :text)
	(format-simple-text-tree ports
				 #'(lambda (node stream)
				     (with-output-as-presentation (stream
								   (node-object node)
								   (type-of (node-object node)))
				       (write-string (node-name node) stream)))
				 #'node-children
				 :stream stream
				 :indent *tree-output-indent*
				 :indent-increment *tree-indent-increment*)
      (progn
	(terpri stream)
	(format-graph-from-roots ports
				 #'(lambda (node stream)
				     ;; would like to put some spacing in here... work out how.
				     (with-output-as-presentation (stream
								   (node-object node)
								   (type-of (node-object node)))
 				       (write-string (node-name node) stream)))
				 #'node-children
				 :stream stream
				 :orientation :horizontal
;				 :orientation :vertical
				 :graph-type :tree)
	(terpri stream)
	;; Work around an issue (not sure if it's a feature or a bug) with
	;; format-graph-from-roots not changing stream size.
	(change-space-requirements stream
				   :width (bounding-rectangle-width stream)
				   :height (bounding-rectangle-height stream))))))

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
        &key stream (indent 2) (indent-increment 2))

  ;; Don't do anything if stream is nil, or if there aren't any roots.

  (unless (or (null stream) (null roots))
    (loop for node in roots
              do (terpri stream)
	      do (indenting-output (stream (list indent :character))
		     (funcall display-fn node stream))
	      do (format-simple-text-tree (funcall inferiors-fn node)
					  display-fn
					  inferiors-fn
					  :stream stream
					  :indent (+ indent
						     indent-increment)
					  :indent-increment indent-increment))))


(defun get-list-of-ports ()
  (let ((nodes ()))
    (map-over-ports #'(lambda (port) (push (make-node-for-port port) nodes)))
    (nreverse nodes)))


(defun make-node-for-port (in-port)
  (make-node :name (princ-to-string in-port)
	     :children (if (climi::port-grafts in-port)  ; <- ::FIXME:: do something portable.
			   (let ((child-nodes nil))
			     (map-over-grafts #'(lambda (graft)
						  (push (make-node-for-sheet-or-graft graft) child-nodes))
					      in-port)
			     (nreverse child-nodes))
			 nil)
	     :object in-port))


(defun make-node-for-sheet-or-graft (in-sheet)
  (make-node :name (princ-to-string in-sheet)
	     :children (if (sheet-children in-sheet)
			   (let ((child-nodes nil))
			     (dolist (sheet (sheet-children in-sheet))
			       (let ((node (make-node-for-sheet-or-graft sheet)))
				 (push node child-nodes)))
			     (nreverse child-nodes))
			 nil)
	     :object in-sheet))

;;; Show processes that Lisp knows about...
(defmethod display-processes ((frame glimpse) stream)
  (let ((processes (get-list-of-processes)))
    (format-simple-text-tree processes
			     #'(lambda (node stream)
				 (with-output-as-presentation (stream
							       (node-object node)
							       (type-of (node-object node)))
;;				   (format *debug-io* "Added process with type: ~a~%" (type-of (node-object node)))
				   (write-string (node-name node) stream)))
			     #'node-children
			     :stream stream
			     :indent 0
			     :indent-increment 0)))

(defun get-list-of-processes ()
  (let ((nodes ()))
    (dolist (proc (clim-sys:all-processes))
      (push (make-node :name (princ-to-string proc)
		       :children nil
		       :object proc) nodes))
    (nreverse nodes)))

(defun describe-sheet (sheet output)
  ;; Display:
  ;; sheet-region, sheet-transformation
  ;; sheet-device-region, sheet-device-transformation
  ;; sheet-native-region, sheet-native-transformation
  ;; sheet-mirror-region, sheet-mirror-transformation (neither in spec :-( )
  (format output "Object ~a has the following geometry:~%" sheet)
  
  ;; Look up FORMAT in Hyperspec and do this formatting more nicely... or use
  ;; CLIM table output facilities.

  ;; Grafts have a region but no transformation...
  (if (typep sheet 'graft)
      (format output "               SHEET-REGION: ~s~%       SHEET-TRANSFORMATION: -None-~%"
	      (sheet-region sheet))
    (format output "               SHEET-REGION: ~s~%       SHEET-TRANSFORMATION: ~s~%"
	    (sheet-region sheet)
	    (sheet-transformation sheet)))
  (if (typep sheet 'sheet-with-medium-mixin)
      (format output "        SHEET-DEVICE-REGION: ~s~%SHEET-DEVICE-TRANSFORMATION: ~s~%"
	      (sheet-device-region sheet)
	      (sheet-native-transformation sheet))
    (progn
      (format output "SHEET has no MEDIUM")
      (if (sheet-mirror sheet)
	  (with-drawing-options (output :ink +red+)
	    (format output " (but it has a MIRROR, ho-hum)~%"))
	(terpri output))))
  (format output "        SHEET-NATIVE-REGION: ~s~%SHEET-NATIVE-TRANSFORMATION: ~s~%"
	  (sheet-native-region sheet)
	  (sheet-native-transformation sheet))
  (format output "It has the parent: ")
  (with-output-as-presentation (output
				(sheet-parent sheet)
				(type-of (sheet-parent sheet)))
    (format output "~s" (sheet-parent sheet)))
  (stream-terpri output))

(defun find-gadget (frame &key (name 'toggle-output))
  (get-frame-pane frame name))
