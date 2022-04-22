;;;; Use tools from Graphviz as external programs to perform layout.
;;;;
;;;; This file is part of the mcclim-dot extension. See
;;;; Extensions/dot/README.md and Extensions/dot/LICENSE for more information.

(cl:in-package #:mcclim-dot)

(defun external-graphviz-dot-processor (graph-record dot-graph
                                        &key program layout-engine noop (splines "true")
                                          directed)
  "Layout the graph by executing a program from the graphviz suite of graph
visualization tools."
  (declare (ignore graph-record))

  (let ((args (list "-Tjson0")))
    (unless (null splines)
      (push (format nil "-Gsplines=~a" splines) args))
    (unless (null noop)
      (push (format nil "-n~d" noop) args))
    (when (null layout-engine)
      (cond
        ((numberp noop)
         (setf layout-engine "neato"))
        (directed
         (setf layout-engine "dot"))
        (t
         (setf layout-engine "neato"))))
    (when (symbolp layout-engine)
      (setf layout-engine (string-downcase (string layout-engine))))
    (push (format nil "-K~A" layout-engine) args)

    (let* ((raw-out (uiop:run-program (list* (or program dot:*dot-path*)
                                             args)
                                      :input (lambda (stream)
                                               (dot:print-graph dot-graph :stream stream :directed directed))
                                      :output '(:string :stripped t)
                                      :error-output :interactive))
           (shasht:*read-default-array-format* :list)
           (obj (shasht:read-json raw-out))
           (bb (gethash "bb" obj))
           (gv-id-to-node-map (make-hash-table))
           (edges nil)
           (nodes nil))
      (dolist (raw-node (gethash "objects" obj))
        (let* ((name (gethash "name" raw-node))
               (gv-id (gethash "_gvid" raw-node))
               (pos (gethash "pos" raw-node))
               (node (make-instance 'dot:node :id name :attributes (list :pos pos))))
          (push node nodes)
          (setf (gethash gv-id gv-id-to-node-map) node)))

      (dolist (raw-edge (gethash "edges" obj))
        (let* ((tail-id (gethash "tail" raw-edge))
               (tail (gethash tail-id gv-id-to-node-map))
               (head-id (gethash "head" raw-edge))
               (head (gethash head-id gv-id-to-node-map))
               (pos (gethash "pos" raw-edge))
               (label (gethash "label" raw-edge))
               (lp (gethash "lp" raw-edge)))
          (push (make-instance 'dot::edge
                               :source tail
                               :target head
                               :attributes (list :pos pos :label label :lp lp))
                edges)))
      (make-instance 'dot::graph
                     :attributes (list :bb bb)
                     :nodes nodes
                     :edges edges))))
