;;;; Use tools from Graphviz as external programs to perform layout.
;;;;
;;;; This file is part of the mcclim-dot extension. See
;;;; Extensions/dot/README.md and Extensions/dot/LICENSE for more information.

(cl:in-package #:mcclim-dot)

(defun external-graphviz-dot-processor (graph-record dot-graph &key program)
  "Layout the graph by executing a program from the graphviz suite of graph
visualization tools."
  (declare (ignore graph-record))
  (setf program (or program dot:*dot-path*))
  (let* ((raw-out (uiop:run-program (list program "-Tjson0")
                                    :input (lambda (stream)
                                             (dot:print-graph dot-graph :stream stream))
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
             (node (make-instance 'dot::node :id name :attributes (list :pos pos))))
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
                   :edges edges)))
