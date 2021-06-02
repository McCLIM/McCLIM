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
         (obj (jsown:parse raw-out "objects" "edges" "bb"))
         (bb (jsown:val obj "bb"))
         (gv-id-to-node-map (make-hash-table))
         (edges nil)
         (nodes nil))
    (dolist (raw-node (jsown:val obj "objects"))
      (let* ((name (jsown:val raw-node "name"))
             (gv-id (jsown:val raw-node "_gvid"))
             (pos (jsown:val raw-node "pos"))
             (node (make-instance 'dot::node :id name :attributes (list :pos pos))))
        (push node nodes)
        (setf (gethash gv-id gv-id-to-node-map) node)))

    (dolist (raw-edge (jsown:val obj "edges"))
      (let* ((tail-id (jsown:val raw-edge "tail"))
             (tail (gethash tail-id gv-id-to-node-map))
             (head-id (jsown:val raw-edge "head"))
             (head (gethash head-id gv-id-to-node-map))
             (pos (jsown:val raw-edge "pos")))
        (push (make-instance 'dot::edge
                             :source tail
                             :target head
                             :attributes (list :pos pos))
              edges)))
    (make-instance 'dot::graph
                   :attributes (list :bb bb)
                   :nodes nodes
                   :edges edges)))
