;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Graph Formatting
;;;   Created: 2002-08-13
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------

;;;  (c) copyright 2002 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.
 
(in-package :CLIM-INTERNALS)

;;;; Notes

;; - Now what exactly are layout-graph-nodes and layout-graph-edges
;;   supposed to do? If LAYOUT-GRAPH-NODES is only responsible for
;;   laying out the node output records, why does it get the
;;   arc-drawer? If it should also draw the edges why then is there
;;   the other function? --GB 2002-08-13

;; - There is this hash table initarg to graph-output-records? Should
;;   FORMAT-GRAPH-FROM-ROOTS pass a suitable hash table for the given
;;   'duplicate-test', if so why it is passed down and why is it not
;;   restricted to the set of hash test functions? --GB 2002-08-13

;; - What is the purpose of (SETF GRAPH-NODE-CHILDREN) and
;;   (SETF GRAPH-NODE-PARENTS)?

;;;; Declarations

;; format-graph-from-roots

(define-protocol-class graph-output-record (output-record))

(defgeneric graph-root-nodes (graph-output-record))
(defgeneric (setf graph-root-nodes) (new-value graph-output-record))
(defgeneric generate-graph-nodes (graph-output-record stream root-objects
                                  object-printer inferior-producer
                                  &key duplicate-key duplicate-test))
(defgeneric layout-graph-nodes (graph-output-record stream arc-drawer arc-drawing-options))
(defgeneric layout-graph-edges (graph-output-record stream arc-drawer arc-drawing-options))
;;; NOTE: Which calls which? --GB 2002-08-13

(define-protocol-class graph-node-output-record (output-record))

(defgeneric graph-node-parents (graph-node-record))
(defgeneric (setf graph-node-parents) (new-value graph-node-record))
(defgeneric graph-node-children (graph-node-record))
(defgeneric (setf graph-node-children) (new-value graph-node-record))
(defgeneric graph-node-object (graph-node-record))

;;;; Machinery for graph types

(defconstant +built-in-graph-types+
  '(:tree :directed-graph :digraph :directed-acyclic-graph :dag)
  "List of graph types builtin by CLIM.")

(defvar *graph-types-hash*
  (make-hash-table :test #'eq)
  "A hash table which maps from symbols that name graph-types to class names; Filled by CLIM:DEFINE-GRAPH-TYPE")

(defun register-graph-type (graph-type class)
  "Registers a new graph-type."
  (setf (gethash graph-type *graph-types-hash*) class))

(defun find-graph-type (graph-type)
  "Find the a graph type; when it does not exist barks at the user."
  (or (gethash graph-type *graph-types-hash*)
      (progn
        (cerror "Specify another graph type to use instead."
                "There is no such graph type defined: ~S.~%The defined ones are: ~{~S~^, ~@_~}."
                graph-type
                (loop for key being each hash-key of *graph-types-hash*
                      collect key))
        ;; accept anyone?
        (princ "Graph Type? ")
        (find-graph-type (read)))))

(defmacro define-graph-type (graph-type class)
  (check-type graph-type symbol)
  (check-type class symbol)
  (unless (eq *package* (find-package :climi))
    (when (member graph-type +built-in-graph-types+)
      (cerror "Do it anyway" "You are about to redefine the builtin graph type ~S."
              graph-type)))
  ;; Note: I would really like this to obey to package locks and stuff.
  `(progn
    (register-graph-type ',graph-type ',class)
    ',graph-type))

(define-graph-type :tree tree-graph-output-record)
(define-graph-type :directed-acyclic-graph dag-graph-output-record)
(define-graph-type :dag dag-graph-output-record)
(define-graph-type :directed-graph digraph-graph-output-record)
(define-graph-type :digraph digraph-graph-output-record)

;;;; Entry

(defun format-graph-from-roots (root-objects object-printer inferior-producer
                                &key stream orientation cutoff-depth
                                     merge-duplicates duplicate-key duplicate-test
                                     generation-separation
                                     within-generation-separation
                                     center-nodes arc-drawer arc-drawing-options
                                     graph-type (move-cursor t))
  ;; Mungle some arguments
  (check-type cutoff-depth (or null integer))
  (setf stream (or stream *standard-output*)
        graph-type (or graph-type (if merge-duplicates :digraph :tree))
        duplicate-key (or duplicate-key #'identity)
        duplicate-test (or duplicate-test #'eql)
        orientation (or orientation :horizontal)
        generation-separation (or generation-separation 30)
        within-generation-separation (or within-generation-separation 10))
  ;;
  (let ((graph-output-record
         (with-new-output-record (stream (find-graph-type graph-type)
                                         graph-output-record
                                         :orientation orientation
                                         :center-nodes center-nodes
                                         :cutoff-depth cutoff-depth
                                         :merge-duplicates merge-duplicates
                                         :generation-separation generation-separation
                                         :within-generation-separation within-generation-separation
                                         :hash-table (make-hash-table :test duplicate-test))
           (with-output-recording-options (stream :draw nil :record t)
             (generate-graph-nodes graph-output-record stream root-objects
                                   object-printer inferior-producer
                                   :duplicate-key duplicate-key
                                   :duplicate-test duplicate-test)
             (layout-graph-nodes graph-output-record stream arc-drawer arc-drawing-options)
             (layout-graph-edges graph-output-record stream arc-drawer arc-drawing-options)))))
    (setf (output-record-position graph-output-record)
          (stream-cursor-position stream))
    (with-output-recording-options (stream :draw t :record nil)
      (replay-output-record graph-output-record stream))
    (when move-cursor
      (setf (stream-cursor-position stream)
            (values (bounding-rectangle-max-x graph-output-record)
                    (bounding-rectangle-max-y graph-output-record))))
    graph-output-record))

;;;; Graph Output Records

(defclass standard-graph-output-record (graph-output-record
                                        standard-sequence-output-record)
  ((orientation
    :initarg :orientation
    :initform :horizontal)
   (center-nodes
    :initarg :center-nodes
    :initform nil)
   (cutoff-depth
    :initarg :cutoff-depth
    :initform nil)
   (merge-duplicates
    :initarg :merge-duplicates
    :initform nil)
   (generation-separation
    :initarg :generation-separation
    :initform 10)
   (within-generation-separation
    :initarg :within-generation-separation
    :initform 10)
   (hash-table
    :initarg :hash-table
    :initform nil)
   (root-nodes
    :initform nil
    :accessor graph-root-nodes) ))

(defclass tree-graph-output-record (standard-graph-output-record))
(defclass dag-graph-output-record (standard-graph-output-record))
(defclass digraph-graph-output-record (standard-graph-output-record))

;;;; Nodes

(defclass standard-graph-node-output-record (graph-node-output-record
                                             standard-sequence-output-record)
  ((graph-parents
    :initarg :graph-parents
    :initform nil
    :accessor graph-node-parents)
   (graph-children
    :initarg :graph-children
    :initform nil
    :accessor graph-node-children)
   (object
    :initarg :object
    :reader graph-node-object)))

;;;;

(defmethod generate-graph-nodes ((graph-output-record standard-graph-output-record)
                                 stream root-objects
                                 object-printer inferior-producer
                                 &key duplicate-key duplicate-test)
  (declare (ignore duplicate-test))
  (with-slots (cutoff-depth merge-duplicates hash-table) graph-output-record
    (labels
        ((traverse-objects (node objects depth)
           (unless (and cutoff-depth (>= depth cutoff-depth))
             (remove nil
                     (map 'list
                          (lambda (child)
                            (let* ((key (funcall duplicate-key child))
                                   (child-node (and merge-duplicates
                                                    (gethash key hash-table))))
                              (cond (child-node
                                     (when node
                                       (push node (graph-node-parents child-node)))
                                     child-node)
                                    (t
                                     (let ((child-node
                                            (with-output-to-output-record
                                                (stream 'standard-graph-node-output-record new-node
                                                        :object child)
                                              (funcall object-printer child stream))))
                                       (when merge-duplicates
                                         (setf (gethash key hash-table) child-node))
                                       (when node
                                         (push node (graph-node-parents child-node)))
                                       (setf (graph-node-children child-node)
                                             (traverse-objects child-node
                                                              (funcall inferior-producer child)
                                                              (+ depth 1)))
                                       child-node)))))
                          objects)))))
      ;;
      (setf (graph-root-nodes graph-output-record)
            (traverse-objects nil root-objects 0))
      (values))))

(defmethod layout-graph-nodes ((graph-output-record tree-graph-output-record)
                               stream arc-drawer arc-drawing-options)
  (declare (ignore arc-drawer arc-drawing-options
                   stream))
  ;; flaw: compute the actual generation-separation
  ;; within-generation-separation from ddp measures.
  (with-slots (orientation center-nodes generation-separation within-generation-separation root-nodes) graph-output-record
    (check-type orientation (member :horizontal :vertical)) ;xxx move to init.-inst.
    ;; here major dimension is the dimension in which we grow the
    ;; tree.
    (labels ((node-major-dimension (node)
               (if (eq orientation :vertical)
                   (bounding-rectangle-height node)
                   (bounding-rectangle-width node)))
             (node-minor-dimension (node)
               (if (eq orientation :vertical)
                   (bounding-rectangle-width node)
                   (bounding-rectangle-height node))))
      ;; Gather the width of the individual generations
      (let ((ghash (make-hash-table :test #'eq)))
        (labels ((generation-sizes (nodes)
                   ;; Yup! We'd need the same tree traversal here ...
                   (setf nodes (remove-if (lambda(x)(gethash x ghash)) nodes))
                   (dolist (n nodes)(setf(gethash n ghash)t))
                   (progn
                     (cond ((null nodes)
                            nil)
                           (t
                            (cons (+ generation-separation
                                     (reduce #'max (mapcar #'node-major-dimension nodes)
                                             :initial-value 0))
                                  (generation-sizes (mapcan #'copy-list
                                                            (mapcar #'graph-node-children nodes)))))))))
        ;;;;;;;;;;

          (let ((hash (make-hash-table :test #'eq)))
            (labels ((foo (node majors u0 v0)
                       (cond ((gethash node hash)
                              v0)
                             (t
                              (setf (gethash node hash) t)
                              (cond ((null (graph-node-children node))
                                     (setf (output-record-position node)
                                           (if (eq orientation :vertical)
                                               (values v0 u0)
                                               (values u0 v0)))
                                     (add-output-record node graph-output-record) ;xxx right time?
                                     (+ v0 (node-minor-dimension node)))
                                    (t
                                     (let ((u (+ u0 (car majors)))
                                           (v v0))
                                       (maplist (lambda (rest)
                                                  (setf v (foo (car rest) (or (cdr majors)
                                                                              (list 100))
                                                               u v))
                                                  (unless (null (cdr rest))
                                                    (incf v within-generation-separation)))
                                                (graph-node-children node))

                                       (let ((v1 (+ v0 (/ (- (- v v0)
                                                             (node-minor-dimension node))
                                                          2))))
                                         (setf (output-record-position node)
                                               (if (eq orientation :vertical)
                                                   (values v1 u0)
                                                   (values u0 v1)))
                                         (add-output-record node graph-output-record)) ;xxx right time?
                                       ;; xxx what happens if this node is taller than the children?
                                       v)))))))
              ;;
              (let ((majors (generation-sizes (graph-root-nodes graph-output-record))))
                (let ((u (+ 0 (car majors)))
                      (v 0))
                  (maplist (lambda (rest)
                             (setf v (foo (car rest) majors u v))
                             (unless (null rest)
                               (incf v within-generation-separation)))
                           (graph-root-nodes graph-output-record)))))))))))

(defmethod layout-graph-edges ((graph-output-record tree-graph-output-record)
                               stream arc-drawer arc-drawing-options)
  (with-slots (root-nodes orientation) graph-output-record
    (let ((hash (make-hash-table)))
      (labels ((walk (node)
                 (unless (gethash node hash)
                   (setf (gethash node hash) t)
                   (dolist (k (graph-node-children node))
                     (with-bounding-rectangle* (x1 y1 x2 y2) node
                       (with-bounding-rectangle* (u1 v1 u2 v2) k
                         (case orientation
                           (:horizontal
                            (draw-line* stream (+ x2 2) (/ (+ y1 y2) 2)
                             (- u1 2) (/ (+ v1 v2) 2)
                             :line-thickness 0))
                           (:vertical
                            (draw-line* stream (/ (+ x1 x2) 2) y2
                             (/ (+ u1 u2) 2) v1)))))
                     ;;
                     (walk k)))))
        (map nil #'walk root-nodes)))))

#||
;; Experimental version for rectangular graphs
(defmethod layout-graph-edges ((graph-output-record tree-graph-output-record)
                               stream arc-drawer arc-drawing-options)
  (with-slots (root-nodes orientation) graph-output-record
    (let ((hash (make-hash-table)))
      (labels ((walk (node &aux (vlast nil) uu)
                 (unless (gethash node hash)
                   (setf (gethash node hash) t)
                   (with-bounding-rectangle* (x1 y1 x2 y2) node
                     (dolist (k (graph-node-children node))
                       (with-bounding-rectangle* (u1 v1 u2 v2) k
                         (case orientation
                           (:horizontal
                            (draw-line* stream (/ (+ x2 u1) 2) (/ (+ v1 v2) 2)
                             (- u1 2) (/ (+ v1 v2) 2))
                            (setf uu u1)
                            (setf vlast (max (or vlast 0) (/ (+ v1 v2) 2))))
                           (:vertical
                            (draw-line* stream (/ (+ x1 x2) 2) y2
                             (/ (+ u1 u2) 2) v1))))
                       (walk k))
                     (when vlast
                       (draw-line* stream (+ x2 2) (/ (+ y1 y2) 2) (/ (+ x2 uu) 2) (/ (+ y1 y2) 2))
                       (draw-line* stream (/ (+ x2 uu) 2) (/ (+ y1 y2) 2)
                                   (/ (+ x2 uu) 2) vlast))))))
        (map nil #'walk root-nodes)))))
||#

#||
(define-application-frame graph-test ()
  ()
  (:panes
   (interactor :interactor :width 800 :height 400 :max-width +fill+ :max-height +fill+))
  (:layouts
   (default
       interactor)))

(define-graph-test-command foo ()
  (with-text-style (*query-io* (make-text-style :sans-serif nil 12))
    (let ((*print-case* :downcase))
      (format-graph-from-roots
       (list `(define-graph-test-command test ()
               (let ((stream *query-io*)
                     (orientation :horizontal))
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
                        :orientation orientation
                        :stream stream)))))))
       #'(lambda (x s) (with-output-as-presentation (s x 'command)
                         (let ((*print-level* 1))
                           (princ (if (consp x) (car x) x) s))))
       #'(lambda (x) (and (consp x) (cdr x)))
       :stream *query-io*
       :orientation :horizontal))))

(defun external-symbol-p (sym)
  ;; *cough* *cough*
  (< (count #\: (let ((*package* (find-package :keyword)))
                  (prin1-to-string sym)))
     2))

(define-graph-test-command bar ()
  (with-text-style (*query-io* (make-text-style :sans-serif nil 12))
    (let ((*print-case* :downcase))
      (format-graph-from-roots
       (list (clim-mop:find-class 'climi::basic-output-record))
       #'(lambda (x s)
           (with-text-style (s (make-text-style nil
                                                (if (external-symbol-p (class-name x))
                                                    :bold
                                                    nil)
                                                nil))
             (prin1 (class-name x) s)))
       #'(lambda (x)
           (clim-mop:class-direct-subclasses x))
       :stream *query-io*
       :orientation :horizontal))))

(define-graph-test-command baz ()
  (with-text-style (*query-io* (make-text-style :sans-serif nil 12))
    (let ((*print-case* :downcase))
      (format-graph-from-roots
       (list (clim-mop:find-class 'climi::basic-output-record)
             (clim-mop:find-class 'climi::graph-output-record)
             )
       #'(lambda (x s)
           (with-text-style (s (make-text-style nil
                                                (if (external-symbol-p (class-name x))
                                                    :bold
                                                    nil)
                                                nil))
             (prin1 (class-name x) s)))
       #'(lambda (x)
           (clim-mop:class-direct-superclasses x))
       ;; :duplicate-key #'(lambda (x) 't)
       :merge-duplicates t
       :graph-type :tree
       :stream *query-io*
       :orientation :horizontal))))

(define-graph-test-command test ()
  (let ((stream *query-io*)
        (orientation :vertical))
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
           :orientation orientation
           :stream stream))))))
||#