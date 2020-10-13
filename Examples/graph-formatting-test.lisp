;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2002 by Gilbert Baumann
;;;  (c) copyright 2017 by John A. Carroll
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Tests for FORMAT-GRAPH-FROM-ROOT[S]
;;;

(defpackage #:clim-demo.graph-formatting-test
  (:use
   #:clim-lisp
   #:clim)
  (:export
   #:graph-formatting-test))
(in-package #:clim-demo.graph-formatting-test)

(define-application-frame graph-formatting-test ()
  ()
  (:pane
   (scrolling ()
     (make-pane :interactor)))
  (:menu-bar nil)
  (:default-initargs :width 1000 :height 800))

(define-graph-formatting-test-command (com-horizontal-tree :name t) ()
  (let ((stream *query-io*))
   (with-text-style (stream (make-text-style :sans-serif nil 12))
     (let ((*print-case* :downcase))
       (format-graph-from-roots
        (list `(define-graph-formatting-test-command test ()
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
                          (lambda (node s)
                            (write-string (node-name node) s))
                          #'node-children
                          :orientation orientation
                          :stream stream)))))))
        (lambda (x s) (with-output-as-presentation (s x 'command)
                        (let ((*print-level* 1))
                          (princ (if (consp x) (car x) x) s))))
        (lambda (x) (and (consp x) (cdr x)))
        :stream stream
        :orientation :horizontal)))))

(defun external-symbol-p (sym)
  (eq (nth-value 1 (find-symbol (symbol-name sym) (symbol-package sym))) :external))

(defun print-class (class stream)
  (let* ((name (class-name class))
         (face (if (external-symbol-p name)
                   :bold
                   nil)))
    (with-text-style (stream (make-text-style nil face nil))
      (let ((*print-case* :downcase))
        (prin1 name stream)))))

(define-graph-formatting-test-command (com-horizontal-tree-with-borders :name t) ()
  (let ((stream *query-io*))
    (with-text-style (stream (make-text-style :sans-serif nil 10))
      (format-graph-from-roots
       (list (find-class 'climi::basic-output-record))
       (lambda (class stream)
         (surrounding-output-with-border (stream :shape :oval)
           (print-class class stream)))
       #'closer-mop:class-direct-subclasses
       :generation-separation '(4 :line)
       :within-generation-separation '(2 :character)
       :stream stream
       :orientation :horizontal))))

(define-graph-formatting-test-command (com-vertical-graph :name t)
    ((class '(completion (standard-graph-output-record climi::basic-output-record climi::graph-output-record))
            :default 'standard-graph-output-record :insert-default t))
  (let ((stream *query-io*))
    (with-text-style (stream (make-text-style :sans-serif nil 10))
      (let ((*print-case* :downcase))
        (format-graph-from-roots
         (list (find-class class))
         #'print-class
         (lambda (class)
           (reverse (closer-mop:class-direct-superclasses class)))
         :merge-duplicates t
         :graph-type :tree
         :arc-drawer #'climi::arrow-arc-drawer
         :stream stream
         :orientation :vertical)))))

(define-graph-formatting-test-command (com-vertical-tree :name t) ()
  (let ((stream *query-io*))
    (with-text-style (stream (make-text-style :sans-serif nil 10))
      (format-graph-from-roots
       (list '(:foo
               (:bar)
               (:baaaaaaaaaaaaaaz
                (:a)
                (:b))
               (:q
                (:x) (:y))))
       (lambda (x s)
         (prin1 (first x) s))
       #'cdr
       :generation-separation '(4 :line)
       :within-generation-separation '(2 :character)
       :stream stream
       :orientation :vertical))))

(define-graph-formatting-test-command (com-arc-drawing-options :name t) ()
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
           (lambda (node s)
             (write-string (node-name node) s))
           #'node-children
           :arc-drawer #'climi::arrow-arc-drawer
           :arc-drawing-options (list :ink +red+ :line-thickness 1)
           :orientation orientation
           :stream stream))))))

(define-graph-formatting-test-command (com-cycles :name t) ()
  (let ((stream *query-io*)
        (orientation :vertical))
    (fresh-line stream)
    (format-graph-from-roots
     (list '(defun dcons (x) (cons x x))
           '#1=(1 (2 . 4) 3 . #1#))
     (lambda (node s)
       (if (consp node)
           (progn
             (draw-circle* s 5 5 5 :filled nil))
           (princ node s)))
     (lambda (x)
       (if (consp x) (list (car x) (cdr x))))
     :cutoff-depth nil
     :graph-type :digraph
     :merge-duplicates t
     :arc-drawing-options (list :ink +red+ :line-thickness 1)
     :orientation orientation
     :within-generation-separation 30
     :stream stream)))

(define-graph-formatting-test-command (com-more-cycles :name t) ()
  (let ((stream *query-io*))
    (fresh-line stream)
    (format-graph-from-roots
     '((4 (1 (1)
           (3 (2 (1))
            (5 (6 (7 (10 (9 (5) (11 (12 (13 (11) (15 (14 (13))))) (14)))
                         (11 (14)) (14)))
                (8 (9) (10))
                (10))
             (8))))
        (2) (12) (13)))
     (lambda (x s) (princ (car x) s))
     #'cdr
     :graph-type :digraph :merge-duplicates t
     :duplicate-key #'car
     :generation-separation 40
     :within-generation-separation 30
     :stream stream)))

(define-graph-formatting-test-command (com-maximize-generations :name t) ()
  (let ((stream *query-io*))
    (fresh-line stream)
    (format-graph-from-root
     (find-class 'clim:gadget)
     (lambda (object stream)
       (write-string (string (class-name object)) stream))
     #'closer-mop:class-direct-subclasses
     :graph-type :dag :merge-duplicates t
     :maximize-generations t
     :center-nodes t
     :stream stream)))

(defun graph-formatting-test (&key new-process)
  (flet ((do-it ()
           (run-frame-top-level (make-application-frame 'graph-formatting-test))))
    (if new-process
        (clim-sys:make-process #'do-it)
        (do-it))))
