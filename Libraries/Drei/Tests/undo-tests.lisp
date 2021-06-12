;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2006 Troels Henriksen <athas@sigkill.dk>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Tests for the undo functionality.

(cl:in-package #:drei-tests)

(def-suite undo-tests :description "The test suite for tests
related to Drei's undo system." :in drei-tests)

(in-suite undo-tests)

(defclass test-undo-record (drei-undo:standard-undo-record)
  ())

(defmethod drei-undo:flip-undo-record ((record test-undo-record)))

(test add-undo
  (let ((tree (make-instance 'drei-undo:standard-undo-tree)))
    (finishes (drei-undo:add-undo (make-instance 'test-undo-record) tree))
    (finishes (drei-undo:add-undo (make-instance 'test-undo-record) tree))))

(test undo
  (let ((tree (make-instance 'drei-undo:standard-undo-tree)))
    (drei-undo:add-undo (make-instance 'test-undo-record) tree)
    (drei-undo:add-undo (make-instance 'test-undo-record) tree)
    (finishes (drei-undo:undo tree 2))
    (signals drei-undo:no-more-undo
      (drei-undo:undo tree 1))))

(test redo
  (let ((tree (make-instance 'drei-undo:standard-undo-tree)))
    (drei-undo:add-undo (make-instance 'test-undo-record) tree)
    (drei-undo:undo tree 1)
    (drei-undo:redo tree 1)
    (finishes (drei-undo:undo tree 1))))
