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

(defclass test-undo-record (standard-undo-record)
  ())

(defmethod flip-undo-record ((record test-undo-record)))

(test add-undo
  (let ((tree (make-instance 'standard-undo-tree)))
    (finishes (add-undo (make-instance 'test-undo-record) tree))
    (finishes (add-undo (make-instance 'test-undo-record) tree))))

(test undo
  (let ((tree (make-instance 'standard-undo-tree)))
    (add-undo (make-instance 'test-undo-record) tree)
    (add-undo (make-instance 'test-undo-record) tree)
    (finishes (undo tree 2))
    (signals no-more-undo
      (undo tree 1))))

(test redo
  (let ((tree (make-instance 'standard-undo-tree)))
    (add-undo (make-instance 'test-undo-record) tree)
    (undo tree 1)
    (redo tree 1)
    (finishes (undo tree 1))))
