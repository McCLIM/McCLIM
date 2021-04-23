;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2021 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------

(cl:in-package #:clim-tests)

(def-suite* :mcclim.recording
  :in :mcclim)

(test recording.parent-triggers-add-output-record
  "Tests whether specifying `:parent' adds the child to the record."
  (let ((parent (make-instance 'standard-sequence-output-record)))
    (make-instance 'basic-output-record :parent parent)
    (is (= 1 (output-record-count parent)))))

;;; This is a regression test for a bug found in the the tree record
;;; implementation.
(test recording.recompute-extent-for-new/changed-child
  "Tests whether the bounding rectangle is propagated in a tree."
  (let ((history (make-instance 'standard-tree-output-history :stream nil))
        (record1 (make-instance 'standard-tree-output-record))
        (record2 (make-instance 'standard-tree-output-record))
        (child-x (make-instance 'basic-output-record
                                :x1 10 :y1 10 :x2 90 :y2 90)))
    (is (null-bounding-rectangle-p history))
    (is (null-bounding-rectangle-p record1))
    (is (null-bounding-rectangle-p record2))
    (is (not (null-bounding-rectangle-p child-x)))
    ;; adding record1 and record2 to history
    (add-output-record record1 history)
    (add-output-record record2 history)
    ;; nothing should change
    (is (null-bounding-rectangle-p history))
    (is (null-bounding-rectangle-p record1))
    (is (null-bounding-rectangle-p record2))
    ;; adding child-x to record-1 should change also the history
    (add-output-record child-x record1)
    (is (not (null-bounding-rectangle-p history)))
    (is (not (null-bounding-rectangle-p record1)))
    (is (null-bounding-rectangle-p record2))))

;;; Empty compound output record may not be covered by the bounding rectangle of
;;; the parent. Ensure that in that case we still count them and that they may
;;; be mapped over.
(test recording.map-over-output-records.outside-tree
  "Test whether map-over-output-records maps over each record."
  (let ((history (make-instance 'standard-tree-output-history :stream nil))
        (record1 (make-instance 'standard-tree-output-record))
        (child-x (make-instance 'basic-output-record
                                :x1 10 :y1 10 :x2 90 :y2 90)))
    (add-output-record record1 history)
    (add-output-record child-x history)
    (is (= 2 (output-record-count history)))
    (let ((counter 0))
      (map-over-output-records (lambda (record)
                                 (declare (ignore record))
                                 (incf counter))
                               history)
      (is (= 2 counter)))))
