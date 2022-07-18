;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2002 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) Copyright 2002-2004 by Tim Moore <moore@bricoworks.com>
;;;  (c) Copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;  (c) Copyright 2021 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; This file contains the underlying mechanism for caching output records
;;; based on their :unique-id. It is mixed to updating streams and output
;;; records.
;;;
(in-package #:clim-internals)

;;; The map from unique values to output records. Unfortunately the :ID-TEST
;;; is specified in the child updating output records, not in the record that
;;; holds the cache! So, the map lookup code jumps through some hoops to use a
;;; hash table if the child id tests allow that and if there enough records in
;;; the map to make that worthwhile.

(defclass updating-output-map-mixin ()
  ((id-map :accessor id-map :initform nil)
   (id-counter :accessor id-counter
               :documentation "The counter used to assign unique ids to
                updating output records without one.")
   (tester-function :accessor tester-function :initform 'none
                    :documentation "The function used to lookup
  updating output records in this map if unique; otherwise, :mismatch.")
   (element-count :accessor element-count :initform 0)))

;;; Complete guess...
(defparameter *updating-map-threshold* 10
  "The limit at which the id map in an updating output record switches to a
  hash table.")

;;; ((eq map-test-func :mismatch)
;;;   nil)
(defun function-matches-p (map func)
  (let ((map-test-func (tester-function map)))
    (cond ((eq map-test-func func)
           t)
          ((and (symbolp map-test-func) (symbolp func)) ; not eq
           nil)
          ((and (symbolp map-test-func) (fboundp map-test-func))
           (eq (symbol-function map-test-func) func))
          ((and (symbolp func) (fboundp func))
           (eq map-test-func (symbol-function func)))
          (t nil))))

(defun ensure-test (map test)
  (unless (function-matches-p map test)
    (explode-map-hash map)
    (setf (tester-function map) :mismatch)))

(defgeneric clear-map (map)
  (:method ((map updating-output-map-mixin))
    (setf (id-map map) nil)
    (setf (id-counter map) 0)
    (setf (element-count map) 0)))

;;; Perhaps these should be generic functions, but in the name of premature
;;; optimization they're not :)
(defun get-from-map (map value test)
  (when (eq (tester-function map) 'none)
    (return-from get-from-map nil))
  (ensure-test map test)
  (let ((map (id-map map)))
    (if (hash-table-p map)
        (gethash value map)
        (cdr (assoc value map :test test)))))

(defun maybe-convert-to-hash (map)
  (let ((test (tester-function map)))
    (when (and (not (eq test :mismatch))
               (> (element-count map) *updating-map-threshold*)
               (or (case test
                     ((eq eql equal equalp) t))
                   (eq test #'eq)
                   (eq test #'eql)
                   (eq test #'equal)
                   (eq test #'equalp)))
      (let ((new-map (make-hash-table :test test)))
        (loop
           for (key . value) in (id-map map)
           do (setf (gethash key new-map) value))
        (setf (id-map map) new-map)))))

(defun explode-map-hash (map)
  (let ((hash-map (id-map map)))
    (when (hash-table-p hash-map)
      (loop
         for key being each hash-key of hash-map using (hash-value record)
         collect (cons key record) into alist
         finally (setf (id-map map) alist)))))

(defun add-to-map (map record value test replace)
  (if (eq (tester-function map) 'none)
      (setf (tester-function map) test)
      (ensure-test map test))
  (let ((val-map (id-map map)))
    (if (hash-table-p val-map)
        (multiple-value-bind (existing-value in-table)
            (if replace
                (gethash value val-map)
                (values nil nil))
          (declare (ignore existing-value))
          (setf (gethash value val-map) record)
          (unless in-table
            (incf (element-count map))))
        (let ((val-cons (if replace
                            (assoc value val-map :test test)
                            nil)))
          (if val-cons
              (setf (cdr val-cons) record)
              (progn
                (setf (id-map map) (acons value record val-map))
                (incf (element-count map))
                (maybe-convert-to-hash map)))))))

(defun delete-from-map (map value test)
  (ensure-test map test)
  (let ((val-map (id-map map))
        (deleted nil))
    (if (hash-table-p val-map)
        (setf deleted (remhash value val-map))
        (setf (values (id-map map) deleted)
              (delete-1 value val-map :test test :key #'car)))
    (when deleted
      (decf (element-count map)))))

;;; Reset the ID counter so that updating output records without explicit IDs
;;; can be assigned one during a run of the code. I'm not sure about using
;;; reinitialize-instance for this...
(defmethod shared-initialize :after ((obj updating-output-map-mixin) slot-names
                                     &key)
  (declare (ignore slot-names))
  (setf (id-counter obj) 0))
