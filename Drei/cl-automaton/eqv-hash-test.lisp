;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :eqv-hash-user)

(defclass foo ()
  ((slot1 :initform 0 :initarg :slot1 :type fixnum :accessor slot1)
   (slot2 :initform 0 :initarg :slot2 :type fixnum :accessor slot2)))
(defclass foo-intention (equalp-key-situation) ())
(defparameter +foo-intention+ (make-instance 'foo-intention))
(defmethod eqv ((foo1 foo) (foo2 foo) (s (eql +foo-intention+)))
  (eql (slot1 foo1) (slot1 foo2)))
(defmethod hash ((foo1 foo) (s (eql +foo-intention+)))
  (floor (slot1 foo1) 2))

(deftest htref.test-1 ; (eqv i1 i2), (= (hash i1) (hash i2))
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 1 :slot2 2))
	(i2 (make-instance 'foo :slot1 1 :slot2 3)))
    (setf (htref ght i1) i1)
    (setf (htref ght i2) i2)
    (and (= (cnt ght) 1)
	 (eq (htref ght i1) i2)
	 (eq (htref ght i2) i2)))
  t)

(deftest htref.test-2 ; (not (eqv i1 i2)), (= (hash i1) (hash i2))
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2))
	(i2 (make-instance 'foo :slot1 3)))
    (setf (htref ght i1) i1)
    (setf (htref ght i2) i2)
    (and (= (cnt ght) 2)
	 (eq (htref ght i1) i1)
	 (eq (htref ght i2) i2)))
  t)

(deftest htref.test-3 ; (not (eqv i1 i2)), (/= (hash i1) (hash i2))
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2))
	(i2 (make-instance 'foo :slot1 4)))
    (setf (htref ght i1) i1)
    (setf (htref ght i2) i2)
    (and (= (cnt ght) 2)
	 (eq (htref ght i1) i1)
	 (eq (htref ght i2) i2)))
  t)

(deftest htref.test-4
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 1 :slot2 2))
	(i2 (make-instance 'foo :slot1 1 :slot2 3)))
    (setf (htref ght i1) i1)
    (and (= (cnt ght) 1) (eq (htref ght i2) i1)))
  t)

(deftest htref.test-5
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 1 :slot2 2))
	(i2 (make-instance 'foo :slot1 1 :slot2 3)))
    (setf (htref ght i1) i1)
    (multiple-value-bind (v vp)
	(htref ght i2)
      (declare (ignore v))
      (values (cnt ght) vp)))
  1 t)

(deftest htref.test-6
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2))
	(i2 (make-instance 'foo :slot1 3)))
    (setf (htref ght i1) i1)
    (htref ght i2))
  nil nil)

(deftest htref.test-7
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2))
	(i2 (make-instance 'foo :slot1 3)))
    (and (eq (setf (htref ght i1) i2) i2) (= (cnt ght) 1)))
  t)

(deftest htadd.test-1
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2)))
    (htadd ght i1)
    (htref ght i1))
  t t)

(deftest htadd.test-2
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2)))
    (htref ght i1))
  nil nil)

(deftest htadd.test-3
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2))
	(i2 (make-instance 'foo :slot1 3)))
    (htadd ght i1)
    (htref ght i2))
  nil nil)

(deftest htpresent.test-1
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2)))
    (htadd ght i1)
    (and (= (cnt ght) 1) (htpresent ght i1)))
  t)

(deftest htpresent.test-2
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2)))
    (and (= (cnt ght) 0) (htpresent ght i1)))
  nil)

(deftest htremove.test-1
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2)))
    (values
     (htremove ght i1)
     (= (cnt ght) 0)
     (htref ght i1)))
  nil t nil)

(deftest htremove.test-2
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2)))
    (htadd ght i1)
    (values
     (htremove ght i1)
     (= (cnt ght) 0)
     (htref ght i1)))
  t t nil)

(deftest with-ht.test-1
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2))
	(i2 (make-instance 'foo :slot1 3))
	l)
    (htadd ght i1)
    (htadd ght i2)
    (with-ht (k v) ght
      (push (cons k v) l))
    (and (= (length l) 2)
	 (equal (assoc i1 l) (cons i1 t))
	 (equal (assoc i2 l) (cons i2 t))))
  t)

(deftest with-ht.test-2
  (let ((ght (make-generalized-hash-table +foo-intention+))
	l)
    (with-ht (k v) ght
      (push (cons k v) l))
    l)
  nil)

(deftest with-ht-collect.test-1
  (let ((ght (make-generalized-hash-table +foo-intention+))
	(i1 (make-instance 'foo :slot1 2))
	(i2 (make-instance 'foo :slot1 3))
	l)
    (htadd ght i1)
    (htadd ght i2)
    (let ((l (with-ht-collect (k v) ght (cons k v))))
      (and (= (length l) 2)
	   (equal (assoc i1 l) (cons i1 t))
	   (equal (assoc i2 l) (cons i2 t)))))
  t)

(deftest with-ht-collect.test-2
  (let ((ght (make-generalized-hash-table +foo-intention+)))
    (with-ht-collect (k v) ght (cons k v)))
  nil)
