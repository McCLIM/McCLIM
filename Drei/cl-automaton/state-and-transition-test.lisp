;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :automaton-user)

(deftest clone.transition.test-1
  (let* ((t1 (make-instance 'automaton::transition
			    :minc (char-code #\a) :maxc (char-code #\b)
			    :to (make-instance 'automaton::state)))
	 (t2 (automaton::clone t1)))
    (and (eqv t1 t2 +equalp-key-situation+)
	 (eql (hash t1 +equalp-key-situation+)
	      (hash t2 +equalp-key-situation+))))
  t)

(deftest transition<.test-1
  (let ((t1 (make-instance 'automaton::transition
			   :minc (char-code #\a) :maxc (char-code #\b)
			   :to (make-instance 'automaton::state)))
	(t2 (make-instance 'automaton::transition
			   :minc (char-code #\c) :maxc (char-code #\d)
			   :to (make-instance 'automaton::state)))
	(automaton::*to-first* nil))
    (automaton::transition< t1 t2))
  t)

(deftest transition<.test-2
  (let ((t1 (make-instance 'automaton::transition
			   :minc (char-code #\a) :maxc (char-code #\b)
			   :to (make-instance 'automaton::state)))
	(t2 (make-instance 'automaton::transition
			   :minc (char-code #\c) :maxc (char-code #\d)
			   :to (make-instance 'automaton::state)))
	(automaton::*to-first* t))
    (setf (automaton::num (automaton::to t1)) 1)
    (automaton::transition< t2 t1))
  t)

(deftest transition<.test-2a
  (let ((t1 (make-instance 'automaton::transition
			   :minc (char-code #\a) :maxc (char-code #\b)
			   :to (make-instance 'automaton::state)))
	(t2 (make-instance 'automaton::transition
			   :minc (char-code #\a) :maxc (char-code #\d)
			   :to (make-instance 'automaton::state)))
	(automaton::*to-first* t))
    (automaton::transition< t2 t1))
  t)

(deftest transition<.test-3
  (let ((t1 (make-instance 'automaton::transition
			   :minc (char-code #\a) :maxc (char-code #\c)
			   :to (make-instance 'automaton::state)))
	(t2 (make-instance 'automaton::transition
			   :minc (char-code #\a) :maxc (char-code #\b)
			   :to (make-instance 'automaton::state)))
	(automaton::*to-first* nil))
    (automaton::transition< t1 t2))
  t)

(deftest sstep.test-1
  (let* ((s (make-instance 'automaton::state))
	 (tr (make-instance 'automaton::transition
			    :minc (char-code #\a) :maxc (char-code #\b) :to s)))
    (htadd (automaton::transitions s) tr)
    (eq (automaton::sstep s #\a) s))
  t)

(deftest sstep.test-2
  (let* ((s (make-instance 'automaton::state))
	 (tr (make-instance 'automaton::transition
			    :minc (char-code #\a) :maxc (char-code #\b) :to s)))
    (htadd (automaton::transitions s) tr)
    (automaton::sstep s #\c))
  nil)

(deftest add-epsilon.test-1
  (let* ((s1 (make-instance 'automaton::state))
	 (s2 (make-instance 'automaton::state))
	 (tr (make-instance 'automaton::transition
			    :minc (char-code #\a) :maxc (char-code #\b) :to s2)))
    (htadd (automaton::transitions s2) tr)
    (automaton::add-epsilon s1 s2)
    (htpresent (automaton::transitions s1) tr))
  t)

(deftest sorted-transition-vector.test-1
  (let* ((t1 (make-instance 'automaton::transition
			    :minc (char-code #\a) :maxc (char-code #\c)
			    :to (make-instance 'automaton::state)))
	 (t2 (make-instance 'automaton::transition
			    :minc (char-code #\a) :maxc (char-code #\b)
			    :to (make-instance 'automaton::state)))
	 (s (make-instance 'automaton::state)))
    (htadd (automaton::transitions s) t1)
    (htadd (automaton::transitions s) t2)
    (equalp (automaton::sorted-transition-vector s nil)
	    (vector t1 t2)))
  t)

(deftest sorted-transition-list.test-1
  (let* ((t1 (make-instance 'automaton::transition
			    :minc (char-code #\a) :maxc (char-code #\c)
			    :to (make-instance 'automaton::state)))
	 (t2 (make-instance 'automaton::transition
			    :minc (char-code #\a) :maxc (char-code #\b)
			    :to (make-instance 'automaton::state)))
	 (s (make-instance 'automaton::state)))
    (htadd (automaton::transitions s) t1)
    (htadd (automaton::transitions s) t2)
    (equal (automaton::sorted-transition-list s nil)
	   (list t1 t2)))
  t)