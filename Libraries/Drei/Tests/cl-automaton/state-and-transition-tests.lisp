;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2005 Aleksandar Bakic <a_bakic@yahoo.com>
;;;  (c) copyright 2006-2008 Troels Henriksen <athas@sigkill.dk>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Tests for the states and transitions functionality of the
;;; cl-automaton system.

(cl:in-package #:drei-tests)

(def-suite state-and-transition-tests :description "The test
suite for CL-AUTOMATON state-and-transition related tests." :in automaton-tests)

(in-suite state-and-transition-tests)

(test clone.transition
  (let* ((t1 (make-instance 'automaton::transition
                            :minc (char-code #\a) :maxc (char-code #\b)
                            :to (make-instance 'automaton::state)))
         (t2 (automaton::clone t1)))
    (is (eqv t1 t2 +equalp-key-situation+))
    (is (eql (hash t1 +equalp-key-situation+)
             (hash t2 +equalp-key-situation+)))))

(test transition<.1
  (let ((t1 (make-instance 'automaton::transition
                           :minc (char-code #\a) :maxc (char-code #\b)
                           :to (make-instance 'automaton::state)))
        (t2 (make-instance 'automaton::transition
                           :minc (char-code #\c) :maxc (char-code #\d)
                           :to (make-instance 'automaton::state)))
        (automaton::*to-first* nil))
    (is-true (automaton::transition< t1 t2))))

(test transition<.2
  (let ((t1 (make-instance 'automaton::transition
                           :minc (char-code #\a) :maxc (char-code #\b)
                           :to (make-instance 'automaton::state)))
        (t2 (make-instance 'automaton::transition
                           :minc (char-code #\c) :maxc (char-code #\d)
                           :to (make-instance 'automaton::state)))
        (automaton::*to-first* t))
    (setf (automaton::num (automaton::to t1)) 1)
    (is-true (automaton::transition< t2 t1)))
  (let ((t1 (make-instance 'automaton::transition
                           :minc (char-code #\a) :maxc (char-code #\b)
                           :to (make-instance 'automaton::state)))
        (t2 (make-instance 'automaton::transition
                           :minc (char-code #\a) :maxc (char-code #\d)
                           :to (make-instance 'automaton::state)))
        (automaton::*to-first* t))
    (is-true (automaton::transition< t2 t1))))

(test transition<.3
  (let ((t1 (make-instance 'automaton::transition
                           :minc (char-code #\a) :maxc (char-code #\c)
                           :to (make-instance 'automaton::state)))
        (t2 (make-instance 'automaton::transition
                           :minc (char-code #\a) :maxc (char-code #\b)
                           :to (make-instance 'automaton::state)))
        (automaton::*to-first* nil))
    (is-true (automaton::transition< t1 t2))))

(test sstep.test-1
  (let* ((s (make-instance 'automaton::state))
         (tr (make-instance 'automaton::transition
                            :minc (char-code #\a) :maxc (char-code #\b) :to s)))
    (htadd (automaton::transitions s) tr)
    (is (eq (automaton::sstep s #\a) s))))

(test sstep.test-2
  (let* ((s (make-instance 'automaton::state))
         (tr (make-instance 'automaton::transition
                            :minc (char-code #\a) :maxc (char-code #\b) :to s)))
    (htadd (automaton::transitions s) tr)
    (is-false (automaton::sstep s #\c))))

(test add-epsilon
  (let* ((s1 (make-instance 'automaton::state))
         (s2 (make-instance 'automaton::state))
         (tr (make-instance 'automaton::transition
                            :minc (char-code #\a) :maxc (char-code #\b) :to s2)))
    (htadd (automaton::transitions s2) tr)
    (automaton::add-epsilon s1 s2)
    (is-true (htpresent (automaton::transitions s1) tr))))

(test sorted-transition-vector
  (let* ((t1 (make-instance 'automaton::transition
                            :minc (char-code #\a) :maxc (char-code #\c)
                            :to (make-instance 'automaton::state)))
         (t2 (make-instance 'automaton::transition
                            :minc (char-code #\a) :maxc (char-code #\b)
                            :to (make-instance 'automaton::state)))
         (s (make-instance 'automaton::state)))
    (htadd (automaton::transitions s) t1)
    (htadd (automaton::transitions s) t2)
    (is (equalp (automaton::sorted-transition-vector s nil)
                (vector t1 t2)))))

(test sorted-transition-list
  (let* ((t1 (make-instance 'automaton::transition
                            :minc (char-code #\a) :maxc (char-code #\c)
                            :to (make-instance 'automaton::state)))
         (t2 (make-instance 'automaton::transition
                            :minc (char-code #\a) :maxc (char-code #\b)
                            :to (make-instance 'automaton::state)))
         (s (make-instance 'automaton::state)))
    (htadd (automaton::transitions s) t1)
    (htadd (automaton::transitions s) t2)
    (is (equal (automaton::sorted-transition-list s nil)
               (list t1 t2)))))
