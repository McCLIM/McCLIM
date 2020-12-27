;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2005 Aleksandar Bakic <a_bakic@yahoo.com>
;;;  (c) copyright 2006-2008 Troels Henriksen <athas@sigkill.dk>
;;;  (c) copyright 2019 Christoph Keßler <ck@plskthx.org>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The test cases in this files test the functions of the
;;; DREI-KILL-RING package implementing the kill ring functionality of
;;; Drei.

(in-package #:drei-tests)

(def-suite kill-ring-tests :description "The test suite for DREI-KILL-RING
related tests." :in drei-tests)

(in-suite kill-ring-tests)

(defconstant +kill-ring-max-size-lower-limit+ 5)

(defun random-kill-ring-max-size ()
  (max +kill-ring-max-size-lower-limit+ (random 20)))

(defun make-kill-ring (max-size)
  (let ((max-size (case max-size
                    (:min    +kill-ring-max-size-lower-limit+)
                    (:random (random-kill-ring-max-size))
                    (t       max-size))))
   (make-instance 'kill-ring :max-size max-size)))

(test kill-ring-sizing
  (let ((random-size (random-kill-ring-max-size)))
    (let ((instance (make-instance 'kill-ring :max-size random-size)))
      (is (= (kill-ring-max-size instance) random-size)))
    (let ((instance (make-instance 'kill-ring :max-size random-size)))
      (setf (kill-ring-max-size instance)
            (* random-size 2))
      (is  (= (kill-ring-max-size instance)
              (* random-size 2))))
    (let ((instance (make-instance 'kill-ring :max-size random-size)))
      (is (/= (kill-ring-max-size instance)
              (kill-ring-length instance))))))

(test kill-ring-standard-push
  (with-drei-environment ()
    (let ((instance (make-kill-ring :random)))
      (kill-ring-standard-push instance #(#\A))
      (kill-ring-standard-push instance #(#\B))
      (kill-ring-standard-push instance #(#\C))
      (is (= (kill-ring-length instance) 3)))
    (let ((instance (make-kill-ring :random)))
      (signals type-error
        (kill-ring-standard-push instance nil)))
    (let ((instance (make-kill-ring :min)))
      (kill-ring-standard-push instance #(#\A))
      (kill-ring-standard-push instance #(#\B))
      (kill-ring-standard-push instance #(#\C))
      (kill-ring-standard-push instance #(#\D))
      (kill-ring-standard-push instance #(#\E))
      (is (equal (coerce (kill-ring-yank instance) 'string)
                 "E"))
      (rotate-yank-position instance)
      (is (equal (coerce (kill-ring-yank instance) 'string)
                 "D"))

      (rotate-yank-position instance)
      (is (equal (coerce (kill-ring-yank instance) 'string)
                 "C")))))

(test kill-ring-concatenating-push
  (with-drei-environment ()
    (let ((instance (make-kill-ring :min)))
      (kill-ring-standard-push instance #(#\A))
      (kill-ring-concatenating-push instance #(#\B))
      (is (equal (coerce (kill-ring-yank instance) 'string)
                 "AB")))
    (let ((instance (make-kill-ring (+ +kill-ring-max-size-lower-limit+ 2))))
      (kill-ring-standard-push instance #(#\B))
      (kill-ring-standard-push instance #(#\Space))
      (kill-ring-standard-push instance #(#\A))
      (rotate-yank-position instance 2)
      (kill-ring-concatenating-push instance #(#\B #\C))
      (is (equal (coerce (kill-ring-yank instance) 'string)
                 "ABC")))))

(test kill-ring-reverse-concatenating-push
  (with-drei-environment ()
    (let ((instance (make-kill-ring :min)))
      (kill-ring-standard-push instance #(#\A))
      (kill-ring-reverse-concatenating-push instance #(#\B))
      (is (equal (coerce (kill-ring-yank instance) 'string)
                 "BA")))
    (let ((instance (make-kill-ring (+ +kill-ring-max-size-lower-limit+ 2))))
      (kill-ring-standard-push instance #(#\B))
      (kill-ring-standard-push instance #(#\Space))
      (kill-ring-standard-push instance #(#\A))
      (rotate-yank-position instance 2)
      (kill-ring-reverse-concatenating-push instance #(#\B #\C))
      (is (equal (coerce (kill-ring-yank instance) 'string)
                 "BCA")))))

(test kill-ring-yank
  (with-drei-environment ()
    (let ((instance (make-kill-ring :min)))
      (kill-ring-standard-push instance #(#\A))
      (is (equal (coerce (kill-ring-yank instance) 'string)
                 "A")))
    (let ((instance (make-kill-ring (+ +kill-ring-max-size-lower-limit+ 2))))
      (kill-ring-standard-push instance #(#\A))
      (is (equal (coerce (kill-ring-yank instance) 'string)
                 "A"))
      (is (equal (coerce (kill-ring-yank instance) 'string)
                 "A"))
      (is (eq (kill-ring-yank instance)
              (kill-ring-yank instance))))
    (let ((instance (make-kill-ring (+ +kill-ring-max-size-lower-limit+ 2))))
      (signals empty-kill-ring
        (kill-ring-yank instance)))))
