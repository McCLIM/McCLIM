;;; (c) Copyright 2006 by Troels Henriksen (athas@sigkill.dk)
;;; 

(in-package :drei-tests)

(deftest kill-ring-sizing.test-1
    (let* ((random-size (random 20))
           (instance (make-instance 'kill-ring :max-size random-size)))
      (eql (kill-ring-max-size instance)
           random-size))
  t)

(deftest kill-ring-sizing.test-2
    (let* ((random-size (random 20))
           (instance (make-instance 'kill-ring :max-size random-size)))
      (setf (kill-ring-max-size instance)
            (* random-size 2))
      (eql (kill-ring-max-size instance)
           (* random-size 2)))
  t)

(deftest kill-ring-sizing.test-3
    (let* ((random-size (1+ (random 20)))
           (instance (make-instance 'kill-ring :max-size random-size)))
      (not (eql (kill-ring-max-size instance)
                (kill-ring-length instance))))
  t)

(deftest kill-ring-standard-push.test-1
    (let* ((random-size (max 3 (random 20)))
           (instance (make-instance 'kill-ring :max-size random-size)))
      (kill-ring-standard-push instance #(#\A))
      (kill-ring-standard-push instance #(#\B))
      (kill-ring-standard-push instance #(#\C))
      (kill-ring-length instance))
  3)

(deftest kill-ring-standard-push.test-2
    (let* ((random-size (1+ (random 20)))
           (instance (make-instance 'kill-ring :max-size random-size)))
      (handler-case (kill-ring-standard-push instance nil)
        (type-error ()
          t)))
  t)

(deftest kill-ring-standard-push.test-3
    (let* ((instance (make-instance 'kill-ring :max-size 3)))
      (kill-ring-standard-push instance #(#\A))
      (kill-ring-standard-push instance #(#\B))
      (kill-ring-standard-push instance #(#\C))
      (kill-ring-standard-push instance #(#\D))
      (kill-ring-standard-push instance #(#\E))
      (values
       (kill-ring-yank instance)
       (progn
         (rotate-yank-position instance)
         (kill-ring-yank instance))
       (progn
         (rotate-yank-position instance)
         (kill-ring-yank instance))))
  #(#\E)
  #(#\D)
  #(#\C))

(deftest kill-ring-concatenating-push.test-1
    (let* ((instance (make-instance 'kill-ring :max-size 3)))
      (kill-ring-standard-push instance #(#\A))
      (kill-ring-concatenating-push instance #(#\B))
      (kill-ring-yank instance))
  #(#\A #\B))

(deftest kill-ring-concatenating-push.test-2
    (let* ((instance (make-instance 'kill-ring :max-size 5)))
      (kill-ring-standard-push instance #(#\B))
      (kill-ring-standard-push instance #(#\Space))
      (kill-ring-standard-push instance #(#\A))
      (rotate-yank-position instance 2)
      (kill-ring-concatenating-push instance #(#\B #\C))
      (kill-ring-yank instance))
  #(#\A #\B #\C))

(deftest kill-ring-reverse-concatenating-push.test-1
    (let* ((instance (make-instance 'kill-ring :max-size 3)))
      (kill-ring-standard-push instance #(#\A))
      (kill-ring-reverse-concatenating-push instance #(#\B))
      (kill-ring-yank instance))
  #(#\B #\A))

(deftest kill-ring-reverse-concatenating-push.test-2
    (let* ((instance (make-instance 'kill-ring :max-size 5)))
      (kill-ring-standard-push instance #(#\B))
      (kill-ring-standard-push instance #(#\Space))
      (kill-ring-standard-push instance #(#\A))
      (rotate-yank-position instance 2)
      (kill-ring-reverse-concatenating-push instance #(#\B #\C))
      (kill-ring-yank instance))
  #(#\B #\C #\A))

(deftest kill-ring-yank.test-1
    (let* ((instance (make-instance 'kill-ring :max-size 5)))
      (kill-ring-standard-push instance #(#\A))
      (kill-ring-yank instance))
  #(#\A))

(deftest kill-ring-yank.test-2
    (let* ((instance (make-instance 'kill-ring :max-size 5)))
      (kill-ring-standard-push instance #(#\A))
      (values (kill-ring-yank instance)
              (kill-ring-yank instance)))
  #(#\A)
  #(#\A))

(deftest kill-ring-yank.test-3
    (let* ((instance (make-instance 'kill-ring :max-size 5)))
      (handler-case (kill-ring-yank instance)
        (empty-kill-ring ()
          t)))
  t)