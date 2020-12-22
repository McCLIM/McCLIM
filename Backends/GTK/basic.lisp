(in-package :clim-gtk)

(defun round-coordinate (x)
  (floor (+ x 0.5)))

(define-condition gtk-thread-condition (error)
  ((original :initarg :original
             :initform (alexandria:required-argument :original))))

#+nil
(defmacro in-gtk-thread (() &body body)
  (alexandria:with-gensyms (lock condvar result block-sym condition eval-res type result0)
    `(let ((,lock (bordeaux-threads:make-lock))
           (,condvar (bordeaux-threads:make-condition-variable))
           (,result nil))
       (gtk:within-gtk-thread
         (let ((,eval-res (block ,block-sym
                            (handler-bind ((error (lambda (,condition)
                                                    (return-from ,block-sym (list :condition ,condition)))))
                              (let ((,result (multiple-value-list (progn ,@body))))
                                (cons :result ,result))))))
           (bordeaux-threads:with-lock-held (,lock)
             (setq ,result ,eval-res)
             (bordeaux-threads:condition-notify ,condvar))))
       (destructuring-bind (,type . ,result0)
           (bordeaux-threads:with-lock-held (,lock)
             (loop
               until ,result
               do (bordeaux-threads:condition-wait ,condvar ,lock))
             ,result)
         (ecase ,type
           (:result (apply #'values ,result0))
           (:condition (error 'gtk-thread-condition :original ,result0)))))))

(defvar *gtk-thread-p* nil)

(defun call-with-gtk-thread (fn)
  (if *gtk-thread-p*
      (funcall fn)
      ;; ELSE: Need to call the function in the GTK thread
      (let ((lock (bordeaux-threads:make-lock))
            (condvar (bordeaux-threads:make-condition-variable))
            (result nil))
        (gtk:within-gtk-thread
          (let ((*gtk-thread-p* t))
            (let ((inner-result (multiple-value-list (funcall fn))))
              (bordeaux-threads:with-lock-held (lock)
                (setq result (cons :result inner-result))
                (bordeaux-threads:condition-notify condvar)))))
        (bordeaux-threads:with-lock-held (lock)
          (loop
            until result
            do (bordeaux-threads:condition-wait condvar lock)))
        (apply #'values (cdr result)))))

(defmacro in-gtk-thread (() &body body)
  `(call-with-gtk-thread (lambda () ,@body)))
