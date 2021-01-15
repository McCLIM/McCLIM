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

(defun call-with-gtk-thread (no-block fn)
  (if *gtk-thread-p*
      (funcall fn)
      ;; ELSE: Need to call the function in the GTK thread
      (labels ((call ()
                 (block gtk-update
                   (handler-bind ((error (lambda (condition)
                                           (log:error "Error in GTK thread: ~a" condition)
                                           (break)
                                           (return-from gtk-update (list :error condition)))))
                     (let ((*gtk-thread-p* t))
                       (let ((inner-result (multiple-value-list (funcall fn))))
                         (cons :result inner-result)))))))
        (let ((lock (bordeaux-threads:make-lock))
              (condvar (bordeaux-threads:make-condition-variable))
              (result nil))
          (cond (no-block
                 (gtk:within-gtk-thread
                   (call))
                 nil)
                ;; ELSE: Call the function and wait for result
                (t
                 (gtk:within-gtk-thread
                   (let ((new-result (call)))
                     (bordeaux-threads:with-lock-held (lock)
                       (setq result new-result)
                       (bordeaux-threads:condition-notify condvar))))
                 (bordeaux-threads:with-lock-held (lock)
                   (loop
                     until result
                     do (bordeaux-threads:condition-wait condvar lock)))
                 (ecase (car result)
                   (:result (apply #'values (cdr result)))
                   (:error (error "Error in GTK thread: ~a" (cadr result))))))))))

(defmacro in-gtk-thread ((&key no-wait) &body body)
  (alexandria:once-only (no-wait)
    `(call-with-gtk-thread ,no-wait (lambda () ,@body))))

(defmacro iterate-over-seq-pairs ((a-sym b-sym seq) first-pair &body body)
  (check-type a-sym symbol)
  (check-type b-sym symbol)
  (alexandria:once-only (seq)
    (alexandria:with-gensyms (a b array-index)
      `(etypecase ,seq
         (list (when ,seq
                 (let ((,a-sym (first ,seq))
                       (,b-sym (second ,seq)))
                   ,first-pair)
                 (loop
                   for (,a ,b) on (cddr ,seq) by #'cddr
                   do (let ((,a-sym ,a)
                            (,b-sym ,b))
                        ,@body))))
         (array (when (plusp (length ,seq))
                  (let ((,a-sym (aref ,seq 0))
                        (,b-sym (aref ,seq 1)))
                    ,first-pair)
                  (loop
                    for ,array-index from 2 below (length ,seq) by 2
                    do (let ((,a-sym (aref ,seq ,array-index))
                             (,b-sym (aref ,seq (1+ ,array-index))))
                         ,@body))))))))

(defmacro iterate-over-4-blocks ((a-sym b-sym c-sym d-sym seq) &body body)
  (check-type a-sym symbol)
  (check-type b-sym symbol)
  (check-type c-sym symbol)
  (check-type d-sym symbol)
  (alexandria:once-only (seq)
    (alexandria:with-gensyms (a b c d array-index sequence)
      `(etypecase ,seq
         (list (loop
                 for (,a ,b ,c ,d) on ,seq by (lambda (,sequence) (nthcdr 4 ,sequence))
                 do (let ((,a-sym ,a)
                          (,b-sym ,b)
                          (,c-sym ,c)
                          (,d-sym ,d))
                      ,@body)))
         (array (loop
                  for ,array-index from 0 below (length ,seq) by 4
                  do (let ((,a-sym (aref ,seq ,array-index))
                           (,b-sym (aref ,seq (+ ,array-index 1)))
                           (,c-sym (aref ,seq (+ ,array-index 2)))
                           (,d-sym (aref ,seq (+ ,array-index 3))))
                       ,@body)))))))
