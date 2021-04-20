;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2002 by Alexey Dejneka <adejneka@comail.ru>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-postscript-font)

(defvar *lisp-metrics-file*
  (merge-pathnames (make-pathname :name "standard-metrics"
                                  :type "lisp"
                                  :defaults *load-truename*)
                   *load-truename*))

(with-open-file (out *lisp-metrics-file*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (print '(in-package :clim-postscript) out)
  (let ((*default-pathname-defaults*
         (make-pathname :directory '(:absolute "usr" "share" "texmf" "fonts" "afm" "adobe")
                        :type "afm")))
    (loop for file in
         '("times/ptmr8a" "times/ptmb8a" "times/ptmri8a" "times/ptmbi8a"
           "courier/pcrr8a" "courier/pcrro8a" "courier/pcrb8a" "courier/pcrbo8a"
           "helvetic/phvr8a" "helvetic/phvro8a" "helvetic/phvb8a" "helvetic/phvbo8a")
         do (print `(define-font-metrics
                        ,@(mapcar (lambda (arg)
                                    (list 'quote arg))
                                  (multiple-value-list
                                   (with-open-file (stream (merge-pathnames file))
                                     (read-afm-stream stream)))))
                   out))))
