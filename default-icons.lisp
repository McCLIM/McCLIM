(in-package #:climi)

(flet ((load-icon (name)
         (let* ((file-pathname #.(or *compile-file-pathname*
                                     *load-pathname*))
                (icon-pathname (merge-pathnames
                                (make-pathname :directory '(:relative "data/icons")
                                               :name name
                                               :type "png")
                                file-pathname)))
           (make-pattern-from-bitmap-file icon-pathname :format :png))))
  (unless *default-icon-large*
    (setf *default-icon-large* (load-icon "mcclim-logo-round")))
  (unless *default-icon-small*
    (setf *default-icon-small* (load-icon "mcclim-logo-round-small"))))
