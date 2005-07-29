;;;; -*- Lisp -*-

#|
To autoload mcclim-freetype after mcclim, link this file to a
directory in your asdf:*central-registry* and add the following to
your lisp's init file:

 (defmethod asdf:perform :after ((o asdf:load-op) (s (eql (asdf:find-system :clim-clx))))
   (asdf:oos 'asdf:load-op :mcclim-freetype))
|#

(defpackage :mcclim-freetype-system (:use :cl :asdf))
(in-package :mcclim-freetype-system)

(defclass uncompiled-cl-source-file (cl-source-file) ())

(defmethod perform ((o compile-op) (f uncompiled-cl-source-file))
  t)

(defmethod output-files ((operation compile-op) (c uncompiled-cl-source-file))
  (list (component-pathname c)))

(defsystem :mcclim-freetype
  :depends-on (:clim-clx)
  :serial t
  :components
  ((:file "freetype-package")
   (:uncompiled-cl-source-file "freetype-ffi")
   (:file "freetype-fonts")))


;;; Freetype autodetection

(defun parse-fontconfig-output (s)
  (let* ((match-string (concatenate 'string (string #\Tab) "file:"))
         (matching-line
          (loop for l = (read-line s nil nil)
                while l
                if (= (mismatch l match-string) (length match-string))
                   do (return l)))
         (filename (when matching-line
                     (probe-file
                      (subseq matching-line
                              (1+ (position #\" matching-line :from-end nil :test #'char=))
                              (position #\" matching-line :from-end t   :test #'char=))))))
    (when filename
      (make-pathname :directory (pathname-directory filename)))))

(defun warn-about-unset-font-path ()
  (warn "~%~%NOTE:~%~
* Remember to set mcclim-freetype:*freetype-font-path* to the
  location of the Bitstream Vera family of fonts on disk. If you
  don't have them, get them from http://www.gnome.org/fonts/~%~%~%"))

#+sbcl
(defmethod perform :after ((o load-op) (s (eql (asdf:find-system :mcclim-freetype))))
  (let ((fc-match  (sb-ext:find-executable-in-search-path "fc-match")))
    (if (null fc-match)
        (warn-about-unset-font-path)
        (let* ((process (sb-ext:run-program fc-match `("-v" "Bitstream Vera")
                                            :output :stream
                                            :input nil))
               (font-path (parse-fontconfig-output (sb-ext:process-output process))))
          (if (null font-path)
              (warn-about-unset-font-path)
              (setf (symbol-value (intern "*FREETYPE-FONT-PATH*" :mcclim-freetype))
                    font-path))))))

#-sbcl
(defmethod perform :after ((o load-op) (s (eql (asdf:find-system :mcclim-freetype))))
  (warn-about-unset-font-path))