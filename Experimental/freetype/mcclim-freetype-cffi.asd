;;;; -*- Lisp -*-

;;;---------------------------------------------------------------------------
;;; This is currently a *NON-FUNCTIONING* system definition for
;;; FREETYPE with CFFI instead of CMUCL/SBCL style alien definitions.
;;; This is a work in progress and should not be used by anyone until
;;; its availability is announced. [2006/05/23:rpg]
;;;---------------------------------------------------------------------------


#|
To autoload mcclim-freetype after mcclim, link this file to a
directory in your asdf:*central-registry* and add the following to
your lisp's init file:

 (defmethod asdf:perform :after ((o asdf:load-op) (s (eql (asdf:find-system :clim-clx))))
   (asdf:oos 'asdf:load-op :mcclim-freetype))
|#

(defpackage :mcclim-freetype-system (:use :cl :asdf))
(in-package :mcclim-freetype-system)

(defclass uncompiled-cl-source-file (source-file) ())

(defmethod perform ((o compile-op) (f uncompiled-cl-source-file))
  t)
(defmethod perform ((o load-op) (f uncompiled-cl-source-file))
  (mapcar #'load (input-files o f)))
(defmethod output-files ((operation compile-op) (c uncompiled-cl-source-file))
  nil)
(defmethod input-files ((operation load-op) (c uncompiled-cl-source-file))
  (list (component-pathname c)))
(defmethod operation-done-p ((operation compile-op) (c uncompiled-cl-source-file))
  t)
(defmethod source-file-type ((c uncompiled-cl-source-file) (s module))
  "lisp")

(defsystem :mcclim-freetype
  :depends-on (:clim-clx :cffi)
  :serial t
  :components
  ((:file "freetype-package-cffi")
   (:uncompiled-cl-source-file "freetype-cffi")
   (:file "freetype-fonts-cffi")))


;;; Freetype autodetection

(defun parse-fontconfig-output (s)
  (when (stringp s)
    (setf s
	  (make-string-input-stream s)))
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
(defun find-bitstream-fonts ()
  (let ((fc-match  (sb-ext:find-executable-in-search-path "fc-match")))
    (if (null fc-match)
	nil
      (let* ((process (sb-ext:run-program fc-match `("-v" "Bitstream Vera")
					  :output :stream
					  :input nil))
	     (font-path (parse-fontconfig-output (sb-ext:process-output process))))
	font-path))))

#+allegro
(defun find-bitstream-fonts ()
  (let* ((fc-match (excl.osi:find-in-path "fc-match"))
	 (command (format nil "~A -v Bitstream Vera" fc-match)))
    (if (null fc-match)
	nil
	(multiple-value-bind (output error-output exit-code)
	    (excl.osi:command-output
	     command
	     :whole t)
	  (if (not (= exit-code 0))
	      (progn
		(format t "~&Tried to autoset font path, but was unable to find Bitstream Vera fonts.~%~T~A error output was ~%~T~T~A~%"
			command error-output)
		nil)
	      (let ((font-path (parse-fontconfig-output output)))
		(if (null font-path)
		    (progn
		      (format t "~&Tried to autoset font path, using command:~%~T~A~%~Tbut was unable to find Bitstream Vera fonts.~%"
			      command)
		      nil)
		    font-path)))))))

;;;#-(or sbcl allegro)
;;;(defmethod perform :after ((o load-op) (s (eql (asdf:find-system :mcclim-freetype))))
;;;  (warn-about-unset-font-path))

(defvar cl-user::*mcclim-freetype-font-path* nil
  "Set this variable to tell mcclim-freetype where to find the bitstream 
Vera fonts (instead of having it look for them.")

(defmethod perform :after ((o load-op) (s (eql (asdf:find-system :mcclim-freetype))))
  (let (font-path)
    (cond (cl-user::*mcclim-freetype-font-path*
	   (setf  (symbol-value (intern "*FREETYPE-FONT-PATH*" :mcclim-freetype))
		  cl-user::*mcclim-freetype-font-path*))
	  ((setf font-path (find-bitstream-fonts))
	   (setf (symbol-value (intern "*FREETYPE-FONT-PATH*" :mcclim-freetype))
		 font-path))
	  (t (warn-about-unset-font-path)))))
