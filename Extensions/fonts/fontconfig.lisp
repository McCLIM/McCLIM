;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MCCLIM-FREETYPE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Experimental FreeType support
;;;   Created: 2003-05-25 16:32
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2008 by Andy Hefner

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

;;; Who originally wrote this? I want to put them in the file header. -Hefner

(in-package :mcclim-truetype)

(defparameter *family-names*
  '((:serif      . "Serif")
    (:sans-serif . "Sans")
    (:fix        . "Mono")))

(defparameter *fontconfig-faces*
  '((:roman . "")
    (:bold  . "bold")
    (:italic . "oblique")
    ((:bold :italic) . "bold:oblique")))

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
      (parse-namestring filename))))

(defun warn-about-unset-font-path ()
  (cerror "Proceed"
          "~%~%NOTE:~%~
* McCLIM was unable to configure itself automatically using
  fontconfig. Therefore you must configure it manually.~%"))

(defun find-fontconfig-font (font-fc-name)
  (multiple-value-bind (output errors code)
      (uiop:run-program (list "fc-match" "-v" font-fc-name)
			:input nil :output :string :error-output nil
			:force-shell t :ignore-error-status t)
    (declare (ignore errors))
    (if (not (zerop code))
	(warn "~&fc-match failed with code ~D.~%" code)
	(with-input-from-string (stream output)
	  (parse-fontconfig-output stream)))))

(defun fontconfig-name (family face) 
  (format nil "~A:~A" family face))

(defun build-font/family-map (&optional (families *family-names*))
  (loop for family in families nconcing
    (loop for face in *fontconfig-faces* 
          as filename = (find-fontconfig-font (fontconfig-name (cdr family) (cdr face)))
          when (null filename) do (return-from build-font/family-map nil)
          collect
          (cons (list (car family) (car face)) filename))))

(defun autoconfigure-fonts ()
  (let ((map (build-font/family-map)))
    (if (and map (support-map-p map))
        (setf *families/faces* map)
        (warn-about-unset-font-path))))

(defun support-map-p (font-map)
  (handler-case
      (every #'(lambda (font)
		 (zpb-ttf:with-font-loader (ignored (cdr font)) t))
	     font-map)
    (zpb-ttf::bad-magic () nil)))
