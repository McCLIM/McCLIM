(in-package :clim-listener)

;;; Miscellaneous utilities, UI tools, and non-portable bits

;;; (C) Copyright 2003 by Andy Hefner (hefner1@umbc.edu)

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


(defun filtermap (list func &optional (filter #'null))
  (delete-if filter (mapcar func list)))

;(defmacro multiple-value-prog2 (&body body)  `(progn ,(first body) (multiple-value-prog1 ,@(rest body))))

;; multiple-value-or, ugh. Normal OR drops values except from the last form.
(defmacro mv-or (&rest forms)
  (if (null forms)  nil
    (let ((tmp (gensym)))
      `(let ((,tmp (multiple-value-list ,(first forms))))
         (if (first ,tmp) (values-list ,tmp) (mv-or ,@(rest forms)))))))


;; DEBUGF is useful, I can sleep better knowing it's in the image.
(defmacro debugf (&rest stuff)
  `(progn (fresh-line *trace-output*)
     ,@(reduce #'append 
                  (mapcar #'(lambda (x)                              
                              (cond
                                ((stringp x) `((princ ,x *trace-output*)))
                                (T `((princ ',x *trace-output*)
                                     (princ "=" *trace-output*)
                                     (write ,x :stream *trace-output*)
                                     (princ #\space *trace-output*)))))

                          stuff))
     (terpri *trace-output*)))


; This feels like a kludge, but it works well enough.
; The problem is if you give it a pathname like #P"/etc", which
; clearly refers to a directory, but "as a file", if that makes
; any sense. This is not a problem in itself, but people are prone
; to typing things like that.

(defun directoryp (pathname)
  "Returns pathname when supplied with a directory, otherwise nil"
  (if (pathname-name pathname) nil pathname))

(defun getenv (var)
  (or 
   #+cmu (cdr (assoc var ext:*environment-list*))
   #+sbcl (sb-ext:posix-getenv var)
   nil))

;; A farce of a  "portable" run-program, which grows as I need options from
;; the CMUCL run-program.
;; This ought to change the current directory to *default-pathname-defaults*..

(defun run-program (program args &key (wait T) (output *standard-output*))
  ;; Under CMUCL 18d, the PTY option is very broken.
  #+CMU (ext:run-program program args ; :input *standard-input* 
                                       :output output :wait wait)
  ;; Wonder if they fixed it in SBCL..
  #+SBCL (sb-ext:run-program program args :input *standard-input*
                                         :output output :wait wait)
  #-(or CMU SBCL) (format T "~&Sorry, don't know how to run programs in your CL.~%"))


;; CLIM/UI utilities

(defmacro bold ((stream) &body body)
  `(with-text-face (,stream :bold)
     ,@body))

(defmacro italic ((stream) &body body)
  `(with-text-face (,stream :italic)
     ,@body))

(defmacro bordering ((stream shape) &body body)
  `(surrounding-output-with-border (,stream :shape ,shape)
     ,@body))

(defmacro underlining ((stream) &body body)
  `(bordering (,stream :underline) ,@body))

(defun invoke-as-heading (cont &optional ink)
  (with-drawing-options (T :ink (or ink +royal-blue+) :text-style (make-text-style :sans-serif :bold nil))
     (bordering (T :underline)                        
      (funcall cont))))