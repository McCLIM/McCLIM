;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM-2, Chapter 32.2 Multi-processing
;;;            for ACL
;;;   Created: 2001-05-22
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2001 by Gilbert Baumann
;;;  (c) copyright 2002 by John Wiseman (jjwiseman@yahoo.com)

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

(in-package :CLIM-INTERNALS)

(defconstant *multiprocessing-p* t)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :clim-mp *features*))

(defun make-process (function &key name)
  (ccl:process-run-function name function))

(defun destroy-process (process)
  (ccl:process-kill process))

(defun current-process ()
  ccl:*current-process*)

(defun all-processes ()
  ccl:*all-processes*)

(defun processp (object)
  (typep object 'ccl::process))

(defun process-name (process)
  (ccl:process-name process))

(defun process-state (process)
  (declare (ignore process))
  ;; Hmm can we somehow gain useful information here?
  nil)

(defun process-whostate (process)
  (ccl:process-whostate process))

(defun process-wait (reason predicate)
  (ccl:process-wait reason predicate))

(defun process-wait-with-timeout (reason timeout predicate)
  (ccl:process-wait-with-timeout reason timeout predicate))

(defun process-yield ()
  (ccl:process-allow-schedule))

(defun process-interrupt (process function)
  (ccl:process-interrupt process function))

(defun disable-process (process)
  (ccl:process-enable-arrest-reason process 'suspend))

(defun enable-process (process)
  (ccl:process-disable-arrest-reason process 'suspend))

(defun restart-process (process)
  (ccl:process-reset process) )

(defmacro without-scheduling (&body body)
  `(ccl:without-interrupts ,@body))

;; We perhaps could make use of EXCL::ATOMICALLY, which is
;; undocumented, but seems to do what we want.
;; Use EXCL::ATOMICALLY in OpenMCL?? - mikemac

#-openmcl-native-threads
(defmacro atomic-incf (place)
  `(ccl:without-interrupts
    (incf (the fixnum ,place))))

#-openmcl-native-threads
(defmacro atomic-decf (place)
  `(ccl:without-interrupts
    (decf (the fixnum ,place))))

#+openmcl-native-threads
(defmacro atomic-incf (place)
   `(ccl::atomic-incf ,place))

#+openmcl-native-threads
(defmacro atomic-decf (place)
   `(ccl::atomic-decf ,place))
;;; 32.3 Locks

(defun make-lock (&optional name)
  (ccl:make-lock name))

(defmacro with-lock-held ((place &optional state) &body body)
  `(ccl:with-lock-grabbed (,place 'ccl:*current-process*
				  ,@(if state (list state) nil))
			  ,@body))

(defun make-recursive-lock (&optional name)
  (ccl:make-lock name))

(defmacro with-recursive-lock-held ((place &optional state) &body body)
  `(ccl:with-lock-grabbed (,place 'ccl:*current-process*
				  ,@(if state (list state) nil))
			  ,@body))
