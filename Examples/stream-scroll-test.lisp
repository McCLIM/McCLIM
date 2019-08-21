;;; License: LGPL (See file COPYING for details).
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


(in-package :clim-demo)

(defvar *xxx* nil)

(defun stream-scroll-test (&optional (n 200))
  (when *xxx* (close *xxx*))
  (setf *xxx* (clim:open-window-stream :scroll-bars :both))

  (sleep 1) ; @FIXME: a seperate issue - there's no sync mechanism for when frame is ready
  (dotimes (i n)
    (write-string (format nil "~A: Line with some text.~%" (1+ i)) *xxx*))
  (format t "~a/~a output records written."
            (length (output-record-children (stream-output-history *xxx*)))
            n)
  *xxx*)
