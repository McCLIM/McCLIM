;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)

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

(in-package :COMMON-LISP-USER)

(defpackage "CLIM-POSTSCRIPT"
  (:use "CLIM" "CLIM-EXTENSIONS" "CLIM-LISP")
  (:export "LOAD-AFM-FILE")
  (:import-from "CLIM-INTERNALS"
                "GET-ENVIRONMENT-VARIABLE"
                "MAP-REPEATED-SEQUENCE"
                "ATAN*"

                "ELLIPSE-NORMAL-RADII*"

                "GET-TRANSFORMATION"
                "UNTRANSFORM-ANGLE"
                "WITH-TRANSFORMED-POSITION"

                "MAXF"

                "PORT-TEXT-STYLE-MAPPINGS"
  ))

