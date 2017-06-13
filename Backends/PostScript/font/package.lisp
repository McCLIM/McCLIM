;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

;;;  (c) copyright 2017 by
;;;           Cyrus Harmon (cyrus@bobobeach.com)

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


(defpackage #:clim-postscript-font
  (:use #:clim #:clim-extensions #:clim-lisp)
  (:export #:postscript-font-medium
           #:*iso-latin-1-symbolic-names*)
  (:import-from #:clim-internals
                #:get-environment-variable
                #:map-repeated-sequence
                #:atan*

                #:ellipse-normal-radii*

                #:get-transformation
                #:untransform-angle
                #:with-transformed-position

                #:maxf

                #:port-text-style-mappings))

