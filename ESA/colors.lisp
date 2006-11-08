;;; -*- Mode: Lisp; Package: clim-extensions -*-

;;;  (c) copyright 2006 by
;;;           Tim Moore (moore@bricoworks.com)

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

;;; Color definitions from McCLIM that don't exist in Classic CLIM

(in-package :clim-extensions)

#-(or mcclim building-mcclim)
(progn
  (defparameter +blue-violet+ (make-rgb-color 0.5412 0.1686 0.8863))
  (defparameter +gray50+ (make-gray-color 0.4980))
  (defparameter +gray85+ (make-gray-color 0.8510))
  (defparameter +dark-blue+ (make-rgb-color 0.0 0.0 0.5451))
  (defparameter +dark-green+ (make-rgb-color 0.0000 0.3922 0.0000))
  (defparameter +dark-violet+ (make-rgb-color 0.5804 0.0000 0.8275))
  (defparameter +maroon+ (make-rgb-color 0.6902 0.1882 0.3765))
  (defparameter +purple+  (make-rgb-color 0.6275 0.1255 0.9412)))
