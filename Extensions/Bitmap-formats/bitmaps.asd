;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (c) 2008, Troels Henriksen (athas@sigkill.dk)
;;; Copyright (c) 2009, Samium Gromoff (_deepfire@feelingofgreen.ru)
;;; Copyright (c) 2009, Cyrus Harmon (ch-lisp@bobobeach.com)
;;; Copyright (c) 2016, Daniel Kochmański (daniel@turtleware.eu)
;;;
;;; See file 'LICENSE' for the copyright details
;;;

(defsystem :mcclim-gif-bitmaps
  :description "Support for GIF images in McCLIM bitmap reading functions."
  :depends-on (:mcclim :skippy)
  :components ((:file "gif")))

(defsystem :mcclim-png-bitmaps
  :description "Support for PNG images in McCLIM bitmap reading functions."
  :depends-on (:mcclim :png-read)
  :components ((:file "png")))

(defsystem :mcclim-jpeg-bitmaps
  :description "Support for JPEG images in McCLIM bitmap reading functions."
  :depends-on (:mcclim :cl-jpeg)
  :components ((:file "jpeg")))

(defsystem :mcclim-tiff-bitmaps
  :description "Support for TIFF images in McCLIM bitmap reading functions."
  :depends-on (:mcclim :retrospectiff)
  :components ((:file "tiff")))
