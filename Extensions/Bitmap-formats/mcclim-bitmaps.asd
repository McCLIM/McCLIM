;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (c) 2008, Troels Henriksen (athas@sigkill.dk)
;;; Copyright (c) 2009, Samium Gromoff (_deepfire@feelingofgreen.ru)
;;; Copyright (c) 2009, Cyrus Harmon (ch-lisp@bobobeach.com)
;;; Copyright (c) 2016, Daniel Kochmański (daniel@turtleware.eu)
;;;
;;; See file 'LICENSE' for the copyright details
;;;

(defsystem #:mcclim-bitmaps
  :description "Support for GIF, PNG, JPEG and TIFF images in McCLIM ~
  bitmap reading functions."
  :depends-on (:mcclim :skippy :png-read :cl-jpeg :retrospectiff)
  :components ((:file "gif")
               (:file "png")
               (:file "jpeg")
               (:file "tiff")))
