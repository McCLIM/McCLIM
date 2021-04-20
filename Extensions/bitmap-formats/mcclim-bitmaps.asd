;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2008 Troels Henriksen <athas@sigkill.dk>
;;;  (c) Copyright 2009 Samium Gromoff <_deepfire@feelingofgreen.ru>
;;;  (c) Copyright 2009 Cyrus Harmon <ch-lisp@bobobeach.com>
;;;  (c) Copyright 2016 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:asdf-user)

(defsystem "mcclim-bitmaps"
  :description "Support for various image formats in McCLIM."
  :long-description "Support for various image formats in McCLIM
bitmap reading functions.

Currently supported formats are the formats covered by opticl
library and XPM format."
  :depends-on ("clim-basic" "opticl")
  :components ((:file "xpm")
               (:file "bitmaps" :depends-on ("xpm"))))
