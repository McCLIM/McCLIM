;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  fonts.lisp  -- Font access abstraction for CLX backend
;;;;
;;;;  Copyright (c) 2016, Daniel Kochmański
;;;;
;;;;    See toplevel file 'Copyright' for the copyright details.
;;;;

;; (defpackage #:mcclim-clx/fonts
;;   (:use :clim-clx :clim-lisp)
;;   (:export #:font-ascent
;;            #:font-descent
;;            #:font-glyph-width))

;; (in-package #:mcclim-clx/fonts)

(in-package #:clim-clx)



(defgeneric font-ascent (font)
  (:method (font)
    (xlib:font-ascent font)))

(defgeneric font-descent (font)
  (:method (font)
    (xlib:font-descent font)))

(defgeneric font-glyph-width (font char)
  (:method (font char)
    (xlib:char-width font (char-code char))))

;;; This function should return nine values:
;;;
;;; (width ascent descent left right font-ascent font-descent
;;; direction first-not-done)
(defgeneric font-text-extents (font string &key start end translate)
  (:method (font string
            &key (start 0) (end (length string)) (translate #'translate))
    (xlib:text-extents font string
                       :start start :end end
                       :translate translate)))

(defgeneric font-draw-glyphs (font mirror gc x y string
                              &key start end translate size)
  (:method (font mirror gc x y string
            &key (start 0) (end (length string)) (translate #'translate) (size 16))
    (declare (ignore font))
    (xlib:draw-glyphs mirror gc x y string
                      :start start :end end :translate translate :size size)))
