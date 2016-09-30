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


;;; Font listing implementation

(defclass clx-font-family (clim-extensions:font-family)
  ((all-faces :initform nil
              :accessor all-faces
              :reader clim-extensions:font-family-all-faces)
   (raw-name :initarg :raw-name
             :reader clx-font-family-raw-name)))

(defclass clx-font-face (clim-extensions:font-face)
  ((all-sizes :initform nil
              :accessor all-sizes
              :reader clim-extensions:font-face-all-sizes)
   (raw-name :initarg :raw-name
             :reader clx-font-face-raw-name)))

(defmethod clim-extensions:port-all-font-families :around
    ((port clx-port) &key invalidate-cache)
  (when (or (not (slot-boundp port 'font-families)) invalidate-cache)
    (setf (font-families port) (reload-font-table port)))
  (append (call-next-method)
          (font-families port)))

(defun split-font-name (name)
  (loop
     repeat 12
     for next = (position #\- name :start 0)
     :then (position #\- name :start (1+ next))
     and prev = nil then next
     while next
     when prev
     collect (subseq name (1+ prev) next)))

;;; XXX: xlib can acquire very large set of fonts from xserver, but
;;; most of them doesn't have a pixelsize or doesn't handle even basic
;;; encodings. To make the result as clean as possible we load only
;;; fonts which we know that can render correctly basic text. For more
;;; fancy needs we have ttf fonts which work well with unicode. xorg
;;; fonts are deprecated.
(defun reload-font-table (port)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (font (xlib:list-font-names (clx-port-display port) "*-iso8859-1"))
      (destructuring-bind
            (&optional foundry family weight slant setwidth style pixelsize
                       ;;pointsize xresolution yresolution
                       ;;spacing averagewidth registry encoding
                       &rest ignore)
          (split-font-name font)
        (declare (ignore setwidth style ignore))
        (setf pixelsize (and pixelsize (parse-integer pixelsize)))
        (unless (or (null foundry)
                    (null pixelsize)
                    (zerop pixelsize))
          (let* ((family-name (format nil "~A ~A" foundry family))
                 (family-name* (format nil "~A-~A" foundry family))
                 (family-instance
                  (or (gethash family-name table)
                      (setf (gethash family-name table)
                            (make-instance 'clx-font-family
                                           :port port
                                           :name family-name
                                           :raw-name family-name*))))
                 (face-name (format nil "~A ~A" weight slant))
                 (face-name* (format nil "~A-~A" weight slant))
                 (face-instance
                  (find face-name (all-faces family-instance)
                        :key #'clim-extensions:font-face-name
                        :test #'equal)))
            (unless face-instance
              (setf face-instance
                    (make-instance 'clx-font-face
                                   :family family-instance
                                   :name face-name
                                   :raw-name face-name*))
              (push face-instance (all-faces family-instance)))
            (pushnew pixelsize (all-sizes face-instance))))))
    (sort (loop
             for family being each hash-value in table
             do
               (setf (all-faces family)
                     (sort (all-faces family)
                           #'string<
                           :key #'clim-extensions:font-face-name))
               (dolist (face (all-faces family))
                 (setf (all-sizes face)
                       #- (or) (sort (all-sizes face) #'<)))
             collect family)
          #'string<
          :key #'clim-extensions:font-family-name)))

(defmethod clim-extensions:font-face-text-style
    ((face clx-font-face) &optional size)
  (make-text-style
   (clx-font-family-raw-name
    (clim-extensions:font-face-family face))
   (clx-font-face-raw-name face)
   size))
