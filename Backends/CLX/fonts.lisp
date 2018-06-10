;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  fonts.lisp  -- Font access abstraction for CLX backend
;;;;
;;;;  Copyright (c) 2016, Daniel Kochmański
;;;;
;;;;    see toplevel file 'copyright' for the copyright details.
;;;;

;; (defpackage #:mcclim-clx/fonts
;;   (:use :clim-clx :clim-lisp)
;;   (:export #:font-ascent
;;            #:font-descent
;;            #:font-glyph-width))

;; (in-package #:mcclim-clx/fonts)

(in-package #:clim-clx)

(defparameter *clx-text-sizes*
  '(:normal         12
    :tiny            8
    :very-small      8
    :small          10
    :large          14
    :very-large     18
    :huge           24))

(defconstant *families/names*
  '(:fix         "adobe-courier"
    :serif       "adobe-times"
    :sans-serif  "adobe-helvetica"))

(defparameter *families/faces*
  '(;; "adobe-courier"
    ((:fix :roman)                 . "medium-r")
    ((:fix :bold)                  . "bold-r")
    ((:fix :italic)                . "medium-o")
    ((:fix (:bold :italic))        . "bold-o")
    ((:fix (:italic :bold))        . "bold-o")
    ;; "adome-times"
    ((:sans :roman)                . "medium-r")
    ((:sans :bold)                 . "bold-r")
    ((:sans :italic)               . "medium-i")
    ((:sans (:bold :italic))       . "bold-i")
    ((:sans (:italic :bold))       . "boid-i")
    ;; "adobe-helvetica"
    ((:sans-serif :roman)          . "medium-r")
    ((:sans-serif :bold)           . "bold-r")
    ((:sans-serif :italic)         . "medium-o")
    ((:sans-serif (:bold :italic)) . "bold-o")
    ((:sans-serif (:italic :bold)) . "bold-o")))

(clim-internals::define-protocol-class font-renderer ())

(defclass clx-standard-font-renderer (font-renderer)
  ())

(defun open-font (display font-name)
  (let ((fonts (xlib:list-font-names display font-name :max-fonts 1)))
    (when fonts
      (xlib:open-font display (first fonts)))))

(defun text-style-to-x-font (port text-style)
  (lookup-text-style-to-x-font port (clx-port-font-renderer port) text-style))

(defgeneric lookup-text-style-to-x-font (port font-renderer text-style)
  (:method ((port t) (font-renderer t) (text-style t))
    (let ((text-style (parse-text-style text-style)))
      (labels
          ((find-and-make-xlib-face (display family face size)
             (let* ((family-name (if (stringp family)
                                     family
                                     (getf *families/names* family)))
                    (face-name (if (stringp face)
                                   face
                                   (assoc (list family face) *families/faces*
                                          :test #'equal))))
               (flet ((try (encoding)
                        (open-font display
                                   (format nil "-~a-~a-*-*-~d-*-*-*-*-*-~a"
                                           family-name face-name size encoding))))
;;; xxx: this part is a bit problematic - we either list all fonts
;;; with any possible encoding (what leads to the situation, when our
;;; font can't render a simple string "abcd") or we end with only a
;;; partial list of fonts. since we have mcclim-ttf extension which
;;; handles unicode characters well, this mechanism of getting fonts
;;; is deprecated and there is no big harm.
                 (or (try "iso8859-1")
                     (xlib:open-font display "fixed")))))
           (find-font ()
             (multiple-value-bind (family face size)
                 (text-style-components text-style)

               (setf face   (or face :roman)
                     family (or family :fix)
                     size   (or size :normal)
                     size   (round (getf clim-clx::*clx-text-sizes* size size)))

               (when (zerop size)
                 (setf size (getf clim-clx::*clx-text-sizes* :normal)))

               (let ((display (clim-clx::clx-port-display port)))
                 (find-and-make-xlib-face display family face size)))))
        (or (text-style-mapping port text-style)
            (setf (climi::text-style-mapping port text-style)
                  (find-font)))))))

;;; the generic function port-character-width might be intended to be
;;; common for all ports, but in fact, that symbol is in the clim-clx
;;; package, so it is only defined here, and nowhere used. 
(defgeneric port-character-width (port text-style char))

(defmethod port-character-width ((port clx-basic-port) text-style char)
  (let* ((font (text-style-to-x-font port text-style))
	 (width (xlib:char-width font (char-code char))))
    width))

;;; the generic function port-string-width might be intended to be
;;; common for all ports, but in fact, that symbol is in the clim-clx
;;; package, so it is only defined here, and nowhere used. 
(defgeneric port-string-width (port text-style string &key start end))

(defmethod port-string-width ((port clx-basic-port) text-style string &key (start 0) end)
  (xlib:text-width (text-style-to-x-font port text-style)
		   string :start start :end end))



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
(defgeneric font-text-extents (font string &key start end translate direction)
  (:method (font string
            &key (start 0) (end (length string)) (translate #'translate) direction)
    (declare (ignore direction))
    (xlib:text-extents font string
                       :start start :end end
                       :translate translate)))

(defgeneric font-draw-glyphs (font mirror gc x y string
                              &key start end translate size direction transformation)
  (:method (font mirror gc x y string
            &key (start 0) (end (length string)) (translate #'translate) (size 16) direction transformation)
    (declare (ignore font direction))
    (multiple-value-bind (x y)
        (transform-position transformation x y)
      (xlib:draw-glyphs mirror gc (truncate (+ x 0.5)) (truncate (+ y 0.5)) string
                        :start start :end end :translate translate :size size))))


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

(defgeneric port-find-all-font-families (port font-renderer &key invalidate-cache)
  (:method (port font-renderer &key invalidate-cache)
    (when (or (null (clim-clx::font-families port)) invalidate-cache)
      (setf (font-families port) (reload-font-table port)))
    (font-families port)))

(defmethod clim-extensions:port-all-font-families ((port clx-basic-port) &key invalidate-cache)
  (append (call-next-method)
          (port-find-all-font-families port (clx-port-font-renderer port) :invalidate-cache invalidate-cache)))

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
