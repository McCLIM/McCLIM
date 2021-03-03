;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MCCLIM-TRUETYPE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Font matrics, caching, and XRender text support
;;;   Created: 2003-05-25 16:32
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2003 by Gilbert Baumann
;;;  (c) copyright 2008 by Andy Hefner
;;;  (c) copyright 2016-2021 by Daniel Kochmański
;;;
;;;    See toplevel file 'Copyright' for the copyright details.
;;;

(in-package #:clim-clx)

(declaim (optimize (speed 1) (safety 3) (debug 1) (space 0)))

;;;; Notes

;;; You might need to tweak mcclim-truetype:*families/faces* to point
;;; to where ever there are suitable TTF fonts on your system.

;;; FIXME: Not particularly thread safe.


(defclass clx-ttf-port (clim-clx:clx-render-port)
  ((glyph-set
    :initform nil
    :accessor glyph-set)
   (next-glyph-id
    :initform 0
    :accessor next-glyph-id)))

(defmethod climb:find-port-type ((port (eql :clx-ttf)))
  (values 'clx-ttf-port (nth-value 1 (climb:find-port-type :clx))))

(defun make-glyph-set (display)
  (xlib:render-create-glyph-set
   (first (xlib:find-matching-picture-formats
           display
           :alpha 8 :red 0 :green 0 :blue 0))))

(defun ensure-glyph-set (port)
  (or (glyph-set port)
      (setf (glyph-set port) (make-glyph-set (clx-port-display port)))))

(defun free-glyph-set (port)
  (alexandria:when-let ((glyph-set (glyph-set port)))
    (xlib:render-free-glyph-set glyph-set)
    (setf (glyph-set port) nil)))

(defun draw-glyph-id (port)
  (incf (next-glyph-id port)))

(defclass clx-truetype-font (mcclim-truetype:cached-truetype-font)
  ((port     :initarg :port :reader port)
   (display  :initarg :display :reader clx-truetype-font-display)
   (%buffer% :initform (make-array 1024
                                   :element-type '(unsigned-byte 32)
                                   :adjustable nil
                                   :fill-pointer nil)
             :accessor clx-truetype-font-%buffer%
             :type (simple-array (unsigned-byte 32)))))

(defun register-all-ttf-fonts
    (port &optional (dir mcclim-truetype:*truetype-font-path*))
  (dolist (path (and dir (directory (merge-pathnames "*.ttf" dir))))
    ;; make-truetype-font make fail if zpb can't load the particular
    ;; file - in that case it signals an error and no font is
    ;; created. In that case we just skip that file- hence IGNORE-ERRORS.
    (ignore-errors
     (map () #'(lambda (size)
                 (make-truetype-font port path size))
          '(8 10 12 14 18 24 48 72)))))

(defmethod clime:port-all-font-families ((port clx-ttf-port) &key invalidate-cache)
  (when (or (null (font-families port)) invalidate-cache)
    (setf (font-families port) nil))
  (register-all-ttf-fonts port)
  (font-families port))

(let ((font-loader-cache (make-hash-table :test #'equal))
      (font-families     (make-hash-table :test #'equal))
      (font-faces        (make-hash-table :test #'equal))
      (font-cache        (make-hash-table :test #'equal))
      (text-style-cache  (make-hash-table :test #'equal)))
  (defun make-truetype-font (port filename size)
    (clim-sys:with-lock-held (mcclim-truetype:*zpb-font-lock*)
      (let* ((display (clx-port-display port))
             (loader (alexandria:ensure-gethash
                      filename font-loader-cache
                      (zpb-ttf:open-font-loader filename)))
             (family-name (zpb-ttf:family-name loader))
             (family (alexandria:ensure-gethash
                      family-name font-families
                      (make-instance 'mcclim-truetype:truetype-font-family
                                     :port port
                                     :name (zpb-ttf:family-name loader))))
             (face-name (zpb-ttf:subfamily-name loader))
             (font-face (alexandria:ensure-gethash
                         (list family-name face-name) font-faces
                         (make-instance 'mcclim-truetype:truetype-face
                                        :family family
                                        :name (zpb-ttf:subfamily-name loader)
                                        :loader loader)))
	     (font (alexandria:ensure-gethash
                    (list display loader size) font-cache
                    (make-instance 'clx-truetype-font
                                   :port port
                                   :display display
                                   :face font-face
                                   :size size))))
        (pushnew family (font-families port))
        (alexandria:ensure-gethash
         (list port (make-text-style family-name face-name size))
         text-style-cache
         font))))

  (defun find-truetype-font (port text-style)
    (gethash (list port text-style) text-style-cache)))


(defstruct truetype-device-font-name
  (font-file (error "missing argument"))
  (size      (error "missing argument")))

(defstruct fontconfig-font-name
  (string (error "missing argument"))
  (size   (error "missing argument"))
  (options nil)
  (device-name nil))

(define-condition missing-font (simple-error)
  ((filename :reader missing-font-filename :initarg :filename)
   (text-style :reader missing-font-text-style :initarg :text-style))
  (:report (lambda (condition stream)
             (format stream  "Cannot access ~W (~a)
Your *truetype-font-path* is currently ~W
The following files should exist:~&~{  ~A~^~%~}"
                     (missing-font-filename condition)
                     (missing-font-text-style condition)
                     mcclim-truetype:*truetype-font-path*
                     (mapcar #'cdr mcclim-truetype:*families/faces*)))))

(defmethod text-style-mapping ((port clx-ttf-port)
                               (text-style climi::device-font-text-style)
                               &optional character-set)
  (declare (ignore character-set))
  (let ((font-name (climi::device-font-name text-style)))
    (when (stringp font-name)
      (setf (climi::device-font-name text-style)
            (make-fontconfig-font-name :string font-name
                                       :size (climb:normalize-font-size :normal))
            font-name (climi::device-font-name text-style)))
    (etypecase font-name
      (truetype-device-font-name
       (make-truetype-font port
                           (namestring (truetype-device-font-name-font-file font-name))
                           (truetype-device-font-name-size font-name)))
      (fontconfig-font-name
       (text-style-mapping
        port
        (or (fontconfig-font-name-device-name font-name)
            (setf (fontconfig-font-name-device-name font-name)
                  (make-device-font-text-style
                   port
                   (make-truetype-device-font-name
                    :font-file (mcclim-truetype:find-fontconfig-font
                                (format nil "~A-~A~{:~A~}"
                                        (namestring (fontconfig-font-name-string font-name))
                                        (fontconfig-font-name-size font-name)
                                        (fontconfig-font-name-options font-name)))
                    :size (fontconfig-font-name-size font-name))))))))))

(defmethod text-style-mapping ((port clx-ttf-port) (text-style standard-text-style)
                               &optional character-set
                               &aux (text-style (climb:parse-text-style* text-style)))
  (declare (ignore character-set))
  (labels
      ((find-and-make-truetype-font (family face size)
         (let* ((font-path-maybe-relative
                  (cdr (assoc (list family face) mcclim-truetype:*families/faces*
                              :test #'equal)))
                (font-path
                  (and font-path-maybe-relative
                       (case (car (pathname-directory
                                   font-path-maybe-relative))
                         (:absolute font-path-maybe-relative)
                         (otherwise
                          (merge-pathnames
                           font-path-maybe-relative
                           (or mcclim-truetype:*truetype-font-path* "")))))))
           (if (and font-path (probe-file font-path))
               (make-truetype-font port font-path size)
               (error 'missing-font
                      :filename font-path
                      :text-style text-style))))
       (find-font ()
         (multiple-value-call #'find-and-make-truetype-font
           (clim:text-style-components text-style))))
    (or (find-truetype-font port text-style)
        (mcclim-truetype:invoke-with-truetype-path-restart #'find-font))))

