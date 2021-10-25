;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2008 by Andy Hefner <ahefner@gmail.com>
;;;  (c) Copyright 2016-2021 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; This file configures TTF paths and maps them to the standard text styles.
;;;

(in-package #:mcclim-truetype)

;;; This path may be used by clime:port-all-font-familes by the
;;; port-specific implementation to list "system" fonts.
(defparameter *truetype-font-path*
  (find-if #'probe-file
           '(#p"/usr/share/fonts/truetype/ttf-dejavu/"
             #p"/usr/share/fonts/truetype/dejavu/"
             #p"/usr/share/fonts/dejavu/"
             #p"/usr/share/fonts/truetype/"
             #p"/usr/share/fonts/TTF/"
             #p"/usr/share/fonts/"
             #p"/usr/local/share/fonts/dejavu/"
             #p"/usr/X11R6/lib/X11/fonts/TTF/"
             #p"/usr/X11R7/lib/X11/fonts/TTF/"
             #p"/opt/X11/share/fonts/TTF/"
             #p"/opt/X11/share/fonts/"
             #p"~/.guix-profile/share/fonts/truetype/"
             #p"/Library/Fonts/"
             #p"C:/Windows/Fonts/")))

(defvar *families/faces* nil)

(defun invoke-with-truetype-path-restart (continuation)
  (restart-case (funcall continuation)
    (change-font-path (new-path)
      :report (lambda (stream)
                (format stream "Retry with alternate truetype font path"))
      :interactive (lambda ()
                     (format *query-io* "Enter new value: ")
                     (list (read-line)))
      (setf *truetype-font-path* new-path)
      (invoke-with-truetype-path-restart continuation))))

(defun default-font/family-map ()
  (flet ((try-ttf (name)
           (or (cl-dejavu:font-pathname name)
               (error "Can't find the pathname for the font ~s." name))))
    `(((:fix :roman)                 . ,(try-ttf "DejaVuSansMono.ttf" ))
      ((:fix :italic)                . ,(try-ttf "DejaVuSansMono-Oblique.ttf"))
      ((:fix (:bold :italic))        . ,(try-ttf "DejaVuSansMono-BoldOblique.ttf"))
      ((:fix (:italic :bold))        . ,(try-ttf "DejaVuSansMono-BoldOblique.ttf"))
      ((:fix :bold)                  . ,(try-ttf "DejaVuSansMono-Bold.ttf"))
      ((:serif :roman)               . ,(try-ttf "DejaVuSerif.ttf"))
      ((:serif :italic)              . ,(try-ttf "DejaVuSerif-Italic.ttf"))
      ((:serif (:bold :italic))      . ,(try-ttf "DejaVuSerif-BoldItalic.ttf"))
      ((:serif (:italic :bold))      . ,(try-ttf "DejaVuSerif-BoldItalic.ttf"))
      ((:serif :bold)                . ,(try-ttf "DejaVuSerif-Bold.ttf"))
      ((:sans-serif :roman)          . ,(try-ttf "DejaVuSans.ttf"))
      ((:sans-serif :italic)         . ,(try-ttf "DejaVuSans-Oblique.ttf"))
      ((:sans-serif (:bold :italic)) . ,(try-ttf "DejaVuSans-BoldOblique.ttf"))
      ((:sans-serif (:italic :bold)) . ,(try-ttf "DejaVuSans-BoldOblique.ttf"))
      ((:sans-serif :bold)           . ,(try-ttf "DejaVuSans-Bold.ttf")))))

(defun autoconfigure-fonts ()
  (setf *families/faces* (default-font/family-map)))

(eval-when (:load-toplevel :execute)
  (autoconfigure-fonts))


;;; `fc-match' implementation

(defun parse-fontconfig-output (s)
  (let* ((match-string (concatenate 'string (string #\Tab) "file:"))
         (matching-line
          (loop for l = (read-line s nil nil)
                while l
                if (= (mismatch l match-string) (length match-string))
                   do (return l)))
         (filename (when matching-line
                     (probe-file
                      (subseq matching-line
                              (1+ (position #\" matching-line :from-end nil :test #'char=))
                              (position #\" matching-line :from-end t   :test #'char=))))))
    (when filename
      (parse-namestring filename))))

(defun find-fontconfig-font (font-fc-name)
  (multiple-value-bind (output errors code)
      (uiop:run-program (list "fc-match" "-v" font-fc-name)
                        :output :string :input nil :error-output nil
                        :force-shell t :ignore-error-status t)
    (declare (ignore errors))
    (if (not (zerop code))
        (warn "~&fc-match failed with code ~D.~%" code)
        (with-input-from-string (stream output)
          (parse-fontconfig-output stream)))))
