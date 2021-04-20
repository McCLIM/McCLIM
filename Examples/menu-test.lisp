;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2000 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) Copyright 2021 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;

(defpackage #:clim-demo.menu-test
  (:use #:clim #:clim-extensions #:clim-lisp)
  (:export #:menu-test))

(in-package #:clim-demo.menu-test)

(define-application-frame menu-test ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (screen :application
           :display-time nil
           :text-style (make-text-style :sans-serif :roman :normal)))
  (:layouts
   (defaults (vertically () screen))))

(define-menu-test-command com-file ()
  (format *standard-output* "You pressed the File button.~%")
  (finish-output *standard-output*))

(define-menu-test-command com-toggle-file ()
  (format *standard-output* "You toggled the File button.~%")
  (setf (command-enabled 'com-file *application-frame*)
        (not (command-enabled 'com-file *application-frame*)))
  (finish-output *standard-output*))

(define-menu-test-command com-kenobi ()
  (format *standard-output* "You pressed the General Kenobi button.~%")
  (finish-output *standard-output*))

(define-menu-test-command com-toggle-kenobi ()
  (format *standard-output* "You toggled the Kenobi button.~%")
  (setf (command-enabled 'com-kenobi *application-frame*)
        (not (command-enabled 'com-kenobi *application-frame*)))
  (finish-output *standard-output*))

(define-menu-test-command com-konichiwa ()
  (format *standard-output* "You pressed the Konichiwa button.~%")
  (finish-output *standard-output*))

(define-menu-test-command com-get-in-the-robot ()
  (format *standard-output* "You pressed the Get in the robot button.~%")
  (finish-output *standard-output*))

(define-menu-test-command com-hi ()
  (format *standard-output* "You pressed the Hi button.~%")
  (finish-output *standard-output*))

(define-menu-test-command (com-more :name t)
    ((a 'string :prompt "String"))
  (format *standard-output* "You accepted ~s.~%" a)
  (finish-output *standard-output*))

(make-command-table 'kenobi-command-table
                    :errorp nil
                    :menu '(("General Kenobi"   :command com-kenobi
                                                :text-style (nil :bold nil))
                            ("Konichiwa"        :command com-konichiwa
                                                :text-style (nil :italic nil))
                            ("Get in the robot" :command com-get-in-the-robot
                                                :text-style (:fix nil :large))))

(make-command-table 'buffer-command-table
                    :errorp nil
                    :menu '(("Hello there" :menu kenobi-command-table)
                            ("Hi there"    :command com-hi)
                            ("Disabled"    :command com-disabled)))

(defun get-kenobi (gesture numeric)
  (declare (ignore gesture numeric))
  '(com-kenobi))

(define-command-table menubar-command-table
  :menu (("Buffer" :menu buffer-command-table)
         ("File"   :command com-file)
         ("Toggle File"   :command com-toggle-file)
         ("Toggle Kenobi" :command com-toggle-kenobi)
         ;; horizontal divider
         ("divider" :divider nil)
         ;; literal submenu (McCLIM extension)
         ("About"   :menu (("McCLIM"
                            :menu (("super!" :divider nil)
                                   ("kenobi" :command (com-kenobi))
                                   ("extra!" :divider nil
                                             :text-style (nil :bold nil))
                                   ("kenobi" :function get-kenobi)))
                           ("Lisp"
                            :menu (("More" :command com-more)
                                   (nil :divider nil)
                                   ("empty"  :menu nil)))))))
