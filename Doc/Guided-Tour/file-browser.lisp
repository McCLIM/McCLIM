(eval-when (:compile-toplevel)
  (asdf:oos 'asdf:load-op :clim)
  (asdf:oos 'asdf:load-op :clim-clx))

(in-package :clim-user)

; LTAG-start:file-browser-all
(define-application-frame file-browser ()
  ((active-files :initform nil :accessor active-files))
  (:panes
   (file-browser :application
		 :display-function '(dirlist-display-files)
		 ;; Call the display-function whenever the command
		 ;; loop makes a ``full-cycle''
		 :display-time :command-loop)
   (interactor :interactor))
  (:layouts (default (vertically ()
				 file-browser
				 interactor))))

(defmethod dirlist-display-files ((frame file-browser) pane)
  ;; Clear old displayed entries
  (clear-output-record (stream-output-history pane))

  (dolist (file (active-files frame))
    ;; Instead of write-string, we use present so that the link to
    ;; object file and the semantic information that file is
    ;; pathname is retained.
    (present file 'pathname :stream pane) 
    (terpri pane)))

(define-file-browser-command (com-edit-directory :name "Edit Directory")
  ((dir 'pathname))
  (let ((dir (make-pathname :directory (pathname-directory dir)
			    :name :wild :type :wild :version :wild
			    :defaults dir)))
    (setf (active-files *application-frame*)
	  (directory dir))))

(define-presentation-to-command-translator pathname-to-edit-command
    (pathname                           ; source presentation-type
     com-edit-directory                 ; target-command
     file-browser                       ; command-table
     :gesture :select                   ; use this translator for pointer clicks
     :documentation "Edit this path")   ; used in context menu
    (object)                            ; argument List
  (list object))                        ; arguments for target-command

(defmethod adopt-frame :after (frame-manager (frame file-browser))
  (execute-frame-command frame
    `(com-edit-directory ,(make-pathname :directory '(:absolute)))))
; LTAG-end