(in-package :common-lisp-user)

(defpackage "MY-FIRST-APP"
  ;; Imports the appropriate CLIM library
  (:use :clim :clim-lisp)

  ;; The package will only export a function to run the app
  (:export "MY-FIRST-APP-MAIN"))

;; Good practice
(in-package :my-first-app)

;; Definition of the structure of a minimum app
(define-application-frame my-first-clim-app ()
  ()

  ;; This app only has 1 pane
  (:panes
   (my-interactor :interactor
		  :height 400
		  :width 600))
  
  ;; :layouts section describes how the pane is positioned inside
  ;; the application frame.
  ;; With 1 pane, no point getting complicated, Default is fine...
  (:layouts
    (my-default my-interactor)))

;; Now that the structure of the app is defined, need a function
;; to launch an instance of this app. (The user could run
;; several instances of the same app.)
(defun my-first-app-main ()
  (run-frame-top-level (make-application-frame 'my-first-clim-app)))

