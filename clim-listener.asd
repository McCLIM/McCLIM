;;; -*- lisp -*-

(defpackage :clim-listener.system
  (:use :cl :asdf))

(in-package :clim-listener.system)

(defsystem :clim-listener
    :depends-on (:mcclim #+sbcl :sb-posix :mcclim-images :mcclim-images-xpm)
    :components
    ((:file "Experimental/xpm"
            :pathname #.(make-pathname :directory '(:relative "Experimental") :name "xpm" :type "lisp"))
     (:module "Apps/Listener"
              :pathname #.(make-pathname :directory '(:relative "Apps" "Listener"))
              :depends-on ("Experimental/xpm")
              :components
              ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "icons" :depends-on ("package" "util"))
               (:file "file-types" :depends-on ("package" "icons" "util"))
               (:file "dev-commands" :depends-on ("package" "icons" "file-types" "util"))
               (:file "wholine" :depends-on ("package" "dev-commands" "util"))
               (:file "listener" :depends-on ("package" "wholine" "file-types" "icons" "dev-commands" "util"))
               
               #+CMU (:file "cmu-hacks" :depends-on ("package"))))))