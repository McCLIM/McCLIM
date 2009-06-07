;;; -*- lisp -*-

(defpackage :clim-listener.system
  (:use :cl :asdf))

(in-package :clim-listener.system)

(defsystem :clim-listener
    :depends-on (:mcclim #+sbcl :sb-posix)
    :components
    ((:module "Apps/Listener"
              :pathname #.(make-pathname :directory '(:relative "Apps" "Listener"))
              :components
              ((:file "package")
               (:file "appearance" :depends-on ("package"))
               (:file "util" :depends-on ("package"))
               (:file "icons" :depends-on ("package" "util"))
               (:file "file-types" :depends-on ("package" "icons" "util"))
               (:file "asdf" :depends-on ("package"))
               (:file "dev-commands" 
                      :depends-on ("package" "appearance" "icons" "file-types" "util" "asdf"))
               (:file "wholine" :depends-on ("package" "dev-commands" "util"))
               (:file "listener"
                      :depends-on ("package" "wholine" "file-types" "icons" "dev-commands" "util"))
               #+CMU (:file "cmu-hacks" :depends-on ("package"))))))

