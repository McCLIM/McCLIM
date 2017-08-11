
(defsystem #:clim-lisp
  :serial t
  :depends-on (#:alexandria
               #:trivial-gray-streams
               #:closer-mop
               #+(or) #:log4cl)
  :components (;; First possible patches
               (:file "patch")
               (:module "Lisp-Dep"
                        :components
                        (#+(or cmu scl excl sbcl openmcl lispworks clisp ecl)
                           (:file   #+excl      "fix-acl"
                                    #+clisp     "fix-clisp")))
               (:file "package")))
