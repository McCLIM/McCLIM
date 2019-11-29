
(defsystem #:clim-lisp
  :serial t
  :depends-on (#:alexandria
               #:trivial-gray-streams
               #:closer-mop
               #:log4cl)
  :components (;; First possible patches
               (:file "patch")
               (:module "Lisp-Dep"
                        :components
                        (#+(or cmu scl excl sbcl openmcl lispworks clisp ecl)
                           (:file   #+armedbear "fix-abcl"
                                    #+cmu       "fix-cmu"
                                    #+scl       "fix-scl"
                                    #+excl      "fix-acl"
                                    #+sbcl      "fix-sbcl"
                                    #+openmcl   "fix-openmcl"
                                    #+lispworks "fix-lispworks"
                                    #+clisp     "fix-clisp"
                                    #+ecl       "fix-ecl")))
               (:file "package")))
