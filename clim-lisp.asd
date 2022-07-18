
(defsystem "clim-lisp"
  :serial t
  :depends-on ("alexandria"
               
               
               "closer-mop"
               #+(or) "log4cl")
  :components (;; First possible patches
               (:file "patch")
               (:module "Lisp-Dep"
                :components
                ((:file "fix-acl" :if-feature :excl)
                 (:file "fix-clisp" :if-feature :clisp)))
               (:file "package")))
