
(defsystem "clim-lisp"
  :serial t
  :depends-on ("alexandria"
               "trivial-gray-streams"
               "trivial-features"
               "closer-mop"
               #+(or) "log4cl")
  :components (;; First possible patches
               (:file "patch")
               (:module "Lisp-Dep"
                :components
                ((:file "fix-acl" :if-feature :excl)
                 (:file "fix-clisp" :if-feature :clisp)))
               (:file "package")))
