(defpackage :mcclim-freetype
    (:use :climi :clim :clim-lisp)
    (:export :*freetype-font-path*)
    (:import-from #+(or cmu scl) :alien
                  #+sbcl :sb-alien
                  :slot :make-alien :alien :deref))
