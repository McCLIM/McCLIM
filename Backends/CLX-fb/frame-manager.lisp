;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2016 Alessandro Serra <gas2serra@gmail.com>
;;;  (c) copyright 2018-2020 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The frame manager the CLX framebuffer backend.

(in-package #:clim-clx-fb)

(defclass clx-fb-frame-manager (clim-clx::clx-frame-manager)
  ()
  (:default-initargs :mirroring :single
                     :class-gensym (gensym "CLXFB")))

(defmethod find-concrete-pane-class ((fm clx-fb-frame-manager)
                                     pane-type &optional (errorp t))
  ;; This backend doesn't have any specialized pane implementations
  ;; but depending on circumstances it may add optional mirroring to
  ;; the class. Such automatically defined concrete class has the same
  ;; name but with a gensym prefix and symbol in the backend package.
  (let ((concrete-pane-class (find-concrete-pane-class t pane-type errorp)))
    (maybe-add-mirroring-superclasses
     concrete-pane-class (mirroring fm)
     (symbol-name (class-gensym fm)) (find-package '#:clim-clx-fb)
     (lambda (concrete-pane-class)
       `(,(find-class 'clx-fb-mirrored-sheet-mixin)
         ,(find-class 'climi::always-repaint-background-mixin)
         ,@(unless (subtypep concrete-pane-class 'sheet-with-medium-mixin)
             `(;; temporary-medium-sheet-output-mixin
               ,(find-class 'permanent-medium-sheet-output-mixin)))
         ,concrete-pane-class)))))
