(in-package #:mcclim-sdl2)

;;; A complete sheet should subclass basic-sheet and pick its mixins:
;;;
;;;   Input protocol (xor):
;;;
;;;     standard-sheet-input-mixin
;;;     delegate-sheet-input-mixin
;;;     immediate-sheet-input-mixin
;;;     sheet-mute-input-mixin
;;;
;;;   Output protocol:
;;;
;;;     standard-sheet-output-mixin xor sheet-mute-output-mixin
;;;     sheet-with-medium-mixin
;;;         permanent-medium-sheet-output-mixin
;;;         temporary-medium-sheet-output-mixin
;;;
;;;   Genealogy:
;;;
;;;     sheet-parent-mixin
;;;     sheet-leaf-mixin xor sheet-single-child-mixin xor sheet-multiple-child-mixin
;;;
;;;   Repainting (xor):
;;;
;;;     standard-repainting-mixin
;;;     immediate-repainting-mixin
;;;     sheet-mute-repainting-mixin
;;;
;;;   Geometry (xor):
;;;
;;;     sheet-identity-transformation-mixin
;;;     sheet-translation-mixin
;;;     sheet-y-inverting-transformation-mixin
;;;     sheet-transformation-mixin
;;;
;;;   Windowing (zero or more, may be mixed, the order of mixins is important)
;;;
;;;     top-level-sheet-mixin
;;;     unmanaged-sheet-mixin
;;;     mirrored-sheet-mixin
;;;

(defvar *glider*
  (make-pattern-from-bitmap-file
   (asdf:component-pathname
    (asdf:find-component "clim-examples" '("images" "glider.png")))))

(defclass plain-sheet (;; repainting
                       immediate-repainting-mixin
                       ;; input
                       immediate-sheet-input-mixin
                       ;; output
                       permanent-medium-sheet-output-mixin
                       ;temporary-medium-sheet-output-mixin
                       ;sheet-with-medium-mixin
                       ;sheet-mute-output-mixin
                       ;; geometry
                       sheet-transformation-mixin
                       ;; genealogy
                       sheet-parent-mixin
                       sheet-leaf-mixin
                       ;; windowing
                       top-level-sheet-mixin
                       mirrored-sheet-mixin
                       ;; the base class
                       basic-sheet)
  ()
  (:default-initargs :icon *glider*
                     :pretty-name "McCLIM Test Sheet"
                     :region (make-rectangle* -200 -200 200 200)
                     :transformation (make-scaling-transformation 2 2)))

(defmethod handle-event ((sheet plain-sheet) event)
  (log:info "Unhandled event ~s has arrived." (class-name (class-of event))))

(defun open-plain-sheet (path &optional restartp)
  (let ((port (find-port :server-path path)))
    (when restartp
      (restart-port port))
    (let (;; FIXME supplying the :PORT for the plain-sheet instance is a
          ;; kludge in the core and shouldn't be necessary.
          (sheet (make-instance 'plain-sheet :port port))
          (graft (find-graft :port port)))
      (sheet-adopt-child graft sheet)
      ;; FIXME CLX thinks that every tpl sheet is adopted by a frame.
      (port-enable-sheet port sheet)
      sheet)))

(defun close-plain-sheet (sheet)
  (sheet-disown-child (graft sheet) sheet)
  nil)
