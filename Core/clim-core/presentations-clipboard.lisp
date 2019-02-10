(in-package :clim-internals)

(define-presentation-generic-function %convert-clipboard-content convert-clipboard-content
  (type-key parameters options object type output-type check-only))

(define-presentation-method convert-clipboard-content (obj (type t) output-type check-only)
  (log:info "Default conversion for ~s (type: ~s)" obj type)
  nil)

(define-presentation-method convert-clipboard-content (obj (type string) (output-type (eql :string)) check-only)
  (log:info "Converting string to string: ~s" obj)
  (check-type obj string)
  obj)

(defun convert-clipboard-content (obj output-type &key type check-only)
  (funcall-presentation-generic-function convert-clipboard-content
                                         obj
                                         (or type (presentation-type-of obj))
                                         output-type
                                         check-only))
