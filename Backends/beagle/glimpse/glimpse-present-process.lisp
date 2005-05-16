
(in-package :glimpse)

;;; Looks to me like there's NO way to make this portable... shouldn't CLIM
;;; define a PROCESS _type_? ::FIXME::
(define-presentation-to-command-translator process-select-to-describe-command
  (ccl::process          ; from-type (need to generalize this, if possible)
   com-describe          ; command-name - maybe com-describe-presentation?
   glimpse               ; command table containing command
   :gesture :select      ; activate on :select (left-button click)
   ;; :tester ...
   :documentation
   "Invoke the 'describe presentation' command on the selected process"
   :pointer-documentation ((object stream) (format stream "Describe process ~A" (type-of object)))
   :menu t               ; command should appear in popped-up menus
;   :priority 1123
   :echo nil)            ; don't echo the command when it is invoked
  ;; arglist must be a subset (using string-equal) of:
  ;;    (object presentation context-type frame event window x y)
  (object)
  ;; body of translator; returns a list of the arguments to the command
  ;; named by command-name.
  (list object))

