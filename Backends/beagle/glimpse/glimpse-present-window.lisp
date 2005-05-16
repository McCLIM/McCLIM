
(in-package :glimpse)

;;; Convert a basic :select gesture on a PORT presentation type to a
;;; 'describe object' command. This needs to be applicable when
;;; we're in the 'standard' input context for an interactor pane.

(define-presentation-to-command-translator port-select-to-describe-command
  (port                  ; from-type
   com-describe          ; command-name - maybe com-describe-presentation?
   glimpse               ; command table containing command
   :gesture :select      ; activate on :select (left-button click)
   ;; :tester ...
   :documentation
   "Invoke the 'describe presentation' command on the selected port"
   :pointer-documentation ((object stream) (format stream "Describe port ~A" (type-of object)))
   :menu t               ; command should appear in popped-up menus
;   :priority 1123
   :echo nil)            ; don't echo the command when it is invoked
  ;; arglist must be a subset (using string-equal) of:
  ;;    (object presentation context-type frame event window x y)
  (object)
  ;; body of translator; returns a list of the arguments to the command
  ;; named by command-name.
  (list object))


(define-presentation-to-command-translator graft-select-to-describe-command
  (graft                 ; from-type
   com-describe          ; command-name - maybe com-describe-presentation?
   glimpse               ; command table containing command
   :gesture :select      ; activate on :select (left-button click)
   ;; :tester ...
   :documentation
   "Invoke the 'describe presentation' command on the selected graft"
   :pointer-documentation ((object stream) (format stream "Describe graft ~A" (type-of object)))
   :menu t               ; command should appear in popped-up menus
;   :priority 1123
   :echo nil)            ; don't echo the command when it is invoked
  ;; arglist must be a subset (using string-equal) of:
  ;;    (object presentation context-type frame event window x y)
  (object)
  ;; body of translator; returns a list of the arguments to the command
  ;; named by command-name.
  (list object))

(define-presentation-to-command-translator pane-select-to-describe-command
  (pane                  ; from-type
   com-describe
;;   com-describe-sheet    ; command-name - maybe com-describe-presentation?
   glimpse               ; command table containing command
   :gesture :select      ; activate on :select (left-button click)
   ;; :tester ...
   :documentation
   "Invoke the 'describe presentation' command on the selected pane"
   :pointer-documentation ((object stream) (format stream "Describe pane ~A" (type-of object)))
   :menu t               ; command should appear in popped-up menus
;   :priority 1123
   :echo nil)            ; don't echo the command when it is invoked
  ;; arglist must be a subset (using string-equal) of:
  ;;    (object presentation context-type frame event window x y)
  (object)
  ;; body of translator; returns a list of the arguments to the command
  ;; named by command-name.
  (list object))
