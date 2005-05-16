
(in-package :glimpse)

(define-glimpse-command (com-describe :name t)
  ((obj 'expression
	:prompt "object"
	:gesture :describe))
  (let* ((pane (get-frame-pane *application-frame* 'out))
	 (medium (sheet-medium pane)))
    (with-text-size (medium :very-small)  ; :tiny :small :very-small
      (stream-terpri pane)
      (describe obj pane)
      ;; hack to ensure sheet changes are mirrored on the display device
      ;; (scroll-bars updated etc.)
      (change-space-requirements pane
	 :width (bounding-rectangle-width pane)
	 :height (bounding-rectangle-height pane)))))

(define-glimpse-command (com-clear-detail-window :name nil) ()
  (window-clear (get-frame-pane *application-frame* 'out)))

(define-glimpse-command (com-quit :name nil) ()
  (frame-exit *application-frame*))

(define-glimpse-command (com-show-sheet-hierarchy :name nil
;; Do after def cmd table hierarchy ->    :command-table glimpse-mode-commands
					  :menu t
;; Work out how to add these ->		  :keystroke
					  ) ()
  ;; Enable the "toggle output style" gadget, if it's disabled...
  (let ((gadget (find-gadget *application-frame* :name 'toggle-output)))
    (when gadget
      (activate-gadget gadget)))
  (let* ((pane (get-frame-pane *application-frame* 'app))
	 (medium (sheet-medium pane)))
    ;; We lose the output history, but I think for our purposes that's ok
    (window-clear pane)
    (with-text-size (medium :very-small)
      (display-sheet-hierarchy *application-frame* pane))
    ;; Hack - not sure if this is a bug, or not. Suspect it *is*.
    (change-space-requirements pane
			       :width (bounding-rectangle-width pane)
			       :height (bounding-rectangle-height pane))))


(define-glimpse-command (com-show-processes :name t) ()
  ;; Disable the "toggle output style" gadget, if it's disabled...
  (let ((gadget (find-gadget *application-frame* :name 'toggle-output)))
    (when gadget
      (deactivate-gadget gadget)))
  (let* ((pane (get-frame-pane *application-frame* 'app))
	 (medium (sheet-medium pane)))
    ;; We lose the output history, but I think for our purposes that's ok
    (window-clear pane)
    (with-text-size (medium :very-small)
      (display-processes *application-frame* pane))
    (change-space-requirements pane
			       :width (bounding-rectangle-width pane)
			       :height (bounding-rectangle-height pane))))
