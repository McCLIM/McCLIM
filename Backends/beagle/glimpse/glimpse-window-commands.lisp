
(in-package :glimpse)

(define-glimpse-command (com-describe-sheet :name t)
  ((obj 'sheet
	:prompt "sheet"
	:gesture :describe))
  (declare (special *full-report*))
  (let* ((pane (get-frame-pane *application-frame* 'out))
	 (medium (sheet-medium pane)))
    (with-text-size (medium :very-small)
      (stream-terpri pane)
      (if *full-report*
	  (describe obj pane)
	(describe-sheet obj pane))
      ;; hack to ensure sheet changes are mirrored on the display device
      ;; (scroll-bars updated etc.)
      (change-space-requirements pane
	 :width (bounding-rectangle-width pane)
	 :height (bounding-rectangle-height pane)))))

(define-glimpse-command (com-toggle-output-style :name t) ()
  (declare (special *tree-output-type*))
  (if (eq *tree-output-type* :text)
      (setf *tree-output-type* :graph)
    (setf *tree-output-type* :text))
  ;; Redraw the window view.
  (com-show-sheet-hierarchy))

