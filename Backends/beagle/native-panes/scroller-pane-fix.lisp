
(in-package :clim-internals)


(setf *scrollbar-thickness* (ccl::send (ccl::@class ns:ns-scroller)
				       :scroller-width-for-control-size
				       #$NSRegularControlSize))


;;; Should the side of the scroller-pane that the vertical scrollbar
;;; appears be configurable?
(defmethod allocate-space ((pane scroller-pane) width height)
  (with-slots (viewport vscrollbar hscrollbar) pane
    (let ((viewport-width (if vscrollbar
			      (- width *scrollbar-thickness*)
			    width))
	  (viewport-height (if hscrollbar
			       (- height *scrollbar-thickness*)
			     height)))
      (when vscrollbar
	;; Position scrollbar to be on the RHS of the pane. Shouldn't the
	;; 'y' position be incremented if there's a horizontal scroller too?
	(setf (sheet-transformation vscrollbar)
	      (make-translation-transformation viewport-width 0))
	;; allocate space (width x height)
	(allocate-space vscrollbar
			*scrollbar-thickness*
			(if hscrollbar
			    (- height *scrollbar-thickness*)
			  height)))

      (when hscrollbar
	;; why is previous 'set-transformation' and this one 'move-sheet'?
	;; Whatever, we need to always have hscrollbar at 0 in x axis.
        (move-sheet hscrollbar
		    0
                    (- height *scrollbar-thickness*))
	;; allocate (width x height)
        (allocate-space hscrollbar
                        (if vscrollbar (- width *scrollbar-thickness*) width)
                        *scrollbar-thickness*))
      ;;
      ;; Recalculate the gadget-values of the scrollbars
      ;;
      (when vscrollbar
        (let* ((scrollee (first (sheet-children viewport)))
               (min 0)
               (max (- (max (space-requirement-height (compose-space scrollee))
                            viewport-height)
                       viewport-height))
               (ts  viewport-height)
               (val (if (zerop (gadget-max-value vscrollbar))
                        0
                        (* (/ (gadget-value vscrollbar) (gadget-max-value vscrollbar))
                           max))))
          (setf (gadget-min-value vscrollbar) min
                (gadget-max-value vscrollbar) max
                (scroll-bar-thumb-size vscrollbar) ts
                (gadget-value vscrollbar :invoke-callback nil) val)))
      
      (when hscrollbar
        (let* ((scrollee (first (sheet-children viewport)))
               (min 0)
               (max (- (max (space-requirement-width (compose-space scrollee))
                            viewport-width)
                       viewport-width))
               (ts  viewport-width)
               (val (if (zerop (gadget-max-value hscrollbar))
                        0
                        (* (/ (gadget-value hscrollbar) (gadget-max-value hscrollbar))
                           max))))
          (setf (gadget-min-value hscrollbar) min
                (gadget-max-value hscrollbar) max
                (scroll-bar-thumb-size hscrollbar) ts
                (gadget-value hscrollbar :invoke-callback nil) val)))

      ;; Is this right? Viewport is at 0,0? Yes, because of flipped coords. Doh.
      (when viewport
        (setf (sheet-transformation viewport)
              (make-translation-transformation 0 0))
        (allocate-space viewport
                        viewport-width
                        viewport-height)))))
