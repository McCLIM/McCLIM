
(defsystem #:mcclim-tooltips
  :depends-on (#:clim)
  :description "Tooltip code donated by SRI"
  :long-description 
  "To use tooltips w/ clim presentations, define after methods for the clim
presentation-method clim:highlight-presentation.  When the state argument
is :highlight, the method should call draw-tooltip.  When the state
argument is :unhighlight, the method should call erase-tooltip.  Here's
a sample call (which assumes the fn get-tooltip-text returns a string 
or NIL):

(clim:define-presentation-method clim:highlight-presentation :after
  ((type t) record stream (state (eql :highlight)))
  (draw-tooltip stream (get-tooltip-text record)
                :region (orec-relative->absolute-region record stream)))

(clim:define-presentation-method clim:highlight-presentation :after
  ((type t) record stream (state (eql :unhighlight)))
  (declare (ignore record))
  (erase-tooltip stream))"
  :components ((:file "tooltips")))
