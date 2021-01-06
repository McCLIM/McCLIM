* Tooltip Documentation

  To use tooltips with clim presentations, define after methods for
  the clim presentation-method clim:highlight-presentation. When the
  state argument is :highlight, the method should call draw-tooltip.
  When the state argument is :unhighlight, the method should call
  erase-tooltip.

** Example Code

   Here is a worked example that adds tooltips to the German Towns
   example and the Address Book example.

*** German Towns Example
    Here's a method to take a presentation object and return the text
    the tooltip should display. This is not part of the API but just
    done for convenience; getting the text can be done however you
    want.

**** German Towns tooltip text
     (defmethod get-tooltip-text ((object clim-demo.town-example::town))
       (format nil "~A has ~:d inhabitants."
               (clim-demo.town-example::town-name object)
               (or (clim-demo.town-example::town-population object)
	           "some")))

**** :AFTER methods
     There are two :AFTER presentation methods for the German Towns
     example.

     The first specializes on the TOWN presentation object type and
     the state being EQL :HIGHLIGHT. It calls DRAW-TOOLTIP.

***** German Towns highlight :AFTER method
     (define-presentation-method highlight-presentation :after
       ((type clim-demo.town-example::town) record stream (state (eql :highlight)))
       (draw-tooltip stream (get-tooltip-text (clim-internals::presentation-object record))
                     :region (orec-relative->absolute-region record stream)))

     The second specializes on the TOWN object type and the state
     being EQL :UNHIGHLIGHT. It calls ERASE-TOOLTIP.

***** German Towns unhighlight :AFTER method
     (define-presentation-method highlight-presentation :after
       ((type clim-demo.town-example::town) record stream (state (eql :unhighlight)))
       (declare (ignore record))
       (erase-tooltip stream))


*** Address Book example
    The Address Book example is almost identical to the German Towns
    example, except its GET-TOOLTIP-TEXT method does something
    different and it specializes on the ADDRESS object type.

**** Address Book text
     (defmethod get-tooltip-text ((object clim-demo.address-book::address))
       (with-output-to-string (s nil :element-type 'base-char)
         (clim-demo.address-book::display-address object s)))

**** :AFTER methods for Address Book tooltips

***** Address Book highlight :AFTER method
     (define-presentation-method highlight-presentation :after
       ((type clim-demo.address-book::address) record stream (state (eql :highlight)))
       (draw-tooltip stream (get-tooltip-text (clim-internals::presentation-object record))
                     :region (orec-relative->absolute-region record stream)))

***** Address Book unhighlight :AFTER method
     (define-presentation-method highlight-presentation :after
       ((type clim-demo.address-book::address) record stream (state (eql :unhighlight)))
       (declare (ignore record))
       (erase-tooltip stream))


** API Documentation
*** Functions
**** DRAW-TOOLTIP

Draw a tooltip. Called from a presentation highlight method.

draw-tooltip stream text
             &KEY region x y
	     (text-render-fn #'draw-text*)
	     (ink *tooltip-ink*)
	     (text-style *tooltip-text-style*)
	     (wrap-p *tooltip-wrap-p*)
	     (wrap-width *tooltip-wrap-width*)

Required Arguments:
     stream --- a clim stream
     text   --- the tooltip text

Keyword Arguments:
     region         --- try to put the tooltip near this region
     x, y           --- put the tooltip at x,y
     text-render-fn --- function to draw the text (default: #'draw-text*)
     ink            --- clim ink for drawing the text (default +black+)
     text-style     --- font face (default sans-serif, roman, small)
     wrap-p         --- whether to wrap the text (default t)
     wrap-width     --- how many characters before wrapping (default 40)


**** ERASE-TOOLTIP

Erase a tooltip drawn by DRAW-TOOLTIP. Called from a presentation
unhighlight method.

erase-tooltip stream

Required argument:
     stream --- a clim stream

**** Customization Variables

These variables supply defaults to the DRAW-TOOLTIP function.

     *tooltip-color*       --- color of tooltip background. Default: pale yellow
     *tooltip-ink*         --- color of text. Default: +black+
     *tooltip-text-style*  --- text style (face) of tooltip. Default: sans-serif, roman, small
     *tooltip-wrap-p*      --- wrap tooltip text? Default: t
     *tooltip-wrap-width*  --- characters per line before wrapping. Default: 40
     *tooltip-delay*       --- delay before popping up tooltip. Default: 0.25 s
