;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :CLIM-INTERNALS)

(eval-when (eval load compile)
  (let ((externals '(abort-gesture
		     abort-gesture-event
		     *abort-gestures*
		     accelerator-gesture
		     accelerator-gesture-event
		     accelerator-gesture-numeric-argument
		     *accelerator-gestures*
		     accept
		     accept
		     accept-1
		     accept-from-string
		     accepting-values
		     accept-present-default
		     accept-values
		     accept-values-command-button
		     accept-values-resynchronize
		     action-gadget
		     activate-callback
		     activate-gadget
		     activation-gesture-p
		     *activation-gestures*
		     add-character-output-to-text-record
		     add-command-to-command-table
		     add-gesture-name
		     add-keystroke-to-command-table
		     add-menu-item-to-command-table
		     add-output-record
		     add-presentation-translator-to-command-table
		     add-string-output-to-text-record
		     add-watcher
		     adjust-item-list-cells
		     adjust-multiple-columns
		     adjust-table-cells
		     adopt-frame
		     allocate-pixmap
		     allocate-resource
		     allocate-space
		     all-processes
		     and
		     application-frame
		     *application-frame*
		     application-frame-p
		     application-pane
		     apply-presentation-generic-function
		     area
		     areap
		     armed-callback
		     augment-draw-set
		     +background-ink+
		     bboard-pane
		     beep
		     blank-area
		     boolean
		     bordering
		     border-pane
		     bounding-rectangle*
		     bounding-rectangle
		     bounding-rectangle-height
		     bounding-rectangle-max-x
		     bounding-rectangle-max-y
		     bounding-rectangle-min-x
		     bounding-rectangle-min-y
		     bounding-rectangle-p
		     bounding-rectangle-position
		     bounding-rectangle-size
		     bounding-rectangle-width
		     bury-sheet
		     cache-output-record
		     call-presentation-menu
		     call-presentation-translator
		     cell-align-x
		     cell-align-y
		     cell-min-height
		     cell-min-width
		     cell-output-record
		     cell-output-record-p
		     change-space-requirements
		     changing-space-requirements
		     character
		     child-containing-position
		     children-overlapping-rectangle*
		     children-overlapping-region
		     class-presentation-type-name
		     clear-output-record
		     clear-resource
		     client-setting
		     clim-stream-pane
		     close
		     color
		     color-ihs
		     colorp
		     color-rgb
		     column-output-record
		     column-output-record-p
		     command
		     command-accessible-in-command-table-p
		     command-already-present
		     *command-argument-delimiters*
		     command-arguments
		     *command-dispatchers*
		     command-enabled
		     command-enabled
		     command-line-command-parser
		     command-line-command-unparser
		     command-line-name-for-command
		     command-line-read-remaining-arguments-for-partial-command
		     command-menu-item-options
		     command-menu-item-type
		     command-menu-item-value
		     command-menu-pane
		     command-name
		     command-name
		     *command-name-delimiters*
		     command-not-accessible
		     command-not-present
		     command-or-form
		     *command-parser*
		     command-present-in-command-table-p
		     command-table
		     command-table-already-exists
		     command-table-complete-input
		     command-table-error
		     command-table-inherit-from
		     command-table-name
		     command-table-not-found
		     command-table-p
		     *command-unparser*
		     complete-from-generator
		     complete-from-possibilities
		     complete-input
		     completing-from-suggestions
		     completion
		     *completion-gestures*
		     complex
		     compose-in
		     compose-out
		     compose-over
		     compose-rotation-with-transformation
		     compose-scaling-with-transformation
		     compose-space
		     compose-transformations
		     compose-transformation-with-rotation
		     compose-transformation-with-scaling
		     compose-transformation-with-translation
		     compose-translation-with-transformation
		     compute-difference-set
		     compute-new-output-records
		     contrasting-dash-pattern-limit
		     contrasting-inks-limit
		     +control-key+
		     coordinate
		     copy-area
		     copy-from-pixmap
		     copy-to-pixmap
		     current-process
		     cursor
		     cursorp
		     cursor-position
		     cursor-sheet
		     cursor-visibility
		     cursor-visibility
		     deactivate-gadget
		     deallocate-pixmap
		     deallocate-resource
		     decache-child-output-record
		     default-describe-presentation-type
		     *default-frame-manager*
		     default-frame-top-level
		     *default-server-path*
		     *default-text-style*
		     defgeneric*
		     define-application-frame
		     define-border-type
		     define-command
		     define-command-table
		     define-default-presentation-method
		     define-drag-and-drop-translator
		     define-gesture-name
		     define-graph-type
		     define-presentation-action
		     define-presentation-generic-function
		     define-presentation-method
		     define-presentation-to-command-translator
		     define-presentation-translator
		     define-presentation-type
		     define-presentation-type-abbreviation
		     defmethod*
		     defresource
		     delegate-sheet-delegate
		     delegate-sheet-delegate
		     delegate-sheet-input-mixin
		     delete-gesture-name
		     delete-output-record
		     delete-watcher
		     delimiter-gesture-p
		     *delimiter-gestures*
		     describe-presentation-type
		     describe-presentation-type
		     design
		     designp
		     destroy-port
		     destroy-process
		     device-event
		     disable-frame
		     disarmed-callback
		     disown-frame
		     dispatch-event
		     dispatch-repaint
		     display-command-menu
		     display-command-table-menu
		     display-cursor
		     displayed-output-record
		     displayed-output-record-p
		     display-exit-boxes
		     distribute-event
		     do-command-table-inheritance
		     document-presentation-translator
		     drag-callback
		     drag-callback
		     dragging-output
		     drag-output-record
		     draw-arrow
		     draw-arrow*
		     draw-circle
		     draw-circle*
		     draw-design
		     draw-ellipse
		     draw-ellipse*
		     draw-line
		     draw-line*
		     draw-lines
		     draw-lines*
		     draw-oval
		     draw-oval*
		     draw-pattern*
		     draw-point
		     draw-point*
		     draw-points
		     draw-points*
		     draw-polygon
		     draw-polygon*
		     draw-rectangle*
		     draw-standard-menu
		     draw-text
		     draw-text*
		     draw-triangle
		     draw-triangle*
		     ellipse
		     ellipse-center-point
		     ellipse-center-point*
		     ellipse-end-angle
		     ellipsep
		     ellipse-radii
		     ellipse-start-angle
		     elliptical-arc
		     elliptical-arc-p
		     enable-frame
		     encapsulating-stream
		     encapsulating-stream-p
		     encapsulating-stream-stream
		     erase-input-buffer
		     erase-output-record
		     even-scaling-transformation-p
		     event
		     event-listen
		     event-matches-gesture-name-p
		     event-modifier-state
		     eventp
		     event-peek
		     event-read
		     event-read-no-hang
		     event-sheet
		     event-timestamp
		     event-type
		     event-unread
		     +everywhere+
		     execute-frame-command
		     expand-presentation-type-abbreviation
		     expand-presentation-type-abbreviation-1
		     expression
		     extended-input-stream
		     extended-input-stream-p
		     extended-output-stream
		     extended-output-stream-p
		     +fill+
		     filling-output
		     find-applicable-translators
		     find-cached-output-record
		     find-child-output-record
		     find-command-from-command-line-name
		     find-command-table
		     find-frame-manager
		     find-graft
		     find-innermost-applicable-presentation
		     find-keystroke-item
		     find-menu-item
		     find-pane-for-frame
		     find-pane-named
		     find-port
		     find-presentation-translator
		     find-presentation-translators
		     find-presentation-type-class
		     +flipping-ink+
		     float
		     +foreground-ink+
		     form
		     format-graph-from-roots
		     format-items
		     format-textual-list
		     formatting-cell
		     formatting-column
		     formatting-item-list
		     formatting-row
		     formatting-table
		     frame-calling-frame
		     frame-command-table
		     frame-command-table
		     frame-current-layout
		     frame-current-layout
		     frame-current-panes
		     frame-document-highlighted-presentation
		     frame-drag-and-drop-feedback
		     frame-drag-and-drop-highlighting
		     frame-error-output
		     frame-exit
		     frame-exit
		     frame-find-innermost-applicable-presentation
		     frame-input-context-button-press-handler
		     frame-maintain-presentation-histories
		     frame-manager
		     frame-manager
		     frame-manager
		     frame-manager-frames
		     frame-mananger-p
		     frame-name
		     frame-pane
		     frame-panes
		     frame-parent
		     frame-pointer-documentation-output
		     frame-pretty-name
		     frame-pretty-name
		     frame-properties
		     frame-properties
		     frame-query-io
		     frame-replay
		     frame-standard-input
		     frame-standard-output
		     frame-state
		     frame-top-level-sheet
		     funcall-presentation-generic-function
		     fundamental-binary-input-stream
		     fundamental-binary-output-stream
		     fundamental-binary-stream
		     fundamental-character-input-stream
		     fundamental-character-output-stream
		     fundamental-character-stream
		     fundamental-input-stream
		     fundamental-output-stream
		     fundamental-stream
		     gadget
		     gadget-activate-callback
		     gadget-armed-callback
		     gadget-client
		     gadget-client
		     gadget-dialog-view
		     +gadget-dialog-view+
		     gadget-disarmed-callback
		     gadget-id
		     gadget-id
		     gadget-label
		     gadget-label
		     gadget-label-align-x
		     gadget-label-align-x
		     gadget-label-align-y
		     gadget-label-align-y
		     gadget-label-text-style
		     gadget-label-text-style
		     gadget-max-value
		     gadget-max-value
		     gadget-menu-view
		     +gadget-menu-view+
		     gadget-min-value
		     gadget-min-value
		     gadget-orientation
		     gadget-output-record
		     gadgetp
		     gadget-show-value-p
		     gadget-value
		     gadget-value
		     gadget-value
		     gadget-value
		     gadget-value
		     gadget-value
		     gadget-value
		     gadget-value
		     gadget-value-changed-callback
		     gadget-view
		     +gadget-view+
		     generate-graph-nodes
		     generate-panes
		     gesture-processing-handler
		     gesture-processing-handler
		     get-frame-pane
		     global-command-table
		     graft
		     graft-height
		     graft-orientation
		     graft-pixels-per-inch
		     graft-pixels-per-millimeter
		     graft-units
		     graft-width
		     graphics-displayed-output-record
		     graphics-displayed-output-record-p
		     graph-node-children
		     graph-node-children
		     graph-node-object
		     graph-node-output-record
		     graph-node-output-record-p
		     graph-node-parents
		     graph-node-parents
		     graph-output-record
		     graph-output-record-p
		     graph-root-nodes
		     graph-root-nodes
		     grid-pane
		     handle-event
		     handle-repaint
		     hbox-pane
		     *help-gestures*
		     highlight-applicable-presentation
		     highlight-output-record
		     highlight-presentation
		     horizontally
		     hrack-pane
		     +hyper-key+
		     +identity-transformation+
		     identity-transformation-p
		     immediate-repainting-mixin
		     immediate-rescan
		     immediate-sheet-input-mixin
		     incremental-redisplay
		     indenting-output
		     *input-context*
		     input-editing-stream
		     input-editing-stream-p
		     input-not-of-required-type
		     input-not-of-required-type
		     input-stream-p
		     *input-wait-handler*
		     *input-wait-test*
		     integer
		     interactive-stream-p
		     interactor-pane
		     invalidate-cached-regions
		     invalidate-cached-transformations
		     invertible-transformation-p
		     invert-transformation
		     invoke-accept-values-command-button
		     invoke-updating-output
		     invoke-with-drawing-options
		     invoke-with-new-output-record
		     invoke-with-output-recording-options
		     invoke-with-output-to-output-record
		     invoke-with-text-style
		     item-list-output-record
		     item-list-output-record-p
		     keyboard-event
		     keyboard-event-character
		     keyboard-event-key-name
		     key-modifier-state-match-p
		     key-press-event
		     key-release-event
		     keyword
		     labelled
		     labelled-gadget
		     label-pane
		     layout-frame
		     layout-graph-edges
		     layout-graph-nodes
		     line
		     line-end-point*
		     line-end-point
		     linep
		     line-start-point*
		     line-start-point
		     line-style
		     line-style-cap-shape
		     line-style-dashes
		     line-style-joint-shape
		     line-style-p
		     line-style-thickness
		     line-style-unit
		     lookup-keystroke-command-item
		     lookup-keystroke-item
		     make-3-point-transformation
		     make-3-point-transformation*
		     make-application-frame
		     make-bounding-rectangle
		     make-clim-application-pane
		     make-clim-interactor-pane
		     make-clim-stream-pane
		     make-command-table
		     make-contrasting-dash-patterns
		     make-contrasting-inks
		     make-design-from-output-record
		     make-device-font-text-style
		     make-ellipse
		     make-ellipse*
		     make-elliptical-arc
		     make-elliptical-arc*
		     make-flipping-ink
		     make-gray-color
		     make-ihs-color
		     make-line*
		     make-line-style
		     make-lock
		     make-opacity
		     make-pane
		     make-pane-1
		     make-pattern
		     make-point
		     make-polygon
		     make-polygon*
		     make-polyline
		     make-polyline*
		     make-presentation-type-specifier
		     make-process
		     make-rectangle
		     make-rectangle*
		     make-rectangular-tile
		     make-recursive-lock
		     make-reflection-transformation
		     make-reflection-transformation*
		     make-rgb-color
		     make-rotation-transformation
		     make-rotation-transformation*
		     make-scaling-transformation
		     make-scaling-transformation*
		     make-space-requirement
		     make-stencil
		     make-text-style
		     make-transformation
		     make-translation-transformation
		     map-over-command-table-commands
		     map-over-command-table-keystrokes
		     map-over-command-table-menu-items
		     map-over-command-table-names
		     map-over-command-table-translators
		     map-over-grafts
		     map-over-item-list-cells
		     map-over-output-records-containing-position
		     map-over-output-records-overlapping-region
		     map-over-polygon-coordinates
		     map-over-polygon-segments
		     map-over-ports
		     map-over-presentation-type-supertypes
		     map-over-presentation-type-supertypes
		     map-over-region-set-regions
		     map-over-row-cells
		     map-over-row-cells
		     map-over-table-elements
		     map-resource
		     map-sheet-position-to-child
		     map-sheet-position-to-parent
		     map-sheet-rectangle*-to-child
		     map-sheet-rectangle*-to-parent
		     match-output-records
		     medium
		     medium-background
		     medium-background
		     medium-background
		     medium-background
		     medium-buffering-output-p
		     medium-buffering-output-p
		     medium-clipping-region
		     medium-clipping-region
		     medium-clipping-region
		     medium-clipping-region
		     medium-current-text-style
		     medium-default-text-style
		     medium-default-text-style
		     medium-default-text-style
		     medium-default-text-style
		     medium-draw-ellipse*
		     medium-draw-line*
		     medium-draw-lines*
		     medium-draw-point*
		     medium-draw-points*
		     medium-draw-polygon*
		     medium-draw-rectangle*
		     medium-draw-text*
		     medium-foreground
		     medium-foreground
		     medium-foreground
		     medium-foreground
		     medium-ink
		     medium-ink
		     medium-ink
		     medium-ink
		     medium-line-style
		     medium-line-style
		     medium-line-style
		     medium-line-style
		     medium-merged-text-style
		     mediump
		     medium-text-style
		     medium-text-style
		     medium-text-style
		     medium-text-style
		     medium-transformation
		     medium-transformation
		     medium-transformation
		     medium-transformation
		     member
		     member-alist
		     member-sequence
		     menu-button
		     menu-button-pane
		     menu-choose
		     menu-choose-command-from-command-table
		     menu-choose-from-drawer
		     menu-command-parser
		     menu-item-display
		     menu-item-options
		     menu-item-value
		     menu-read-remaining-arguments-for-partial-command
		     merge-text-styles
		     +meta-key+
		     modifier-state-matches-gesture-name-p
		     mute-repainting-mixin
		     mute-sheet-input-mixin
		     mute-sheet-output-mixin
		     new-page
		     nil
		     note-command-disabled
		     note-command-enabled
		     note-frame-state-changed
		     note-gadget-activated
		     note-gadget-deactivated
		     note-output-record-child-changed
		     note-sheet-adopted
		     note-sheet-degrafted
		     note-sheet-disabled
		     note-sheet-disowned
		     note-sheet-enabled
		     note-sheet-grafted
		     note-sheet-region-changed
		     note-sheet-transformation-changed
		     note-space-requirements-changed
		     notify-user
		     +nowhere+
		     null
		     null-or-type
		     *null-presentation*
		     number
		     *numeric-argument-marker*
		     opacity
		     opacityp
		     opacity-value
		     open-stream-p
		     or
		     oriented-gadget
		     *original-stream*
		     output-record
		     output-record-cache-value
		     output-record-children
		     output-record-contents-ok
		     output-record-count
		     output-record-displayer
		     output-record-end-cursor-position
		     output-record-end-cursor-position
		     output-record-fixed-position
		     output-record-hit-detection-rectangle*
		     output-recording-stream
		     output-recording-stream-p
		     output-record-p
		     output-record-parent
		     output-record-position
		     output-record-position
		     output-record-refined-sensitivity-test
		     output-record-start-cursor-position
		     output-record-start-cursor-position
		     output-record-unique-id
		     output-stream-p
		     pane
		     pane-background
		     pane-foreground
		     pane-frame
		     pane-name
		     pane-needs-redisplay
		     panep
		     pane-scroller
		     panes-need-redisplay
		     pane-viewport
		     pane-viewport-region
		     parse-text-style
		     partial-command-p
		     *partial-command-parser*
		     path
		     pathname
		     pathp
		     permanent-medium-sheet-output-mixin
		     pixmap-depth
		     pixmap-height
		     pixmap-width
		     point
		     pointer
		     pointer-button-click-and-hold-event
		     pointer-button-click-event
		     pointer-button-double-click-event
		     pointer-button-event
		     pointer-button-hold-event
		     pointer-button-press-event
		     *pointer-button-press-handler*
		     pointer-button-release-event
		     pointer-buttons
		     pointer-cursor
		     pointer-cursor
		     *pointer-documentation-output*
		     pointer-documentation-pane
		     pointer-documentation-view
		     +pointer-documentation-view+
		     pointer-enter-event
		     pointer-event
		     pointer-event-button
		     pointer-event-native-x
		     pointer-event-native-y
		     pointer-event-pointer
		     pointer-event-x
		     pointer-event-y
		     pointer-exit-event
		     +pointer-left-button+
		     +pointer-middle-button+
		     pointer-motion-event
		     pointerp
		     pointer-port
		     pointer-position
		     pointer-position
		     +pointer-right-button+
		     pointer-sheet
		     pointer-sheet
		     pointp
		     point-position
		     point-x
		     point-y
		     polygon
		     polygonp
		     polygon-points
		     polyline
		     polyline-closed
		     polylinep
		     port
		     port-draw-character*
		     port-draw-ellipse*
		     port-draw-line*
		     port-draw-lines*
		     port-draw-point*
		     port-draw-points*
		     port-draw-polygon*
		     port-draw-rectangle*
		     port-draw-string*
		     port-keyboard-input-focus
		     port-keyboard-input-focus
		     port-properties
		     port-properties
		     port-server-path
		     *possibilities-gestures*
		     present
		     present
		     presentation
		     presentation-default-preprocessor
		     presentation-matches-context-type
		     presentation-modifier
		     presentation-object
		     presentation-object
		     presentationp
		     presentation-refined-position-test
		     presentation-replace-input
		     presentation-single-box
		     presentation-single-box
		     presentation-subtypep
		     presentation-subtypep
		     presentation-type
		     presentation-type
		     presentation-type-direct-supertypes
		     presentation-type-history
		     presentation-type-name
		     presentation-type-of
		     presentation-type-options
		     presentation-typep
		     presentation-typep
		     presentation-type-parameters
		     presentation-type-specifier-p
		     presentation-type-specifier-p
		     present-to-string
		     print-menu-item
		     process-interrupt
		     process-next-event
		     process-wait
		     process-wait-with-timeout
		     process-yield
		     prompt-for-accept
		     prompt-for-accept-1
		     propagate-output-record-changes
		     propagate-output-record-changes-p
		     push-button
		     push-button-pane
		     push-button-show-as-default-p
		     queue-event
		     queue-repaint
		     queue-rescan
		     radio-box
		     radio-box-current-selection
		     radio-box-current-selection
		     radio-box-pane
		     raise-sheet
		     range-gadget-mixin
		     ratio
		     rational
		     read-command
		     read-command-using-keystrokes
		     read-frame-command
		     read-gesture
		     read-token
		     real
		     realize-mirror
		     recompute-contents-ok
		     recompute-extent-for-changed-child
		     recompute-extent-for-new-child
		     rectangle
		     rectangle-edges*
		     rectangle-height
		     rectangle-max-point
		     rectangle-max-x
		     rectangle-max-y
		     rectangle-min-point
		     rectangle-min-x
		     rectangle-min-y
		     rectanglep
		     rectangle-size
		     rectangle-width
		     rectilinear-transformation-p
		     redisplay
		     redisplay-frame-pane
		     redisplay-frame-panes
		     redisplay-output-record
		     redraw-input-buffer
		     reflection-transformation-p
		     reflection-underspecified
		     region
		     region-contains-position-p
		     region-contains-region-p
		     region-difference
		     region-equal
		     region-intersection
		     region-intersects-region-p
		     regionp
		     region-set
		     region-set-p
		     region-set-regions
		     region-union
		     remove-command-from-command-table
		     remove-keystroke-from-command-table
		     remove-menu-item-from-command-table
		     remove-presentation-translator-from-command-table
		     reorder-sheets
		     repaint-sheet
		     replace-input
		     replay
		     replay-output-record
		     rescan-if-necessary
		     reset-frame
		     reset-scan-pointer
		     reset-watcher
		     restart-port
		     restraining
		     restraining-pane
		     rigid-transformation-p
		     row-output-record
		     row-output-record-p
		     run-frame-top-level
		     run-frame-top-level
		     scaling-transformation-p
		     scroll-bar
		     scroll-bar-drag-callback
		     scroll-bar-drag-down-line-callback
		     scroll-bar-drag-down-page-callback
		     scroll-bar-drag-up-line-callback
		     scroll-bar-drag-up-page-callback
		     scroll-bar-pane
		     scroll-bar-scroll-to-bottom-callback
		     scroll-bar-scroll-to-top-callback
		     scroll-down-line-callback
		     scroll-down-page-callback
		     scroller-pane
		     scroll-extent
		     scrolling
		     scroll-to-bottom-callback
		     scroll-to-top-callback
		     scroll-up-line-callback
		     scroll-up-page-callback
		     sequence
		     sequence-enumerated
		     set-highlighted-presentation
		     sheet
		     sheet-adopt-child
		     sheet-allocated-region
		     sheet-ancestor-p
		     sheet-children
		     sheet-delta-transformation
		     sheet-device-region
		     sheet-device-transformation
		     sheet-direct-mirror
		     sheet-disown-child
		     sheet-enabled-children
		     sheet-enabled-p
		     sheet-enabled-p
		     sheet-grafted-p
		     sheet-identity-transformation-mixin
		     sheet-leaf-mixin
		     sheet-medium
		     sheet-mirror
		     sheet-mirrored-ancestor
		     sheet-multiple-child-mixin
		     sheet-native-region
		     sheet-native-transformation
		     sheet-occluding-sheets
		     sheetp
		     sheet-parent
		     sheet-parent-mixin
		     sheet-region
		     sheet-region
		     sheet-siblings
		     sheet-single-child-mixin
		     sheet-transformation
		     sheet-transformation
		     sheet-transformation-mixin
		     sheet-translation-mixin
		     sheet-viewable-p
		     sheet-y-inverting-transformation-mixin
		     +shift-key+
		     shrink-frame
		     simple-parse-error
		     simple-parse-error
		     singular-transformation
		     slider
		     slider-drag-callback
		     slider-pane
		     space-requirement-height
		     space-requirement-height
		     space-requirement-max-height
		     space-requirement-max-height
		     space-requirement-max-width
		     space-requirement-max-width
		     space-requirement-min-height
		     space-requirement-min-height
		     space-requirement-min-width
		     space-requirement-min-width
		     space-requirement-width
		     space-requirement-width
		     spacer-pane
		     spacing
		     *standard-activation-gestures*
		     standard-application-frame
		     standard-bounding-rectangle
		     standard-cell-output-record
		     standard-column-output-record
		     standard-command-table
		     standard-ellipse
		     standard-elliptical-arc
		     standard-encapsulating-stream
		     standard-extended-input-stream
		     standard-extended-output-stream
		     standard-gadget
		     standard-graph-node-output-record
		     standard-graph-output-record
		     standard-input-editing-stream
		     standard-input-stream
		     standard-item-list-output-record
		     standard-line
		     standard-line-style
		     standard-output-recording-stream
		     standard-output-stream
		     standard-point
		     standard-pointer
		     standard-polygon
		     standard-polyline
		     standard-presentation
		     standard-rectangle
		     standard-region-difference
		     standard-region-intersection
		     standard-region-union
		     standard-repainting-mixin
		     standard-row-output-record
		     standard-sequence-output-record
		     standard-sheet-input-mixin
		     standard-sheet-output-mixin
		     standard-table-output-record
		     standard-text-cursor
		     standard-text-style
		     standard-tree-output-history
		     standard-tree-output-record
		     standard-updating-output-record
		     stream-accept
		     stream-add-character-output
		     stream-add-output-record
		     stream-add-string-output
		     stream-advance-to-column
		     stream-advance-to-column
		     stream-baseline
		     stream-character-width
		     stream-clear-input
		     stream-clear-input
		     stream-clear-output
		     stream-clear-output
		     stream-close-text-output-record
		     stream-current-output-record
		     stream-current-output-record
		     stream-cursor-position
		     stream-cursor-position
		     stream-default-view
		     stream-default-view
		     stream-drawing-p
		     stream-drawing-p
		     stream-element-type
		     stream-end-of-line-action
		     stream-end-of-line-action
		     stream-end-of-page-action
		     stream-end-of-page-action
		     stream-finish-output
		     stream-finish-output
		     stream-force-output
		     stream-force-output
		     stream-fresh-line
		     stream-fresh-line
		     stream-increment-cursor-position
		     stream-input-buffer
		     stream-input-buffer
		     stream-input-buffer
		     stream-input-wait
		     stream-insertion-pointer
		     stream-insertion-pointer
		     stream-line-column
		     stream-line-column
		     stream-line-height
		     stream-listen
		     stream-listen
		     stream-output-history
		     stream-output-history-mixin
		     streamp
		     stream-pathname
		     stream-peek-char
		     stream-peek-char
		     stream-pointer-position
		     stream-pointer-position
		     stream-pointers
		     stream-present
		     stream-primary-pointer
		     stream-primary-pointer
		     stream-process-gesture
		     stream-read-byte
		     stream-read-char
		     stream-read-char
		     stream-read-char-no-hang
		     stream-read-char-no-hang
		     stream-read-gesture
		     stream-read-gesture
		     stream-read-line
		     stream-read-line
		     stream-recording-p
		     stream-recording-p
		     stream-redisplayable-p
		     stream-redisplaying-p
		     stream-replay
		     stream-rescanning-p
		     stream-restore-input-focus
		     stream-scan-pointer
		     stream-scan-pointer
		     stream-set-input-focus
		     stream-start-line-p
		     stream-start-line-p
		     stream-string-width
		     stream-terpri
		     stream-terpri
		     stream-text-cursor
		     stream-text-cursor
		     stream-text-margin
		     stream-text-margin
		     stream-text-output-record
		     stream-truename
		     stream-unread-char
		     stream-unread-char
		     stream-unread-gesture
		     stream-unread-gesture
		     stream-vertical-spacing
		     stream-write-byte
		     stream-write-char
		     stream-write-char
		     stream-write-string
		     stream-write-string
		     string
		     subset
		     subset-alist
		     subset-completion
		     subset-sequence
		     substitute-numeric-argument-marker
		     suggest
		     +super-key+
		     surrounding-output-with-border
		     symbol
		     t
		     table-output-record
		     table-output-record-p
		     table-pane
		     tabling
		     temporary-medium-sheet-output-mixin
		     test-presentation-translator
		     text-displayed-output-record
		     text-displayed-output-record-p
		     text-displayed-output-record-string
		     text-editor
		     text-editor-pane
		     text-field
		     text-field-pane
		     text-size
		     text-style
		     text-style-ascent
		     text-style-components
		     text-style-descent
		     text-style-face
		     text-style-family
		     text-style-fixed-width-p
		     text-style-height
		     text-style-mapping
		     text-style-mapping
		     text-style-p
		     text-style-size
		     text-style-width
		     textual-dialog-view
		     +textual-dialog-view+
		     textual-menu-view
		     +textual-menu-view+
		     textual-view
		     +textual-view+
		     throw-highlighted-presentation
		     timer-event
		     title-pane
		     toggle-button
		     toggle-button-indicator-type
		     toggle-button-pane
		     token-or-type
		     tracking-pointer
		     transformation
		     transformation-equal
		     transformation-error
		     transformationp
		     transformation-underspecified
		     transform-distance
		     transform-position
		     transform-rectangle*
		     transform-region
		     translation-transformation-p
		     tree-recompute-extent
		     type-or-string
		     *undefined-text-style*
		     unhighlight-highlighted-presentation
		     unread-gesture
		     *unsupplied-argument-marker*
		     untransform-distance
		     untransform-position
		     untransform-rectangle*
		     untransform-region
		     updating-output
		     updating-output-record
		     updating-output-record-p
		     user-command-table
		     using-resource
		     value-changed-callback
		     value-gadget
		     vbox-pane
		     vertically
		     view
		     viewp
		     vrack-pane
		     window-clear
		     window-configuration-event
		     window-erase-viewport
		     window-event
		     window-event-mirrored-sheet
		     window-event-native-region
		     window-event-region
		     window-refresh
		     window-repaint-event
		     window-viewport
		     window-viewport-position
		     window-viewport-position
		     with-accept-help
		     with-activation-gestures
		     with-application-frame
		     with-bounding-rectangle*
		     with-command-table-keystrokes
		     with-delimiter-gestures
		     with-drawing-options
		     with-end-of-line-action
		     with-end-of-page-action
		     with-first-quadrant-coordinates
		     with-frame-manager
		     with-graft-locked
		     with-input-context
		     with-input-editing
		     with-input-editor-typeout
		     with-input-focus
		     with-local-coordinates
		     with-lock-held
		     with-look-and-feel-realization
		     with-menu
		     with-new-output-record
		     with-output-as-gadget
		     with-output-as-presentation
		     with-output-buffered
		     with-output-recording-options
		     with-output-to-output-record
		     with-output-to-pixmap
		     with-output-to-postscript-stream
		     without-scheduling
		     with-port-locked
		     with-presentation-type-decoded
		     with-presentation-type-options
		     with-presentation-type-parameters
		     with-radio-box
		     with-recursive-lock-held
		     with-room-for-graphics
		     with-rotation
		     with-scaling
		     with-sheet-medium
		     with-sheet-medium-bound
		     with-text-face
		     with-text-family
		     with-text-size
		     with-text-style
		     with-translation
		     write-token

		     ;;; X11 Color names - some are NOT in the spec - mikemac
		     
		     +snow+ +ghost-white+ +GhostWhite+ +white-smoke+ 
		     +WhiteSmoke+ +gainsboro+ +floral-white+ +FloralWhite+ 
		     +old-lace+ +OldLace+ +linen+ +antique-white+ 
		     +AntiqueWhite+ +papaya-whip+ +PapayaWhip+ +blanched-almond+ 
		     +BlanchedAlmond+ +bisque+ +peach-puff+ +PeachPuff+ 
		     +navajo-white+ +NavajoWhite+ +moccasin+ +cornsilk+ 
		     +ivory+ +lemon-chiffon+ +LemonChiffon+ +seashell+ 
		     +honeydew+ +mint-cream+ +MintCream+ +azure+ 
		     +alice-blue+ +AliceBlue+ +lavender+ +lavender-blush+ 
		     +LavenderBlush+ +misty-rose+ +MistyRose+ +white+ 
		     +black+ +dark-slate-gray+ +DarkSlateGray+ +dark-slate-grey+ 
		     +DarkSlateGrey+ +dim-gray+ +DimGray+ +dim-grey+ 
		     +DimGrey+ +slate-gray+ +SlateGray+ +slate-grey+ 
		     +SlateGrey+ +light-slate-gray+ +LightSlateGray+ +light-slate-grey+ 
		     +LightSlateGrey+ +gray+ +grey+ +light-grey+ 
		     +LightGrey+ +light-gray+ +LightGray+ +midnight-blue+ 
		     +MidnightBlue+ +navy+ +navy-blue+ +NavyBlue+ 
		     +cornflower-blue+ +CornflowerBlue+ +dark-slate-blue+ +DarkSlateBlue+ 
		     +slate-blue+ +SlateBlue+ +medium-slate-blue+ +MediumSlateBlue+ 
		     +light-slate-blue+ +LightSlateBlue+ +medium-blue+ +MediumBlue+ 
		     +royal-blue+ +RoyalBlue+ +blue+ +dodger-blue+ 
		     +DodgerBlue+ +deep-sky-blue+ +DeepSkyBlue+ +sky-blue+ 
		     +SkyBlue+ +light-sky-blue+ +LightSkyBlue+ +steel-blue+ 
		     +SteelBlue+ +light-steel-blue+ +LightSteelBlue+ +light-blue+ 
		     +LightBlue+ +powder-blue+ +PowderBlue+ +pale-turquoise+ 
		     +PaleTurquoise+ +dark-turquoise+ +DarkTurquoise+ +medium-turquoise+ 
		     +MediumTurquoise+ +turquoise+ +cyan+ +light-cyan+ 
		     +LightCyan+ +cadet-blue+ +CadetBlue+ +medium-aquamarine+ 
		     +MediumAquamarine+ +aquamarine+ +dark-green+ +DarkGreen+ 
		     +dark-olive-green+ +DarkOliveGreen+ +dark-sea-green+ +DarkSeaGreen+ 
		     +sea-green+ +SeaGreen+ +medium-sea-green+ +MediumSeaGreen+ 
		     +light-sea-green+ +LightSeaGreen+ +pale-green+ +PaleGreen+ 
		     +spring-green+ +SpringGreen+ +lawn-green+ +LawnGreen+ 
		     +green+ +chartreuse+ +medium-spring-green+ +MediumSpringGreen+ 
		     +green-yellow+ +GreenYellow+ +lime-green+ +LimeGreen+ 
		     +yellow-green+ +YellowGreen+ +forest-green+ +ForestGreen+ 
		     +olive-drab+ +OliveDrab+ +dark-khaki+ +DarkKhaki+ 
		     +khaki+ +pale-goldenrod+ +PaleGoldenrod+ +light-goldenrod-yellow+ 
		     +LightGoldenrodYellow+ +light-yellow+ +LightYellow+ +yellow+ 
		     +gold+ +light-goldenrod+ +LightGoldenrod+ +goldenrod+ 
		     +dark-goldenrod+ +DarkGoldenrod+ +rosy-brown+ +RosyBrown+ 
		     +indian-red+ +IndianRed+ +saddle-brown+ +SaddleBrown+ 
		     +sienna+ +peru+ +burlywood+ +beige+ 
		     +wheat+ +sandy-brown+ +SandyBrown+ +tan+ 
		     +chocolate+ +firebrick+ +brown+ +dark-salmon+ 
		     +DarkSalmon+ +salmon+ +light-salmon+ +LightSalmon+ 
		     +orange+ +dark-orange+ +DarkOrange+ +coral+ 
		     +light-coral+ +LightCoral+ +tomato+ +orange-red+ 
		     +OrangeRed+ +red+ +hot-pink+ +HotPink+ 
		     +deep-pink+ +DeepPink+ +pink+ +light-pink+ 
		     +LightPink+ +pale-violet-red+ +PaleVioletRed+ +maroon+ 
		     +medium-violet-red+ +MediumVioletRed+ +violet-red+ +VioletRed+ 
		     +magenta+ +violet+ +plum+ +orchid+ 
		     +medium-orchid+ +MediumOrchid+ +dark-orchid+ +DarkOrchid+ 
		     +dark-violet+ +DarkViolet+ +blue-violet+ +BlueViolet+ 
		     +purple+ +medium-purple+ +MediumPurple+ +thistle+ 
		     +snow1+ +snow2+ +snow3+ +snow4+ 
		     +seashell1+ +seashell2+ +seashell3+ +seashell4+ 
		     +AntiqueWhite1+ +AntiqueWhite2+ +AntiqueWhite3+ +AntiqueWhite4+ 
		     +bisque1+ +bisque2+ +bisque3+ +bisque4+ 
		     +PeachPuff1+ +PeachPuff2+ +PeachPuff3+ +PeachPuff4+ 
		     +NavajoWhite1+ +NavajoWhite2+ +NavajoWhite3+ +NavajoWhite4+ 
		     +LemonChiffon1+ +LemonChiffon2+ +LemonChiffon3+ +LemonChiffon4+ 
		     +cornsilk1+ +cornsilk2+ +cornsilk3+ +cornsilk4+ 
		     +ivory1+ +ivory2+ +ivory3+ +ivory4+ 
		     +honeydew1+ +honeydew2+ +honeydew3+ +honeydew4+ 
		     +LavenderBlush1+ +LavenderBlush2+ +LavenderBlush3+ +LavenderBlush4+ 
		     +MistyRose1+ +MistyRose2+ +MistyRose3+ +MistyRose4+ 
		     +azure1+ +azure2+ +azure3+ +azure4+ 
		     +SlateBlue1+ +SlateBlue2+ +SlateBlue3+ +SlateBlue4+ 
		     +RoyalBlue1+ +RoyalBlue2+ +RoyalBlue3+ +RoyalBlue4+ 
		     +blue1+ +blue2+ +blue3+ +blue4+ 
		     +DodgerBlue1+ +DodgerBlue2+ +DodgerBlue3+ +DodgerBlue4+ 
		     +SteelBlue1+ +SteelBlue2+ +SteelBlue3+ +SteelBlue4+ 
		     +DeepSkyBlue1+ +DeepSkyBlue2+ +DeepSkyBlue3+ +DeepSkyBlue4+ 
		     +SkyBlue1+ +SkyBlue2+ +SkyBlue3+ +SkyBlue4+ 
		     +LightSkyBlue1+ +LightSkyBlue2+ +LightSkyBlue3+ +LightSkyBlue4+ 
		     +SlateGray1+ +SlateGray2+ +SlateGray3+ +SlateGray4+ 
		     +LightSteelBlue1+ +LightSteelBlue2+ +LightSteelBlue3+ +LightSteelBlue4+ 
		     +LightBlue1+ +LightBlue2+ +LightBlue3+ +LightBlue4+ 
		     +LightCyan1+ +LightCyan2+ +LightCyan3+ +LightCyan4+ 
		     +PaleTurquoise1+ +PaleTurquoise2+ +PaleTurquoise3+ +PaleTurquoise4+ 
		     +CadetBlue1+ +CadetBlue2+ +CadetBlue3+ +CadetBlue4+ 
		     +turquoise1+ +turquoise2+ +turquoise3+ +turquoise4+ 
		     +cyan1+ +cyan2+ +cyan3+ +cyan4+ 
		     +DarkSlateGray1+ +DarkSlateGray2+ +DarkSlateGray3+ +DarkSlateGray4+ 
		     +aquamarine1+ +aquamarine2+ +aquamarine3+ +aquamarine4+ 
		     +DarkSeaGreen1+ +DarkSeaGreen2+ +DarkSeaGreen3+ +DarkSeaGreen4+ 
		     +SeaGreen1+ +SeaGreen2+ +SeaGreen3+ +SeaGreen4+ 
		     +PaleGreen1+ +PaleGreen2+ +PaleGreen3+ +PaleGreen4+ 
		     +SpringGreen1+ +SpringGreen2+ +SpringGreen3+ +SpringGreen4+ 
		     +green1+ +green2+ +green3+ +green4+ 
		     +chartreuse1+ +chartreuse2+ +chartreuse3+ +chartreuse4+ 
		     +OliveDrab1+ +OliveDrab2+ +OliveDrab3+ +OliveDrab4+ 
		     +DarkOliveGreen1+ +DarkOliveGreen2+ +DarkOliveGreen3+ +DarkOliveGreen4+ 
		     +khaki1+ +khaki2+ +khaki3+ +khaki4+ 
		     +LightGoldenrod1+ +LightGoldenrod2+ +LightGoldenrod3+ +LightGoldenrod4+ 
		     +LightYellow1+ +LightYellow2+ +LightYellow3+ +LightYellow4+ 
		     +yellow1+ +yellow2+ +yellow3+ +yellow4+ 
		     +gold1+ +gold2+ +gold3+ +gold4+ 
		     +goldenrod1+ +goldenrod2+ +goldenrod3+ +goldenrod4+ 
		     +DarkGoldenrod1+ +DarkGoldenrod2+ +DarkGoldenrod3+ +DarkGoldenrod4+ 
		     +RosyBrown1+ +RosyBrown2+ +RosyBrown3+ +RosyBrown4+ 
		     +IndianRed1+ +IndianRed2+ +IndianRed3+ +IndianRed4+ 
		     +sienna1+ +sienna2+ +sienna3+ +sienna4+ 
		     +burlywood1+ +burlywood2+ +burlywood3+ +burlywood4+ 
		     +wheat1+ +wheat2+ +wheat3+ +wheat4+ 
		     +tan1+ +tan2+ +tan3+ +tan4+ 
		     +chocolate1+ +chocolate2+ +chocolate3+ +chocolate4+ 
		     +firebrick1+ +firebrick2+ +firebrick3+ +firebrick4+ 
		     +brown1+ +brown2+ +brown3+ +brown4+ 
		     +salmon1+ +salmon2+ +salmon3+ +salmon4+ 
		     +LightSalmon1+ +LightSalmon2+ +LightSalmon3+ +LightSalmon4+ 
		     +orange1+ +orange2+ +orange3+ +orange4+ 
		     +DarkOrange1+ +DarkOrange2+ +DarkOrange3+ +DarkOrange4+ 
		     +coral1+ +coral2+ +coral3+ +coral4+ 
		     +tomato1+ +tomato2+ +tomato3+ +tomato4+ 
		     +OrangeRed1+ +OrangeRed2+ +OrangeRed3+ +OrangeRed4+ 
		     +red1+ +red2+ +red3+ +red4+ 
		     +DeepPink1+ +DeepPink2+ +DeepPink3+ +DeepPink4+ 
		     +HotPink1+ +HotPink2+ +HotPink3+ +HotPink4+ 
		     +pink1+ +pink2+ +pink3+ +pink4+ 
		     +LightPink1+ +LightPink2+ +LightPink3+ +LightPink4+ 
		     +PaleVioletRed1+ +PaleVioletRed2+ +PaleVioletRed3+ +PaleVioletRed4+ 
		     +maroon1+ +maroon2+ +maroon3+ +maroon4+ 
		     +VioletRed1+ +VioletRed2+ +VioletRed3+ +VioletRed4+ 
		     +magenta1+ +magenta2+ +magenta3+ +magenta4+ 
		     +orchid1+ +orchid2+ +orchid3+ +orchid4+ 
		     +plum1+ +plum2+ +plum3+ +plum4+ 
		     +MediumOrchid1+ +MediumOrchid2+ +MediumOrchid3+ +MediumOrchid4+ 
		     +DarkOrchid1+ +DarkOrchid2+ +DarkOrchid3+ +DarkOrchid4+ 
		     +purple1+ +purple2+ +purple3+ +purple4+ 
		     +MediumPurple1+ +MediumPurple2+ +MediumPurple3+ +MediumPurple4+ 
		     +thistle1+ +thistle2+ +thistle3+ +thistle4+ 
		     +gray0+ +grey0+ +gray1+ +grey1+ 
		     +gray2+ +grey2+ +gray3+ +grey3+ 
		     +gray4+ +grey4+ +gray5+ +grey5+ 
		     +gray6+ +grey6+ +gray7+ +grey7+ 
		     +gray8+ +grey8+ +gray9+ +grey9+ 
		     +gray10+ +grey10+ +gray11+ +grey11+ 
		     +gray12+ +grey12+ +gray13+ +grey13+ 
		     +gray14+ +grey14+ +gray15+ +grey15+ 
		     +gray16+ +grey16+ +gray17+ +grey17+ 
		     +gray18+ +grey18+ +gray19+ +grey19+ 
		     +gray20+ +grey20+ +gray21+ +grey21+ 
		     +gray22+ +grey22+ +gray23+ +grey23+ 
		     +gray24+ +grey24+ +gray25+ +grey25+ 
		     +gray26+ +grey26+ +gray27+ +grey27+ 
		     +gray28+ +grey28+ +gray29+ +grey29+ 
		     +gray30+ +grey30+ +gray31+ +grey31+ 
		     +gray32+ +grey32+ +gray33+ +grey33+ 
		     +gray34+ +grey34+ +gray35+ +grey35+ 
		     +gray36+ +grey36+ +gray37+ +grey37+ 
		     +gray38+ +grey38+ +gray39+ +grey39+ 
		     +gray40+ +grey40+ +gray41+ +grey41+ 
		     +gray42+ +grey42+ +gray43+ +grey43+ 
		     +gray44+ +grey44+ +gray45+ +grey45+ 
		     +gray46+ +grey46+ +gray47+ +grey47+ 
		     +gray48+ +grey48+ +gray49+ +grey49+ 
		     +gray50+ +grey50+ +gray51+ +grey51+ 
		     +gray52+ +grey52+ +gray53+ +grey53+ 
		     +gray54+ +grey54+ +gray55+ +grey55+ 
		     +gray56+ +grey56+ +gray57+ +grey57+ 
		     +gray58+ +grey58+ +gray59+ +grey59+ 
		     +gray60+ +grey60+ +gray61+ +grey61+ 
		     +gray62+ +grey62+ +gray63+ +grey63+ 
		     +gray64+ +grey64+ +gray65+ +grey65+ 
		     +gray66+ +grey66+ +gray67+ +grey67+ 
		     +gray68+ +grey68+ +gray69+ +grey69+ 
		     +gray70+ +grey70+ +gray71+ +grey71+ 
		     +gray72+ +grey72+ +gray73+ +grey73+ 
		     +gray74+ +grey74+ +gray75+ +grey75+ 
		     +gray76+ +grey76+ +gray77+ +grey77+ 
		     +gray78+ +grey78+ +gray79+ +grey79+ 
		     +gray80+ +grey80+ +gray81+ +grey81+ 
		     +gray82+ +grey82+ +gray83+ +grey83+ 
		     +gray84+ +grey84+ +gray85+ +grey85+ 
		     +gray86+ +grey86+ +gray87+ +grey87+ 
		     +gray88+ +grey88+ +gray89+ +grey89+ 
		     +gray90+ +grey90+ +gray91+ +grey91+ 
		     +gray92+ +grey92+ +gray93+ +grey93+ 
		     +gray94+ +grey94+ +gray95+ +grey95+ 
		     +gray96+ +grey96+ +gray97+ +grey97+ 
		     +gray98+ +grey98+ +gray99+ +grey99+ 
		     +gray100+ +grey100+ +dark-grey+ +DarkGrey+ 
		     +dark-gray+ +DarkGray+ +dark-blue+ +DarkBlue+ 
		     +dark-cyan+ +DarkCyan+ +dark-magenta+ +DarkMagenta+ 
		     +dark-red+ +DarkRed+ +light-green+ +LightGreen+ 
            
		     ))
	(extensions '(raised-pane raising)))
    (export externals)
    (export externals :clim)
    (export extensions)
    (export extensions :clim-extensions)))
