;; climbols.el: Looks up symbols in the CLIM spec from inside xemacs.
;; Written and tested using XEmacs.
;; This code is in the public domain.

;; Orginally written by Andy Hefner (andy.hefner@verizon.net)



(require 'cl)
(require 'browse-url)
(require 'thingatpt)


(defvar clim-sybmol-table nil
  "The symbol table for looking up CLIM symbols")

;; This was written to use MikeMac's version of the CLIM docs
(defvar clim-base "http://www.mikemac.com/mikemac/clim/")

(defvar clim-history nil)



;; Look up a symbol in MikeMac's CLIM documentation.
;; By default it looks up the symbol under the point, but if it isn't over
;; something resembling a symbol, it will prompt you. 
;; Also, you can use a prefix arg to force prompting.
(defun clim-lookup (p)
  (interactive "p")  
  (let ((symbol-name (thing-at-point 'symbol)))
    (unless (and (= 1 p) (stringp symbol-name))
      (setq symbol-name (read-from-minibuffer "Symbol name: " "" nil nil 'clim-history)))
    (let ((a (assoc (downcase symbol-name) clim-symbol-table)))
      (if a
	  (browse-url (concat clim-base (cdr a)))
	(message "Symbol %s not found." symbol-name)))))


(setq clim-symbol-table 
 '(("abort-gesture" . "extended-input.html#Condition abort-gesture")
   ("abort-gesture-event"
    . "extended-input.html#Generic function abort-gesture-event")
   ("*abort-gestures*" . "extended-input.html#Variable *abort-gestures*")
   ("accelerator-gesture" . "extended-input.html#Condition accelerator-gesture")
   ("accelerator-gesture-event"
   . "extended-input.html#Generic function accelerator-gesture-event")
   ("accelerator-gesture-numeric-argument"
   . "extended-input.html#Generic function accelerator-gesture-numeric-argument")
   ("*accelerator-gestures*"
   . "extended-input.html#Variable *accelerator-gestures*")
   ("accept-1" . "presentation-types.html#Function accept-1")
   ("accept" . "presentation-types.html#Function accept")
   ("accept" . "presentation-types.html#Presentation Method accept")
   ("accept-from-string" . "presentation-types.html#Function accept-from-string")
   ("accepting-values" . "dialogs.html#Macro accepting-values")
   ("accept-present-default"
   . "presentation-types.html#Presentation Method accept-present-default")
   ("accept-values" . "dialogs.html#Frame accept-values")
   ("accept-values-command-button"
   . "dialogs.html#Macro accept-values-command-button")
   ("accept-values-resynchronize"
   . "dialogs.html#Generic function accept-values-resynchronize")
   ("action-gadget" . "gadgets.html#Class action-gadget")
   ("activate-callback" . "gadgets.html#Callback activate-callback")
   (":activate-callback" . "gadgets.html#Init&nbsp;arg :activate-callback")
   ("activate-gadget" . "gadgets.html#Generic function activate-gadget")
   ("activation-gesture-p" . "input-editing.html#Function activation-gesture-p")
   ("*activation-gestures*"
   . "input-editing.html#Variable *activation-gestures*")
   ("add-character-output-to-text-record"
   . "output-recording.html#Generic function add-character-output-to-text-record")
   ("add-command-to-command-table"
   . "commands.html#Function add-command-to-command-table")
   ("add-gesture-name" . "extended-input.html#Function add-gesture-name")
   ("add-input-editor-command"
   . "input-editing.html#Function add-input-editor-command")
   ("add-keystroke-to-command-table"
   . "commands.html#Function add-keystroke-to-command-table")
   ("add-menu-item-to-command-table"
   . "commands.html#Function add-menu-item-to-command-table")
   ("add-output-record"
   . "output-recording.html#Generic function add-output-record")
   ("add-presentation-translator-to-command-table"
   . "commands.html#Function add-presentation-translator-to-command-table")
   ("add-string-output-to-text-record"
   . "output-recording.html#Generic function add-string-output-to-text-record")
   ("adjust-item-list-cells"
   . "table-formatting.html#Generic function adjust-item-list-cells")
   ("adjust-multiple-columns"
   . "table-formatting.html#Generic function adjust-multiple-columns")
   ("adjust-table-cells"
   . "table-formatting.html#Generic function adjust-table-cells")
   ("adopt-frame" . "frames.html#Generic function adopt-frame")
   (":align-x" . "gadgets.html#Init&nbsp;arg :align-x")
   (":align-x" . "panes.html#Option :align-x")
   (":align-x" . "table-formatting.html#Init&nbsp;arg :align-x")
   (":align-y" . "gadgets.html#Init&nbsp;arg :align-y")
   (":align-y" . "panes.html#Option :align-y")
   (":align-y" . "table-formatting.html#Init&nbsp;arg :align-y")
   ("allocate-medium" . "silica.html#Generic function allocate-medium")
   ("allocate-pixmap" . "graphics.html#Generic function allocate-pixmap")
   ("allocate-resource" . "clim-sys.html#Function allocate-resource")
   ("allocate-space" . "panes.html#Generic function allocate-space")
   ("all-processes" . "clim-sys.html#Function all-processes")
   ("and" . "presentation-types.html#Presentation Type and")
   ("application-frame" . "frames.html#Protocol&nbsp;Class application-frame")
   ("*application-frame*" . "frames.html#Variable *application-frame*")
   ("application-frame-p" . "frames.html#Predicate application-frame-p")
   ("application-pane" . "panes.html#Service Pane application-pane")
   ("apply-presentation-generic-function"
   . "presentation-types.html#Macro apply-presentation-generic-function")
   ("area" . "regions.html#Protocol&nbsp;Class area")
   ("areap" . "regions.html#Predicate areap")
   ("armed-callback" . "gadgets.html#Callback armed-callback")
   (":armed-callback" . "gadgets.html#Init&nbsp;arg :armed-callback")
   ("atomic-decf" . "clim-sys.html#Function atomic-decf")
   ("atomic-incf" . "clim-sys.html#Function atomic-incf")
   ("augment-draw-set" . "redisplay.html#Generic function augment-draw-set")
   (":background" . "extended-output.html#Init&nbsp;arg :background")
   (":background" . "panes.html#Option :background")
   ("+background-ink+" . "colors.html#Constant +background-ink+")
   ("basic-gadget" . "gadgets.html#Class basic-gadget")
   ("basic-medium" . "silica.html#Class basic-medium")
   ("basic-pane" . "panes.html#Class basic-pane")
   ("basic-port" . "silica.html#Class basic-port")
   ("basic-sheet" . "silica.html#Class basic-sheet")
   ("bboard-pane" . "panes.html#Layout Pane bboard-pane")
   ("beep" . "extended-output.html#Generic function beep")
   ("+black+" . "colors.html#Constant +black+")
   ("blank-area" . "presentation-types.html#Presentation Type blank-area")
   ("+blue+" . "colors.html#Constant +blue+")
   ("boolean" . "presentation-types.html#Presentation Type boolean")
   ("bounding-rectangle" . "bboxes.html#Generic function bounding-rectangle")
   ("bounding-rectangle*" . "bboxes.html#Generic function bounding-rectangle*")
   ("bounding-rectangle" . "bboxes.html#Protocol&nbsp;Class bounding-rectangle")
   ("bounding-rectangle-height"
   . "bboxes.html#Generic function bounding-rectangle-height")
   ("bounding-rectangle-max-x"
   . "bboxes.html#Generic function bounding-rectangle-max-x")
   ("bounding-rectangle-max-y"
   . "bboxes.html#Generic function bounding-rectangle-max-y")
   ("bounding-rectangle-min-x"
   . "bboxes.html#Generic function bounding-rectangle-min-x")
   ("bounding-rectangle-min-y"
   . "bboxes.html#Generic function bounding-rectangle-min-y")
   ("bounding-rectangle-p" . "bboxes.html#Predicate bounding-rectangle-p")
   ("bounding-rectangle-position"
   . "bboxes.html#Generic function bounding-rectangle-position")
   ("bounding-rectangle-size"
   . "bboxes.html#Generic function bounding-rectangle-size")
   ("bounding-rectangle-width"
   . "bboxes.html#Generic function bounding-rectangle-width")
   ("bury-frame" . "frames.html#Generic function bury-frame")
   ("bury-mirror" . "silica.html#Generic function bury-mirror")
   ("bury-sheet" . "silica.html#Generic function bury-sheet")
   (":button" . "silica.html#Init&nbsp;arg :button")
   ("cache-output-record"
   . "redisplay.html#Generic function cache-output-record")
   (":cache-test" . "redisplay.html#Init&nbsp;arg :cache-test")
   (":cache-value" . "redisplay.html#Init&nbsp;arg :cache-value")
   (":calling-frame" . "frames.html#Init&nbsp;arg :calling-frame")
   ("call-presentation-menu"
   . "presentation-types.html#Function call-presentation-menu")
   ("call-presentation-translator"
   . "presentation-types.html#Function call-presentation-translator")
   ("cell-align-x" . "table-formatting.html#Generic function cell-align-x")
   ("cell-align-y" . "table-formatting.html#Generic function cell-align-y")
   ("cell-min-height" . "table-formatting.html#Generic function cell-min-height")
   ("cell-min-width" . "table-formatting.html#Generic function cell-min-width")
   ("cell-output-record"
   . "table-formatting.html#Protocol&nbsp;Class cell-output-record")
   ("cell-output-record-p"
   . "table-formatting.html#Predicate cell-output-record-p")
   (":center-nodes" . "graph-formatting.html#Init&nbsp;arg :center-nodes")
   ("change-space-requirements"
   . "panes.html#Generic function change-space-requirements")
   ("changing-space-requirements"
   . "panes.html#Macro changing-space-requirements")
   ("character" . "presentation-types.html#Presentation Type character")
   ("check-box" . "gadgets.html#Class check-box")
   ("check-box-current-selection"
   . "gadgets.html#Generic function check-box-current-selection")
   ("check-box-pane" . "gadgets.html#Class check-box-pane")
   ("check-box-selections"
   . "gadgets.html#Generic function check-box-selections")
   ("child-containing-position"
   . "silica.html#Generic function child-containing-position")
   ("children-overlapping-rectangle*"
   . "silica.html#Generic function children-overlapping-rectangle*")
   ("children-overlapping-region"
   . "silica.html#Generic function children-overlapping-region")
   ("class-presentation-type-name"
   . "presentation-types.html#Function class-presentation-type-name")
   ("clear-output-record"
   . "output-recording.html#Generic function clear-output-record")
   ("clear-resource" . "clim-sys.html#Function clear-resource")
   (":client" . "gadgets.html#Init&nbsp;arg :client")
   ("clim-stream-pane" . "panes.html#Service Pane clim-stream-pane")
   (":clipping-region" . "drawing-options.html#Option :clipping-region")
   ("close" . "gray-streams.html#Generic function close")
   ("color" . "colors.html#Protocol&nbsp;Class color")
   ("color-ihs" . "colors.html#Generic function color-ihs")
   ("colorp" . "colors.html#Predicate colorp")
   ("color-rgb" . "colors.html#Generic function color-rgb")
   ("column-output-record"
   . "table-formatting.html#Protocol&nbsp;Class column-output-record")
   ("column-output-record-p"
   . "table-formatting.html#Predicate column-output-record-p")
   ("command" . "commands.html#Presentation Type command")
   ("command-accessible-in-command-table-p"
   . "commands.html#Function command-accessible-in-command-table-p")
   ("command-already-present" . "commands.html#Error command-already-present")
   ("*command-argument-delimiters*"
   . "commands.html#Variable *command-argument-delimiters*")
   ("command-arguments" . "commands.html#Function command-arguments")
   ("*command-dispatchers*" . "commands.html#Variable *command-dispatchers*")
   ("command-enabled" . "frames.html#Generic function command-enabled")
   ("command-line-command-parser"
   . "commands.html#Function command-line-command-parser")
   ("command-line-command-unparser"
   . "commands.html#Function command-line-command-unparser")
   ("command-line-name-for-command"
   . "commands.html#Function command-line-name-for-command")
   ("command-line-read-remaining-arguments-for-partial-command"
   . "commands.html#Function command-line-read-remaining-arguments-for-partial-command")
   ("command-menu-item-options"
   . "commands.html#Function command-menu-item-options")
   ("command-menu-item-type" . "commands.html#Function command-menu-item-type")
   ("command-menu-item-value" . "commands.html#Function command-menu-item-value")
   ("command-menu-pane" . "panes.html#Service Pane command-menu-pane")
   ("command-name" . "commands.html#Function command-name")
   ("command-name" . "commands.html#Presentation Type command-name")
   ("*command-name-delimiters*"
   . "commands.html#Variable *command-name-delimiters*")
   ("command-name-from-symbol"
   . "commands.html#Function command-name-from-symbol")
   ("command-not-accessible" . "commands.html#Error command-not-accessible")
   ("command-not-present" . "commands.html#Error command-not-present")
   ("command-or-form" . "commands.html#Presentation Type command-or-form")
   ("*command-parser*" . "commands.html#Variable *command-parser*")
   ("command-present-in-command-table-p"
   . "commands.html#Function command-present-in-command-table-p")
   ("command-table" . "commands.html#Protocol&nbsp;Class command-table")
   (":command-table" . "frames.html#Init&nbsp;arg :command-table")
   ("command-table-already-exists"
   . "commands.html#Error command-table-already-exists")
   ("command-table-complete-input"
   . "commands.html#Function command-table-complete-input")
   ("command-table-error" . "commands.html#Error command-table-error")
   ("command-table-inherit-from"
   . "commands.html#Generic function command-table-inherit-from")
   ("command-table-name" . "commands.html#Generic function command-table-name")
   ("command-table-not-found" . "commands.html#Error command-table-not-found")
   ("command-table-p" . "commands.html#Predicate command-table-p")
   ("*command-unparser*" . "commands.html#Variable *command-unparser*")
   ("complete-from-generator"
   . "input-editing.html#Function complete-from-generator")
   ("complete-from-possibilities"
   . "input-editing.html#Function complete-from-possibilities")
   ("complete-input" . "input-editing.html#Function complete-input")
   ("completing-from-suggestions"
   . "input-editing.html#Macro completing-from-suggestions")
   ("completion" . "presentation-types.html#Presentation Type completion")
   ("*completion-gestures*"
   . "input-editing.html#Variable *completion-gestures*")
   ("complex" . "presentation-types.html#Presentation Type complex")
   ("compose-in" . "designs.html#Generic function compose-in")
   ("compose-out" . "designs.html#Generic function compose-out")
   ("compose-over" . "designs.html#Generic function compose-over")
   ("compose-rotation-with-transformation"
   . "transforms.html#Function compose-rotation-with-transformation")
   ("compose-scaling-with-transformation"
   . "transforms.html#Function compose-scaling-with-transformation")
   ("compose-space" . "panes.html#Generic function compose-space")
   ("compose-transformations"
   . "transforms.html#Generic function compose-transformations")
   ("compose-transformation-with-rotation"
   . "transforms.html#Function compose-transformation-with-rotation")
   ("compose-transformation-with-scaling"
   . "transforms.html#Function compose-transformation-with-scaling")
   ("compose-transformation-with-translation"
   . "transforms.html#Function compose-transformation-with-translation")
   ("compose-translation-with-transformation"
   . "transforms.html#Function compose-translation-with-transformation")
   ("compute-difference-set"
   . "redisplay.html#Generic function compute-difference-set")
   ("compute-new-output-records"
   . "redisplay.html#Generic function compute-new-output-records")
   (":contents" . "panes.html#Option :contents")
   ("contrasting-dash-pattern-limit"
   . "drawing-options.html#Generic function contrasting-dash-pattern-limit")
   ("contrasting-inks-limit"
   . "colors.html#Generic function contrasting-inks-limit")
   ("+control-key+" . "silica.html#Constant +control-key+")
   ("coordinate" . "regions.html#Function coordinate")
   ("coordinate" . "regions.html#Type coordinate")
   ("copy-area" . "graphics.html#Generic function copy-area")
   ("copy-from-pixmap" . "graphics.html#Function copy-from-pixmap")
   ("copy-textual-output-history"
   . "output-recording.html#Function copy-textual-output-history")
   ("copy-to-pixmap" . "graphics.html#Function copy-to-pixmap")
   ("current-process" . "clim-sys.html#Function current-process")
   (":current-selection" . "gadgets.html#Init&nbsp;arg :current-selection")
   (":current-selection" . "gadgets.html#Init&nbsp;arg :current-selection")
   ("cursor" . "extended-output.html#Protocol&nbsp;Class cursor")
   ("cursor-active" . "extended-output.html#Generic function cursor-active")
   ("cursor-focus" . "extended-output.html#Generic function cursor-focus")
   ("cursorp" . "extended-output.html#Predicate cursorp")
   ("cursor-position" . "extended-output.html#Generic function cursor-position")
   ("cursor-sheet" . "extended-output.html#Generic function cursor-sheet")
   ("cursor-state" . "extended-output.html#Generic function cursor-state")
   ("cursor-visibility"
   . "extended-output.html#Generic function cursor-visibility")
   (":cutoff-depth" . "graph-formatting.html#Init&nbsp;arg :cutoff-depth")
   ("+cyan+" . "colors.html#Constant +cyan+")
   ("deactivate-gadget" . "gadgets.html#Generic function deactivate-gadget")
   ("deallocate-medium" . "silica.html#Generic function deallocate-medium")
   ("deallocate-pixmap" . "graphics.html#Generic function deallocate-pixmap")
   ("deallocate-resource" . "clim-sys.html#Function deallocate-resource")
   ("decache-child-output-record"
   . "redisplay.html#Generic function decache-child-output-record")
   (":decimal-places" . "gadgets.html#Init&nbsp;arg :decimal-places")
   ("default-describe-presentation-type"
   . "presentation-types.html#Function default-describe-presentation-type")
   ("*default-frame-manager*" . "frames.html#Variable *default-frame-manager*")
   ("default-frame-top-level"
   . "frames.html#Generic function default-frame-top-level")
   ("*default-server-path*" . "silica.html#Variable *default-server-path*")
   ("*default-text-style*" . "text-styles.html#Constant *default-text-style*")
   (":default-view" . "extended-output.html#Init&nbsp;arg :default-view")
   ("defgeneric*" . "clim-sys.html#Macro defgeneric*")
   ("define-application-frame" . "frames.html#Macro define-application-frame")
   ("define-border-type" . "bordered-output.html#Macro define-border-type")
   ("define-command" . "commands.html#Macro define-command")
   ("define-command-table" . "commands.html#Macro define-command-table")
   ("define-default-presentation-method"
   . "presentation-types.html#Macro define-default-presentation-method")
   ("define-drag-and-drop-translator"
   . "presentation-types.html#Macro define-drag-and-drop-translator")
   ("define-gesture-name" . "extended-input.html#Macro define-gesture-name")
   ("define-graph-type" . "graph-formatting.html#Macro define-graph-type")
   ("define-presentation-action"
   . "presentation-types.html#Macro define-presentation-action")
   ("define-presentation-generic-function"
   . "presentation-types.html#Macro define-presentation-generic-function")
   ("define-presentation-method"
   . "presentation-types.html#Macro define-presentation-method")
   ("define-presentation-to-command-translator"
   . "presentation-types.html#Macro define-presentation-to-command-translator")
   ("define-presentation-translator"
   . "presentation-types.html#Macro define-presentation-translator")
   ("define-presentation-type"
   . "presentation-types.html#Macro define-presentation-type")
   ("define-presentation-type-abbreviation"
   . "presentation-types.html#Macro define-presentation-type-abbreviation")
   ("defmethod*" . "clim-sys.html#Macro defmethod*")
   ("defresource" . "clim-sys.html#Macro defresource")
   ("degraft-medium" . "silica.html#Generic function degraft-medium")
   ("delegate-sheet-delegate"
   . "silica.html#Generic function delegate-sheet-delegate")
   ("delegate-sheet-input-mixin"
   . "silica.html#Class delegate-sheet-input-mixin")
   ("delete-gesture-name" . "extended-input.html#Function delete-gesture-name")
   ("delete-output-record"
   . "output-recording.html#Generic function delete-output-record")
   ("delimiter-gesture-p" . "input-editing.html#Function delimiter-gesture-p")
   ("*delimiter-gestures*" . "input-editing.html#Variable *delimiter-gestures*")
   ("describe-presentation-type"
   . "presentation-types.html#Function describe-presentation-type")
   ("describe-presentation-type"
   . "presentation-types.html#Presentation Method describe-presentation-type")
   ("design" . "colors.html#Protocol&nbsp;Class design")
   ("designp" . "colors.html#Predicate designp")
   ("destroy-frame" . "frames.html#Generic function destroy-frame")
   ("destroy-mirror" . "silica.html#Generic function destroy-mirror")
   ("destroy-port" . "silica.html#Generic function destroy-port")
   ("destroy-process" . "clim-sys.html#Function destroy-process")
   ("device-event" . "silica.html#Class device-event")
   (":disabled-commands" . "frames.html#Init&nbsp;arg :disabled-commands")
   ("disable-frame" . "frames.html#Generic function disable-frame")
   ("disable-process" . "clim-sys.html#Function disable-process")
   ("disarmed-callback" . "gadgets.html#Callback disarmed-callback")
   (":disarmed-callback" . "gadgets.html#Init&nbsp;arg :disarmed-callback")
   ("disown-frame" . "frames.html#Generic function disown-frame")
   ("dispatch-event" . "silica.html#Generic function dispatch-event")
   ("display-command-menu" . "frames.html#Generic function display-command-menu")
   ("display-command-table-menu"
   . "commands.html#Generic function display-command-table-menu")
   ("displayed-output-record"
   . "output-recording.html#Protocol&nbsp;Class displayed-output-record")
   ("displayed-output-record-ink"
   . "output-recording.html#Generic function displayed-output-record-ink")
   ("displayed-output-record-p"
   . "output-recording.html#Predicate displayed-output-record-p")
   ("display-exit-boxes" . "dialogs.html#Generic function display-exit-boxes")
   (":display-function" . "panes.html#Option :display-function")
   (":display-time" . "panes.html#Option :display-time")
   ("distribute-event" . "silica.html#Generic function distribute-event")
   ("do-command-table-inheritance"
   . "commands.html#Macro do-command-table-inheritance")
   ("document-presentation-translator"
   . "presentation-types.html#Function document-presentation-translator")
   ("drag-callback" . "gadgets.html#Callback drag-callback")
   ("drag-callback" . "gadgets.html#Callback drag-callback")
   (":drag-callback" . "gadgets.html#Init&nbsp;arg :drag-callback")
   (":drag-callback" . "gadgets.html#Init&nbsp;arg :drag-callback")
   ("dragging-output" . "extended-input.html#Macro dragging-output")
   ("drag-output-record"
   . "extended-input.html#Generic function drag-output-record")
   (":draw" . "panes.html#Option :draw")
   ("draw-arrow" . "graphics.html#Function draw-arrow")
   ("draw-arrow*" . "graphics.html#Function draw-arrow*")
   ("draw-circle" . "graphics.html#Function draw-circle")
   ("draw-circle*" . "graphics.html#Function draw-circle*")
   ("draw-design" . "designs.html#Generic function draw-design")
   ("draw-ellipse" . "graphics.html#Function draw-ellipse")
   ("draw-ellipse*" . "graphics.html#Function draw-ellipse*")
   ("draw-line" . "graphics.html#Function draw-line")
   ("draw-line*" . "graphics.html#Function draw-line*")
   ("draw-lines" . "graphics.html#Function draw-lines")
   ("draw-lines*" . "graphics.html#Function draw-lines*")
   ("draw-oval" . "graphics.html#Function draw-oval")
   ("draw-oval*" . "graphics.html#Function draw-oval*")
   ("draw-pattern*" . "designs.html#Function draw-pattern*")
   ("draw-point" . "graphics.html#Function draw-point")
   ("draw-point*" . "graphics.html#Function draw-point*")
   ("draw-points" . "graphics.html#Function draw-points")
   ("draw-points*" . "graphics.html#Function draw-points*")
   ("draw-polygon" . "graphics.html#Function draw-polygon")
   ("draw-polygon*" . "graphics.html#Function draw-polygon*")
   ("draw-rectangle" . "graphics.html#Function draw-rectangle")
   ("draw-rectangle*" . "graphics.html#Function draw-rectangle*")
   ("draw-rectangles" . "graphics.html#Function draw-rectangles")
   ("draw-rectangles*" . "graphics.html#Function draw-rectangles*")
   ("draw-standard-menu" . "menus.html#Function draw-standard-menu")
   ("draw-text" . "graphics.html#Function draw-text")
   ("draw-text*" . "graphics.html#Function draw-text*")
   (":editable-p" . "gadgets.html#Init&nbsp;arg :editable-p")
   ("ellipse" . "regions.html#Protocol&nbsp;Class ellipse")
   ("ellipse-center-point"
   . "regions.html#Generic function ellipse-center-point")
   ("ellipse-center-point*"
   . "regions.html#Generic function ellipse-center-point*")
   ("ellipse-end-angle" . "regions.html#Generic function ellipse-end-angle")
   ("ellipsep" . "regions.html#Predicate ellipsep")
   ("ellipse-radii" . "regions.html#Generic function ellipse-radii")
   ("ellipse-start-angle" . "regions.html#Generic function ellipse-start-angle")
   ("elliptical-arc" . "regions.html#Protocol&nbsp;Class elliptical-arc")
   ("elliptical-arc-p" . "regions.html#Predicate elliptical-arc-p")
   ("enable-frame" . "frames.html#Generic function enable-frame")
   ("enable-process" . "clim-sys.html#Function enable-process")
   ("encapsulating-stream"
   . "encapsulating-streams.html#Protocol&nbsp;Class encapsulating-stream")
   ("encapsulating-stream-p"
   . "encapsulating-streams.html#Predicate encapsulating-stream-p")
   ("encapsulating-stream-stream"
   . "encapsulating-streams.html#Generic function encapsulating-stream-stream")
   (":end-of-line-action"
   . "extended-output.html#Init&nbsp;arg :end-of-line-action")
   (":end-of-line-action" . "panes.html#Option :end-of-line-action")
   (":end-of-page-action"
   . "extended-output.html#Init&nbsp;arg :end-of-page-action")
   (":end-of-page-action" . "panes.html#Option :end-of-page-action")
   ("engraft-medium" . "silica.html#Generic function engraft-medium")
   (":equalize-column-widths"
   . "table-formatting.html#Init&nbsp;arg :equalize-column-widths")
   ("erase-input-buffer"
   . "input-editing.html#Generic function erase-input-buffer")
   ("erase-output-record"
   . "output-recording.html#Generic function erase-output-record")
   ("even-scaling-transformation-p"
   . "transforms.html#Generic function even-scaling-transformation-p")
   ("event" . "silica.html#Protocol&nbsp;Class event")
   ("event-listen" . "silica.html#Generic function event-listen")
   ("event-matches-gesture-name-p"
   . "extended-input.html#Function event-matches-gesture-name-p")
   ("event-modifier-state" . "silica.html#Generic function event-modifier-state")
   ("eventp" . "silica.html#Predicate eventp")
   ("event-peek" . "silica.html#Generic function event-peek")
   ("event-read" . "silica.html#Generic function event-read")
   ("event-read-no-hang" . "silica.html#Generic function event-read-no-hang")
   ("event-sheet" . "silica.html#Generic function event-sheet")
   ("event-timestamp" . "silica.html#Generic function event-timestamp")
   ("event-type" . "silica.html#Generic function event-type")
   ("event-unread" . "silica.html#Generic function event-unread")
   ("+everywhere+" . "regions.html#Constant +everywhere+")
   ("execute-frame-command"
   . "frames.html#Generic function execute-frame-command")
   ("expand-presentation-type-abbreviation-1"
   . "presentation-types.html#Function expand-presentation-type-abbreviation-1")
   ("expand-presentation-type-abbreviation"
   . "presentation-types.html#Function expand-presentation-type-abbreviation")
   ("expression" . "presentation-types.html#Presentation Type expression")
   ("extended-input-stream"
   . "extended-input.html#Protocol&nbsp;Class extended-input-stream")
   ("extended-input-stream-p"
   . "extended-input.html#Predicate extended-input-stream-p")
   ("extended-output-stream"
   . "extended-output.html#Protocol&nbsp;Class extended-output-stream")
   ("extended-output-stream-p"
   . "extended-output.html#Predicate extended-output-stream-p")
   ("+fill+" . "panes.html#Constant +fill+")
   ("filling-output" . "text-formatting.html#Macro filling-output")
   ("find-applicable-translators"
   . "presentation-types.html#Function find-applicable-translators")
   ("find-cached-output-record"
   . "redisplay.html#Generic function find-cached-output-record")
   ("find-child-output-record"
   . "redisplay.html#Generic function find-child-output-record")
   ("find-command-from-command-line-name"
   . "commands.html#Function find-command-from-command-line-name")
   ("find-command-table" . "commands.html#Function find-command-table")
   ("find-frame-manager" . "frames.html#Function find-frame-manager")
   ("find-graft" . "silica.html#Function find-graft")
   ("find-innermost-applicable-presentation"
   . "presentation-types.html#Function find-innermost-applicable-presentation")
   ("find-keystroke-item" . "commands.html#Function find-keystroke-item")
   ("find-menu-item" . "commands.html#Function find-menu-item")
   ("find-pane-for-frame" . "frames.html#Generic function find-pane-for-frame")
   ("find-pane-named" . "frames.html#Generic function find-pane-named")
   ("find-port" . "silica.html#Function find-port")
   ("find-presentation-translator"
   . "commands.html#Function find-presentation-translator")
   ("find-presentation-translators"
   . "presentation-types.html#Function find-presentation-translators")
   ("find-presentation-type-class"
   . "presentation-types.html#Function find-presentation-type-class")
   (":fixed-position" . "redisplay.html#Init&nbsp;arg :fixed-position")
   ("+flipping-ink+" . "colors.html#Constant +flipping-ink+")
   ("float" . "presentation-types.html#Presentation Type float")
   (":foreground" . "extended-output.html#Init&nbsp;arg :foreground")
   (":foreground" . "panes.html#Option :foreground")
   ("+foreground-ink+" . "colors.html#Constant +foreground-ink+")
   ("form" . "presentation-types.html#Presentation Type form")
   ("format-graph-from-roots"
   . "graph-formatting.html#Function format-graph-from-roots")
   ("format-items" . "table-formatting.html#Function format-items")
   ("format-textual-list" . "text-formatting.html#Function format-textual-list")
   ("formatting-cell" . "table-formatting.html#Macro formatting-cell")
   ("formatting-column" . "table-formatting.html#Macro formatting-column")
   ("formatting-item-list" . "table-formatting.html#Macro formatting-item-list")
   ("formatting-row" . "table-formatting.html#Macro formatting-row")
   ("formatting-table" . "table-formatting.html#Macro formatting-table")
   ("frame-all-layouts" . "frames.html#Generic function frame-all-layouts")
   ("frame-calling-frame" . "frames.html#Generic function frame-calling-frame")
   ("frame-command-table" . "frames.html#Generic function frame-command-table")
   ("frame-current-layout" . "frames.html#Generic function frame-current-layout")
   ("frame-current-panes" . "frames.html#Generic function frame-current-panes")
   ("frame-document-highlighted-presentation"
   . "frames.html#Generic function frame-document-highlighted-presentation")
   ("frame-drag-and-drop-feedback"
   . "frames.html#Generic function frame-drag-and-drop-feedback")
   ("frame-drag-and-drop-highlighting"
   . "frames.html#Generic function frame-drag-and-drop-highlighting")
   ("frame-error-output" . "frames.html#Generic function frame-error-output")
   ("frame-exit" . "frames.html#Condition frame-exit")
   ("frame-exit" . "frames.html#Generic function frame-exit")
   ("frame-exit-frame" . "frames.html#Generic function frame-exit-frame")
   ("frame-find-innermost-applicable-presentation"
   . "frames.html#Generic function frame-find-innermost-applicable-presentation")
   ("frame-input-context-button-press-handler"
   . "frames.html#Generic function frame-input-context-button-press-handler")
   ("frame-maintain-presentation-histories"
   . "frames.html#Generic function frame-maintain-presentation-histories")
   ("frame-manager" . "frames.html#Generic function frame-manager")
   ("frame-manager" . "frames.html#Protocol&nbsp;Class frame-manager")
   ("frame-manager-frames" . "frames.html#Generic function frame-manager-frames")
   ("frame-manager-menu-choose"
   . "menus.html#Generic function frame-manager-menu-choose")
   ("frame-manager-notify-user"
   . "frames.html#Generic function frame-manager-notify-user")
   ("frame-mananger-p" . "frames.html#Predicate frame-mananger-p")
   ("frame-name" . "frames.html#Generic function frame-name")
   ("frame-panes" . "frames.html#Generic function frame-panes")
   ("frame-parent" . "frames.html#Generic function frame-parent")
   ("frame-pointer-documentation-output"
   . "frames.html#Generic function frame-pointer-documentation-output")
   ("frame-pretty-name" . "frames.html#Generic function frame-pretty-name")
   ("frame-properties" . "frames.html#Generic function frame-properties")
   ("frame-query-io" . "frames.html#Generic function frame-query-io")
   ("frame-replay" . "frames.html#Generic function frame-replay")
   ("frame-standard-input" . "frames.html#Generic function frame-standard-input")
   ("frame-standard-output"
   . "frames.html#Generic function frame-standard-output")
   ("frame-state" . "frames.html#Generic function frame-state")
   ("frame-top-level-sheet"
   . "frames.html#Generic function frame-top-level-sheet")
   ("funcall-presentation-generic-function"
   . "presentation-types.html#Macro funcall-presentation-generic-function")
   ("fundamental-binary-input-stream"
   . "gray-streams.html#Class fundamental-binary-input-stream")
   ("fundamental-binary-output-stream"
   . "gray-streams.html#Class fundamental-binary-output-stream")
   ("fundamental-binary-stream"
   . "gray-streams.html#Class fundamental-binary-stream")
   ("fundamental-character-input-stream"
   . "gray-streams.html#Class fundamental-character-input-stream")
   ("fundamental-character-output-stream"
   . "gray-streams.html#Class fundamental-character-output-stream")
   ("fundamental-character-stream"
   . "gray-streams.html#Class fundamental-character-stream")
   ("fundamental-input-stream"
   . "gray-streams.html#Class fundamental-input-stream")
   ("fundamental-output-stream"
   . "gray-streams.html#Class fundamental-output-stream")
   ("fundamental-stream" . "gray-streams.html#Class fundamental-stream")
   ("gadget" . "gadgets.html#Protocol&nbsp;Class gadget")
   ("gadget-activate-callback"
   . "gadgets.html#Generic function gadget-activate-callback")
   ("gadget-active-p" . "gadgets.html#Generic function gadget-active-p")
   ("gadget-armed-callback"
   . "gadgets.html#Generic function gadget-armed-callback")
   ("gadget-client" . "gadgets.html#Generic function gadget-client")
   ("gadget-dialog-view" . "presentation-types.html#Class gadget-dialog-view")
   ("+gadget-dialog-view+"
   . "presentation-types.html#Constant +gadget-dialog-view+")
   ("gadget-disarmed-callback"
   . "gadgets.html#Generic function gadget-disarmed-callback")
   ("gadget-id" . "gadgets.html#Generic function gadget-id")
   ("gadget-label" . "gadgets.html#Generic function gadget-label")
   ("gadget-label-align-x"
   . "gadgets.html#Generic function gadget-label-align-x")
   ("gadget-label-align-y"
   . "gadgets.html#Generic function gadget-label-align-y")
   ("gadget-max-value" . "gadgets.html#Generic function gadget-max-value")
   ("gadget-menu-view" . "presentation-types.html#Class gadget-menu-view")
   ("+gadget-menu-view+" . "presentation-types.html#Constant +gadget-menu-view+")
   ("gadget-min-value" . "gadgets.html#Generic function gadget-min-value")
   ("gadget-orientation" . "gadgets.html#Generic function gadget-orientation")
   ("gadget-output-record" . "gadgets.html#Class gadget-output-record")
   ("gadgetp" . "gadgets.html#Predicate gadgetp")
   ("gadget-range" . "gadgets.html#Generic function gadget-range")
   ("gadget-range*" . "gadgets.html#Generic function gadget-range*")
   ("gadget-show-value-p" . "gadgets.html#Generic function gadget-show-value-p")
   ("gadget-value" . "gadgets.html#Generic function gadget-value")
   ("gadget-value" . "gadgets.html#Method gadget-value")
   ("gadget-value" . "gadgets.html#Method gadget-value")
   ("gadget-value" . "gadgets.html#Method gadget-value")
   ("gadget-value" . "gadgets.html#Method gadget-value")
   ("gadget-value" . "gadgets.html#Method gadget-value")
   ("gadget-value" . "gadgets.html#Method gadget-value")
   ("gadget-value" . "gadgets.html#Method gadget-value")
   ("gadget-value" . "gadgets.html#Method gadget-value")
   ("gadget-value" . "gadgets.html#Method gadget-value")
   ("gadget-value-changed-callback"
   . "gadgets.html#Generic function gadget-value-changed-callback")
   ("gadget-view" . "presentation-types.html#Class gadget-view")
   ("+gadget-view+" . "presentation-types.html#Constant +gadget-view+")
   ("generate-graph-nodes"
   . "graph-formatting.html#Generic function generate-graph-nodes")
   ("generate-panes" . "frames.html#Generic function generate-panes")
   (":generation-separation"
   . "graph-formatting.html#Init&nbsp;arg :generation-separation")
   ("generic-list-pane" . "gadgets.html#Class generic-list-pane")
   ("generic-option-pane" . "gadgets.html#Class generic-option-pane")
   ("get-frame-pane" . "frames.html#Generic function get-frame-pane")
   ("global-command-table" . "commands.html#Command Table global-command-table")
   ("graft" . "silica.html#Generic function graft")
   ("graft-height" . "silica.html#Generic function graft-height")
   ("graft-orientation" . "silica.html#Generic function graft-orientation")
   ("graft-pixels-per-inch" . "silica.html#Function graft-pixels-per-inch")
   ("graft-pixels-per-millimeter"
   . "silica.html#Function graft-pixels-per-millimeter")
   ("graft-units" . "silica.html#Generic function graft-units")
   ("graft-width" . "silica.html#Generic function graft-width")
   ("graphics-displayed-output-record"
   . "output-recording.html#Protocol&nbsp;Class graphics-displayed-output-record")
   ("graphics-displayed-output-record-p"
   . "output-recording.html#Predicate graphics-displayed-output-record-p")
   ("graph-node-children"
   . "graph-formatting.html#Generic function graph-node-children")
   ("graph-node-object"
   . "graph-formatting.html#Generic function graph-node-object")
   ("graph-node-output-record"
   . "graph-formatting.html#Protocol&nbsp;Class graph-node-output-record")
   ("graph-node-output-record-p"
   . "graph-formatting.html#Predicate graph-node-output-record-p")
   ("graph-node-parents"
   . "graph-formatting.html#Generic function graph-node-parents")
   ("graph-output-record"
   . "graph-formatting.html#Protocol&nbsp;Class graph-output-record")
   ("graph-output-record-p"
   . "graph-formatting.html#Predicate graph-output-record-p")
   ("graph-root-nodes"
   . "graph-formatting.html#Generic function graph-root-nodes")
   ("+green+" . "colors.html#Constant +green+")
   ("grid-pane" . "panes.html#Layout Pane grid-pane")
   ("handle-event" . "silica.html#Generic function handle-event")
   ("handle-repaint" . "silica.html#Generic function handle-repaint")
   (":hash-table" . "graph-formatting.html#Init&nbsp;arg :hash-table")
   ("hbox-pane" . "panes.html#Layout Pane hbox-pane")
   (":height" . "panes.html#Option :height")
   ("*help-gestures*" . "input-editing.html#Variable *help-gestures*")
   ("highlight-applicable-presentation"
   . "presentation-types.html#Function highlight-applicable-presentation")
   ("highlight-output-record"
   . "output-recording.html#Generic function highlight-output-record")
   ("highlight-presentation"
   . "presentation-types.html#Presentation Method highlight-presentation")
   ("horizontally" . "panes.html#Macro horizontally")
   ("hrack-pane" . "panes.html#Layout Pane hrack-pane")
   ("+hyper-key+" . "silica.html#Constant +hyper-key+")
   (":id" . "gadgets.html#Init&nbsp;arg :id")
   ("+identity-transformation+"
   . "transforms.html#Constant +identity-transformation+")
   ("identity-transformation-p"
   . "transforms.html#Generic function identity-transformation-p")
   (":id-test" . "redisplay.html#Init&nbsp;arg :id-test")
   ("immediate-repainting-mixin"
   . "silica.html#Class immediate-repainting-mixin")
   ("immediate-rescan" . "input-editing.html#Generic function immediate-rescan")
   ("immediate-sheet-input-mixin"
   . "silica.html#Class immediate-sheet-input-mixin")
   (":incremental-redisplay" . "panes.html#Option :incremental-redisplay")
   ("incremental-redisplay"
   . "redisplay.html#Generic function incremental-redisplay")
   ("indenting-output" . "text-formatting.html#Macro indenting-output")
   (":indicator-type" . "gadgets.html#Init&nbsp;arg :indicator-type")
   (":initial-spacing" . "table-formatting.html#Init&nbsp;arg :initial-spacing")
   (":ink" . "drawing-options.html#Option :ink")
   (":input-buffer" . "extended-input.html#Init&nbsp;arg :input-buffer")
   ("*input-context*" . "presentation-types.html#Variable *input-context*")
   ("input-context-type" . "presentation-types.html#Function input-context-type")
   ("input-editing-stream"
   . "input-editing.html#Protocol&nbsp;Class input-editing-stream")
   ("input-editing-stream-p"
   . "input-editing.html#Predicate input-editing-stream-p")
   ("input-editor-format"
   . "input-editing.html#Generic function input-editor-format")
   ("input-not-of-required-type"
   . "input-editing.html#Error input-not-of-required-type")
   ("input-not-of-required-type"
   . "input-editing.html#Function input-not-of-required-type")
   ("input-stream-p" . "gray-streams.html#Generic function input-stream-p")
   ("*input-wait-handler*" . "extended-input.html#Variable *input-wait-handler*")
   ("*input-wait-test*" . "extended-input.html#Variable *input-wait-test*")
   ("integer" . "presentation-types.html#Presentation Type integer")
   ("interactive-stream-p" . "input-editing.html#Predicate interactive-stream-p")
   ("interactor-pane" . "panes.html#Service Pane interactor-pane")
   ("invalidate-cached-regions"
   . "silica.html#Generic function invalidate-cached-regions")
   ("invalidate-cached-transformations"
   . "silica.html#Generic function invalidate-cached-transformations")
   ("invertible-transformation-p"
   . "transforms.html#Generic function invertible-transformation-p")
   ("invert-transformation"
   . "transforms.html#Generic function invert-transformation")
   ("invoke-accept-values-command-button"
   . "dialogs.html#Method invoke-accept-values-command-button")
   ("invoke-updating-output"
   . "redisplay.html#Generic function invoke-updating-output")
   ("invoke-with-drawing-options"
   . "drawing-options.html#Generic function invoke-with-drawing-options")
   ("invoke-with-new-output-record"
   . "output-recording.html#Generic function invoke-with-new-output-record")
   ("invoke-with-output-recording-options"
   . "output-recording.html#Generic function invoke-with-output-recording-options")
   ("invoke-with-output-to-output-record"
   . "output-recording.html#Generic function invoke-with-output-to-output-record")
   ("invoke-with-text-style"
   . "text-styles.html#Generic function invoke-with-text-style")
   ("item-list-output-record"
   . "table-formatting.html#Protocol&nbsp;Class item-list-output-record")
   ("item-list-output-record-p"
   . "table-formatting.html#Predicate item-list-output-record-p")
   (":items" . "gadgets.html#Init&nbsp;arg :items")
   (":items" . "gadgets.html#Init&nbsp;arg :items")
   ("keyboard-event" . "silica.html#Class keyboard-event")
   ("keyboard-event-character"
   . "silica.html#Generic function keyboard-event-character")
   ("keyboard-event-key-name"
   . "silica.html#Generic function keyboard-event-key-name")
   (":key-name" . "silica.html#Init&nbsp;arg :key-name")
   ("key-press-event" . "silica.html#Class key-press-event")
   ("key-release-event" . "silica.html#Class key-release-event")
   ("keyword" . "presentation-types.html#Presentation Type keyword")
   (":label" . "gadgets.html#Init&nbsp;arg :label")
   ("labelled-gadget-mixin" . "gadgets.html#Class labelled-gadget-mixin")
   ("labelling" . "panes.html#Macro labelling")
   ("label-pane" . "panes.html#Service Pane label-pane")
   ("layout-frame" . "frames.html#Generic function layout-frame")
   ("layout-graph-edges"
   . "graph-formatting.html#Generic function layout-graph-edges")
   ("layout-graph-nodes"
   . "graph-formatting.html#Generic function layout-graph-nodes")
   ("line" . "regions.html#Protocol&nbsp;Class line")
   (":line-cap-shape" . "drawing-options.html#Option :line-cap-shape")
   (":line-dashes" . "drawing-options.html#Option :line-dashes")
   ("line-end-point" . "regions.html#Generic function line-end-point")
   ("line-end-point*" . "regions.html#Generic function line-end-point*")
   (":line-joint-shape" . "drawing-options.html#Option :line-joint-shape")
   ("linep" . "regions.html#Predicate linep")
   ("line-start-point" . "regions.html#Generic function line-start-point")
   ("line-start-point*" . "regions.html#Generic function line-start-point*")
   (":line-style" . "drawing-options.html#Option :line-style")
   ("line-style" . "drawing-options.html#Protocol&nbsp;Class line-style")
   ("line-style-cap-shape"
   . "drawing-options.html#Generic function line-style-cap-shape")
   ("line-style-dashes"
   . "drawing-options.html#Generic function line-style-dashes")
   ("line-style-joint-shape"
   . "drawing-options.html#Generic function line-style-joint-shape")
   ("line-style-p" . "drawing-options.html#Predicate line-style-p")
   ("line-style-thickness"
   . "drawing-options.html#Generic function line-style-thickness")
   ("line-style-unit" . "drawing-options.html#Generic function line-style-unit")
   (":line-thickness" . "drawing-options.html#Option :line-thickness")
   (":line-unit" . "drawing-options.html#Option :line-unit")
   ("list-pane" . "gadgets.html#Class list-pane")
   ("lookup-keystroke-command-item"
   . "commands.html#Function lookup-keystroke-command-item")
   ("lookup-keystroke-item" . "commands.html#Function lookup-keystroke-item")
   ("+magenta+" . "colors.html#Constant +magenta+")
   ("make-3-point-transformation"
   . "transforms.html#Function make-3-point-transformation")
   ("make-3-point-transformation*"
   . "transforms.html#Function make-3-point-transformation*")
   ("make-application-frame" . "frames.html#Function make-application-frame")
   ("make-bounding-rectangle" . "bboxes.html#Function make-bounding-rectangle")
   ("make-clim-application-pane"
   . "panes.html#Function make-clim-application-pane")
   ("make-clim-interactor-pane"
   . "panes.html#Function make-clim-interactor-pane")
   ("make-clim-stream-pane" . "panes.html#Function make-clim-stream-pane")
   ("make-command-table" . "commands.html#Function make-command-table")
   ("make-contrasting-dash-patterns"
   . "drawing-options.html#Function make-contrasting-dash-patterns")
   ("make-contrasting-inks" . "colors.html#Function make-contrasting-inks")
   ("make-design-from-output-record"
   . "output-recording.html#Generic function make-design-from-output-record")
   ("make-device-font-text-style"
   . "text-styles.html#Function make-device-font-text-style")
   ("make-ellipse" . "regions.html#Function make-ellipse")
   ("make-ellipse*" . "regions.html#Function make-ellipse*")
   ("make-elliptical-arc" . "regions.html#Function make-elliptical-arc")
   ("make-elliptical-arc*" . "regions.html#Function make-elliptical-arc*")
   ("make-flipping-ink" . "colors.html#Function make-flipping-ink")
   ("make-gray-color" . "colors.html#Function make-gray-color")
   ("make-ihs-color" . "colors.html#Function make-ihs-color")
   ("make-line" . "regions.html#Function make-line")
   ("make-line*" . "regions.html#Function make-line*")
   ("make-line-style" . "drawing-options.html#Function make-line-style")
   ("make-lock" . "clim-sys.html#Function make-lock")
   ("make-medium" . "silica.html#Generic function make-medium")
   ("make-modifier-state" . "extended-input.html#Function make-modifier-state")
   ("make-opacity" . "colors.html#Function make-opacity")
   ("make-pane-1" . "panes.html#Generic function make-pane-1")
   ("make-pane" . "panes.html#Function make-pane")
   ("make-pattern" . "designs.html#Function make-pattern")
   ("make-pattern-from-bitmap-file"
   . "extensions.html#Function make-pattern-from-bitmap-file")
   ("make-point" . "regions.html#Function make-point")
   ("make-polygon" . "regions.html#Function make-polygon")
   ("make-polygon*" . "regions.html#Function make-polygon*")
   ("make-polyline" . "regions.html#Function make-polyline")
   ("make-polyline*" . "regions.html#Function make-polyline*")
   ("make-presentation-type-specifier"
   . "presentation-types.html#Function make-presentation-type-specifier")
   ("make-process" . "clim-sys.html#Function make-process")
   ("make-rectangle" . "regions.html#Function make-rectangle")
   ("make-rectangle*" . "regions.html#Function make-rectangle*")
   ("make-rectangular-tile" . "designs.html#Function make-rectangular-tile")
   ("make-recursive-lock" . "clim-sys.html#Function make-recursive-lock")
   ("make-reflection-transformation"
   . "transforms.html#Function make-reflection-transformation")
   ("make-reflection-transformation*"
   . "transforms.html#Function make-reflection-transformation*")
   ("make-rgb-color" . "colors.html#Function make-rgb-color")
   ("make-rotation-transformation"
   . "transforms.html#Function make-rotation-transformation")
   ("make-rotation-transformation*"
   . "transforms.html#Function make-rotation-transformation*")
   ("make-scaling-transformation"
   . "transforms.html#Function make-scaling-transformation")
   ("make-scaling-transformation*"
   . "transforms.html#Function make-scaling-transformation*")
   ("make-space-requirement" . "panes.html#Function make-space-requirement")
   ("make-stencil" . "designs.html#Function make-stencil")
   ("make-text-style" . "text-styles.html#Function make-text-style")
   ("make-transformation" . "transforms.html#Function make-transformation")
   ("make-translation-transformation"
   . "transforms.html#Function make-translation-transformation")
   ("map-over-command-table-commands"
   . "commands.html#Function map-over-command-table-commands")
   ("map-over-command-table-keystrokes"
   . "commands.html#Function map-over-command-table-keystrokes")
   ("map-over-command-table-menu-items"
   . "commands.html#Function map-over-command-table-menu-items")
   ("map-over-command-table-names"
   . "commands.html#Function map-over-command-table-names")
   ("map-over-command-table-translators"
   . "commands.html#Function map-over-command-table-translators")
   ("map-over-frames" . "frames.html#Function map-over-frames")
   ("map-over-grafts" . "silica.html#Function map-over-grafts")
   ("map-over-item-list-cells"
   . "table-formatting.html#Generic function map-over-item-list-cells")
   ("map-over-output-records-containing-position"
   . "output-recording.html#Generic function map-over-output-records-containing-position")
   ("map-over-output-records-overlapping-region"
   . "output-recording.html#Generic function map-over-output-records-overlapping-region")
   ("map-over-polygon-coordinates"
   . "regions.html#Generic function map-over-polygon-coordinates")
   ("map-over-polygon-segments"
   . "regions.html#Generic function map-over-polygon-segments")
   ("map-over-ports" . "silica.html#Function map-over-ports")
   ("map-over-presentation-type-supertypes"
   . "presentation-types.html#Function map-over-presentation-type-supertypes")
   ("map-over-presentation-type-supertypes"
   . "presentation-types.html#Presentation Method map-over-presentation-type-supertypes")
   ("map-over-region-set-regions"
   . "regions.html#Generic function map-over-region-set-regions")
   ("map-over-row-cells"
   . "table-formatting.html#Generic function map-over-row-cells")
   ("map-over-row-cells"
   . "table-formatting.html#Generic function map-over-row-cells")
   ("map-over-sheets" . "silica.html#Generic function map-over-sheets")
   ("map-over-sheets-containing-position"
   . "silica.html#Generic function map-over-sheets-containing-position")
   ("map-over-sheets-overlapping-region"
   . "silica.html#Generic function map-over-sheets-overlapping-region")
   ("map-over-table-elements"
   . "table-formatting.html#Generic function map-over-table-elements")
   ("map-resource" . "clim-sys.html#Function map-resource")
   ("map-sheet-position-to-child"
   . "silica.html#Generic function map-sheet-position-to-child")
   ("map-sheet-position-to-parent"
   . "silica.html#Generic function map-sheet-position-to-parent")
   ("map-sheet-rectangle*-to-child"
   . "silica.html#Generic function map-sheet-rectangle*-to-child")
   ("map-sheet-rectangle*-to-parent"
   . "silica.html#Generic function map-sheet-rectangle*-to-parent")
   ("match-output-records"
   . "redisplay.html#Generic function match-output-records")
   (":max-height" . "panes.html#Option :max-height")
   (":max-height" . "table-formatting.html#Init&nbsp;arg :max-height")
   (":max-label" . "gadgets.html#Init&nbsp;arg :max-label")
   (":max-value" . "gadgets.html#Init&nbsp;arg :max-value")
   (":max-width" . "panes.html#Option :max-width")
   (":max-width" . "table-formatting.html#Init&nbsp;arg :max-width")
   ("medium" . "silica.html#Protocol&nbsp;Class medium")
   ("medium-background"
   . "drawing-options.html#Generic function medium-background")
   ("medium-background" . "silica.html#Generic function medium-background")
   ("medium-beep" . "graphics.html#Generic function medium-beep")
   ("medium-buffering-output-p"
   . "extended-output.html#Generic function medium-buffering-output-p")
   ("medium-clear-area" . "graphics.html#Generic function medium-clear-area")
   ("medium-clipping-region"
   . "drawing-options.html#Generic function medium-clipping-region")
   ("medium-clipping-region"
   . "silica.html#Generic function medium-clipping-region")
   ("medium-copy-area" . "graphics.html#Generic function medium-copy-area")
   ("medium-current-text-style"
   . "drawing-options.html#Generic function medium-current-text-style")
   ("medium-default-text-style"
   . "drawing-options.html#Generic function medium-default-text-style")
   ("medium-default-text-style"
   . "silica.html#Generic function medium-default-text-style")
   ("medium-drawable" . "silica.html#Generic function medium-drawable")
   ("medium-draw-ellipse*"
   . "graphics.html#Generic function medium-draw-ellipse*")
   ("medium-draw-line*" . "graphics.html#Generic function medium-draw-line*")
   ("medium-draw-lines*" . "graphics.html#Generic function medium-draw-lines*")
   ("medium-draw-point*" . "graphics.html#Generic function medium-draw-point*")
   ("medium-draw-points*" . "graphics.html#Generic function medium-draw-points*")
   ("medium-draw-polygon*"
   . "graphics.html#Generic function medium-draw-polygon*")
   ("medium-draw-rectangle*"
   . "graphics.html#Generic function medium-draw-rectangle*")
   ("medium-draw-rectangles*"
   . "graphics.html#Generic function medium-draw-rectangles*")
   ("medium-draw-text*" . "graphics.html#Generic function medium-draw-text*")
   ("medium-finish-output"
   . "graphics.html#Generic function medium-finish-output")
   ("medium-force-output" . "graphics.html#Generic function medium-force-output")
   ("medium-foreground"
   . "drawing-options.html#Generic function medium-foreground")
   ("medium-foreground" . "silica.html#Generic function medium-foreground")
   ("medium-ink" . "drawing-options.html#Generic function medium-ink")
   ("medium-ink" . "silica.html#Generic function medium-ink")
   ("medium-line-style"
   . "drawing-options.html#Generic function medium-line-style")
   ("medium-line-style" . "silica.html#Generic function medium-line-style")
   ("medium-merged-text-style"
   . "silica.html#Generic function medium-merged-text-style")
   ("mediump" . "silica.html#Predicate mediump")
   ("medium-sheet" . "silica.html#Generic function medium-sheet")
   ("medium-text-style"
   . "drawing-options.html#Generic function medium-text-style")
   ("medium-text-style" . "silica.html#Generic function medium-text-style")
   ("medium-transformation"
   . "drawing-options.html#Generic function medium-transformation")
   ("medium-transformation"
   . "silica.html#Generic function medium-transformation")
   ("member" . "presentation-types.html#Presentation Type Abbreveation member")
   ("member-alist"
   . "presentation-types.html#Presentation Type Abbreveation member-alist")
   ("member-sequence"
   . "presentation-types.html#Presentation Type Abbreveation member-sequence")
   (":menu-bar" . "frames.html#Init&nbsp;arg :menu-bar")
   ("menu-button" . "gadgets.html#Class menu-button")
   ("menu-button-pane" . "gadgets.html#Class menu-button-pane")
   ("menu-choose" . "menus.html#Generic function menu-choose")
   ("menu-choose-command-from-command-table"
   . "commands.html#Function menu-choose-command-from-command-table")
   ("menu-choose-from-drawer"
   . "menus.html#Generic function menu-choose-from-drawer")
   ("menu-command-parser" . "commands.html#Function menu-command-parser")
   ("menu-item-display" . "menus.html#Function menu-item-display")
   ("menu-item-options" . "menus.html#Function menu-item-options")
   ("menu-item-value" . "menus.html#Function menu-item-value")
   ("menu-read-remaining-arguments-for-partial-command"
   . "commands.html#Function menu-read-remaining-arguments-for-partial-command")
   (":merge-duplicates"
   . "graph-formatting.html#Init&nbsp;arg :merge-duplicates")
   ("merge-text-styles" . "text-styles.html#Generic function merge-text-styles")
   ("+meta-key+" . "silica.html#Constant +meta-key+")
   (":min-height" . "panes.html#Option :min-height")
   (":min-height" . "table-formatting.html#Init&nbsp;arg :min-height")
   (":min-label" . "gadgets.html#Init&nbsp;arg :min-label")
   (":min-value" . "gadgets.html#Init&nbsp;arg :min-value")
   (":min-width" . "panes.html#Option :min-width")
   (":min-width" . "table-formatting.html#Init&nbsp;arg :min-width")
   ("mirrored-sheet-mixin" . "silica.html#Class mirrored-sheet-mixin")
   (":mode" . "gadgets.html#Init&nbsp;arg :mode")
   (":mode" . "gadgets.html#Init&nbsp;arg :mode")
   (":modifier" . "presentation-types.html#Init&nbsp;arg :modifier")
   (":modifier-state" . "silica.html#Init&nbsp;arg :modifier-state")
   ("modifier-state-matches-gesture-name-p"
   . "extended-input.html#Function modifier-state-matches-gesture-name-p")
   ("move-and-resize-sheet"
   . "silica.html#Generic function move-and-resize-sheet")
   ("move-sheet" . "silica.html#Generic function move-sheet")
   (":multiple-columns-x-spacing"
   . "table-formatting.html#Init&nbsp;arg :multiple-columns-x-spacing")
   ("*multiprocessing-p*" . "clim-sys.html#Variable *multiprocessing-p*")
   (":name" . "frames.html#Init&nbsp;arg :name")
   (":name" . "panes.html#Option :name")
   (":name-key" . "gadgets.html#Init&nbsp;arg :name-key")
   (":name-key" . "gadgets.html#Init&nbsp;arg :name-key")
   (":ncolumns" . "gadgets.html#Init&nbsp;arg :ncolumns")
   (":n-columns" . "table-formatting.html#Init&nbsp;arg :n-columns")
   ("new-page" . "extensions.html#Function new-page")
   ("nil" . "presentation-types.html#Presentation Type nil")
   (":nlines" . "gadgets.html#Init&nbsp;arg :nlines")
   ("note-command-disabled"
   . "frames.html#Generic function note-command-disabled")
   ("note-command-enabled" . "frames.html#Generic function note-command-enabled")
   ("note-frame-deiconified"
   . "frames.html#Generic function note-frame-deiconified")
   ("note-frame-disabled" . "frames.html#Generic function note-frame-disabled")
   ("note-frame-enabled" . "frames.html#Generic function note-frame-enabled")
   ("note-frame-iconified" . "frames.html#Generic function note-frame-iconified")
   ("note-gadget-activated"
   . "gadgets.html#Generic function note-gadget-activated")
   ("note-gadget-deactivated"
   . "gadgets.html#Generic function note-gadget-deactivated")
   ("note-output-record-child-changed"
   . "redisplay.html#Generic function note-output-record-child-changed")
   ("note-sheet-adopted" . "silica.html#Generic function note-sheet-adopted")
   ("note-sheet-degrafted" . "silica.html#Generic function note-sheet-degrafted")
   ("note-sheet-disabled" . "silica.html#Generic function note-sheet-disabled")
   ("note-sheet-disowned" . "silica.html#Generic function note-sheet-disowned")
   ("note-sheet-enabled" . "silica.html#Generic function note-sheet-enabled")
   ("note-sheet-grafted" . "silica.html#Generic function note-sheet-grafted")
   ("note-sheet-region-changed"
   . "silica.html#Generic function note-sheet-region-changed")
   ("note-sheet-transformation-changed"
   . "silica.html#Generic function note-sheet-transformation-changed")
   ("note-space-requirements-changed"
   . "panes.html#Generic function note-space-requirements-changed")
   ("notify-user" . "frames.html#Generic function notify-user")
   ("+nowhere+" . "regions.html#Constant +nowhere+")
   (":n-rows" . "table-formatting.html#Init&nbsp;arg :n-rows")
   ("null" . "presentation-types.html#Presentation Type null")
   ("null-or-type"
   . "presentation-types.html#Presentation Type Abbreveation null-or-type")
   ("*null-presentation*"
   . "presentation-types.html#Constant *null-presentation*")
   ("number" . "presentation-types.html#Presentation Type number")
   (":number-of-quanta" . "gadgets.html#Init&nbsp;arg :number-of-quanta")
   (":number-of-tick-marks" . "gadgets.html#Init&nbsp;arg :number-of-tick-marks")
   ("*numeric-argument-marker*"
   . "commands.html#Variable *numeric-argument-marker*")
   (":object" . "presentation-types.html#Init&nbsp;arg :object")
   ("opacity" . "colors.html#Protocol&nbsp;Class opacity")
   ("opacityp" . "colors.html#Predicate opacityp")
   ("opacity-value" . "colors.html#Generic function opacity-value")
   ("open-stream-p" . "gray-streams.html#Generic function open-stream-p")
   ("open-window-stream" . "panes.html#Function open-window-stream")
   ("option-pane" . "gadgets.html#Class option-pane")
   ("or" . "presentation-types.html#Presentation Type or")
   (":orientation" . "gadgets.html#Init&nbsp;arg :orientation")
   (":orientation" . "graph-formatting.html#Init&nbsp;arg :orientation")
   ("oriented-gadget-mixin" . "gadgets.html#Class oriented-gadget-mixin")
   ("*original-stream*"
   . "encapsulating-streams.html#Variable *original-stream*")
   ("outlined-pane" . "panes.html#Layout Pane outlined-pane")
   ("outlining" . "panes.html#Macro outlining")
   ("output-record" . "output-recording.html#Protocol&nbsp;Class output-record")
   (":output-record" . "panes.html#Option :output-record")
   ("output-record-cache-value"
   . "redisplay.html#Generic function output-record-cache-value")
   ("output-record-children"
   . "output-recording.html#Generic function output-record-children")
   ("output-record-contents-ok"
   . "redisplay.html#Generic function output-record-contents-ok")
   ("output-record-count"
   . "output-recording.html#Generic function output-record-count")
   ("output-record-displayer"
   . "redisplay.html#Generic function output-record-displayer")
   ("output-record-end-cursor-position"
   . "output-recording.html#Generic function output-record-end-cursor-position")
   ("output-record-fixed-position"
   . "redisplay.html#Generic function output-record-fixed-position")
   ("output-record-hit-detection-rectangle*"
   . "output-recording.html#Generic function output-record-hit-detection-rectangle*")
   ("output-recording-stream"
   . "output-recording.html#Protocol&nbsp;Class output-recording-stream")
   ("output-recording-stream-p"
   . "output-recording.html#Predicate output-recording-stream-p")
   ("output-record-p" . "output-recording.html#Predicate output-record-p")
   ("output-record-parent"
   . "output-recording.html#Generic function output-record-parent")
   ("output-record-position"
   . "output-recording.html#Generic function output-record-position")
   ("output-record-refined-position-test"
   . "output-recording.html#Generic function output-record-refined-position-test")
   ("output-record-start-cursor-position"
   . "output-recording.html#Generic function output-record-start-cursor-position")
   ("output-record-unique-id"
   . "redisplay.html#Generic function output-record-unique-id")
   ("output-stream-p" . "gray-streams.html#Generic function output-stream-p")
   ("pane" . "panes.html#Protocol&nbsp;Class pane")
   ("pane-background" . "panes.html#Generic function pane-background")
   ("pane-foreground" . "panes.html#Generic function pane-foreground")
   ("pane-frame" . "panes.html#Generic function pane-frame")
   ("pane-name" . "panes.html#Generic function pane-name")
   ("pane-needs-redisplay" . "frames.html#Generic function pane-needs-redisplay")
   ("panep" . "panes.html#Predicate panep")
   (":panes" . "frames.html#Init&nbsp;arg :panes")
   ("pane-scroller" . "panes.html#Generic function pane-scroller")
   ("pane-text-style" . "panes.html#Generic function pane-text-style")
   ("pane-viewport" . "panes.html#Generic function pane-viewport")
   ("pane-viewport-region" . "panes.html#Generic function pane-viewport-region")
   (":parent" . "output-recording.html#Init&nbsp;arg :parent")
   ("parse-text-style" . "text-styles.html#Function parse-text-style")
   ("partial-command-p" . "commands.html#Function partial-command-p")
   ("*partial-command-parser*"
   . "commands.html#Variable *partial-command-parser*")
   ("path" . "regions.html#Protocol&nbsp;Class path")
   ("pathname" . "presentation-types.html#Presentation Type pathname")
   ("pathp" . "regions.html#Predicate pathp")
   ("pattern-height" . "designs.html#Generic function pattern-height")
   ("pattern-width" . "designs.html#Generic function pattern-width")
   ("permanent-medium-sheet-output-mixin"
   . "silica.html#Class permanent-medium-sheet-output-mixin")
   ("pixmap-depth" . "graphics.html#Generic function pixmap-depth")
   ("pixmap-height" . "graphics.html#Generic function pixmap-height")
   ("pixmap-width" . "graphics.html#Generic function pixmap-width")
   ("point" . "regions.html#Protocol&nbsp;Class point")
   (":pointer" . "extended-input.html#Init&nbsp;arg :pointer")
   ("pointer" . "extended-input.html#Protocol&nbsp;Class pointer")
   (":pointer" . "silica.html#Init&nbsp;arg :pointer")
   ("pointer-boundary-event" . "silica.html#Class pointer-boundary-event")
   ("pointer-boundary-event-kind"
   . "silica.html#Generic function pointer-boundary-event-kind")
   ("pointer-button-event" . "silica.html#Class pointer-button-event")
   ("pointer-button-hold-event" . "silica.html#Class pointer-button-hold-event")
   ("pointer-button-press-event"
   . "silica.html#Class pointer-button-press-event")
   ("*pointer-button-press-handler*"
   . "extended-input.html#Variable *pointer-button-press-handler*")
   ("pointer-button-release-event"
   . "silica.html#Class pointer-button-release-event")
   ("pointer-button-state"
   . "extended-input.html#Generic function pointer-button-state")
   ("pointer-click-and-hold-event"
   . "silica.html#Class pointer-click-and-hold-event")
   ("pointer-click-event" . "silica.html#Class pointer-click-event")
   ("pointer-cursor" . "extended-input.html#Generic function pointer-cursor")
   ("*pointer-documentation-output*"
   . "frames.html#Variable *pointer-documentation-output*")
   ("pointer-documentation-pane"
   . "panes.html#Service Pane pointer-documentation-pane")
   ("pointer-documentation-view"
   . "presentation-types.html#Class pointer-documentation-view")
   ("+pointer-documentation-view+"
   . "presentation-types.html#Constant +pointer-documentation-view+")
   ("pointer-double-click-event"
   . "silica.html#Class pointer-double-click-event")
   ("pointer-enter-event" . "silica.html#Class pointer-enter-event")
   ("pointer-event" . "silica.html#Class pointer-event")
   ("pointer-event-button" . "silica.html#Generic function pointer-event-button")
   ("pointer-event-native-x"
   . "silica.html#Generic function pointer-event-native-x")
   ("pointer-event-native-y"
   . "silica.html#Generic function pointer-event-native-y")
   ("pointer-event-pointer"
   . "silica.html#Generic function pointer-event-pointer")
   ("pointer-event-x" . "silica.html#Generic function pointer-event-x")
   ("pointer-event-y" . "silica.html#Generic function pointer-event-y")
   ("pointer-exit-event" . "silica.html#Class pointer-exit-event")
   ("+pointer-left-button+" . "silica.html#Constant +pointer-left-button+")
   ("+pointer-middle-button+" . "silica.html#Constant +pointer-middle-button+")
   ("pointer-motion-event" . "silica.html#Class pointer-motion-event")
   ("pointerp" . "extended-input.html#Predicate pointerp")
   ("pointer-position" . "extended-input.html#Generic function pointer-position")
   ("+pointer-right-button+" . "silica.html#Constant +pointer-right-button+")
   ("pointer-sheet" . "extended-input.html#Generic function pointer-sheet")
   ("pointp" . "regions.html#Predicate pointp")
   ("point-position" . "regions.html#Generic function point-position")
   ("point-x" . "regions.html#Generic function point-x")
   ("point-y" . "regions.html#Generic function point-y")
   ("polygon" . "regions.html#Protocol&nbsp;Class polygon")
   ("polygonp" . "regions.html#Predicate polygonp")
   ("polygon-points" . "regions.html#Generic function polygon-points")
   ("polyline" . "regions.html#Protocol&nbsp;Class polyline")
   ("polyline-closed" . "regions.html#Generic function polyline-closed")
   ("polylinep" . "regions.html#Predicate polylinep")
   (":port" . "extended-input.html#Init&nbsp;arg :port")
   ("port" . "extended-input.html#Method port")
   ("port" . "frames.html#Method port") ("port" . "frames.html#Method port")
   ("port" . "silica.html#Generic function port")
   ("port" . "silica.html#Method port") ("port" . "silica.html#Method port")
   ("port" . "silica.html#Protocol&nbsp;Class port")
   ("port-keyboard-input-focus"
   . "silica.html#Generic function port-keyboard-input-focus")
   ("port-name" . "silica.html#Generic function port-name")
   ("portp" . "silica.html#Predicate portp")
   ("port-properties" . "silica.html#Generic function port-properties")
   ("port-server-path" . "silica.html#Generic function port-server-path")
   ("port-type" . "silica.html#Generic function port-type")
   ("*possibilities-gestures*"
   . "input-editing.html#Variable *possibilities-gestures*")
   ("present" . "presentation-types.html#Function present")
   ("present" . "presentation-types.html#Presentation Method present")
   ("presentation" . "presentation-types.html#Protocol&nbsp;Class presentation")
   ("presentation-default-preprocessor"
   . "presentation-types.html#Presentation Method presentation-default-preprocessor")
   ("presentation-matches-context-type"
   . "presentation-types.html#Function presentation-matches-context-type")
   ("presentation-modifier"
   . "presentation-types.html#Generic function presentation-modifier")
   ("presentation-object"
   . "presentation-types.html#Generic function presentation-object")
   ("presentationp" . "presentation-types.html#Predicate presentationp")
   ("presentation-refined-position-test"
   . "presentation-types.html#Presentation Method presentation-refined-position-test")
   ("presentation-replace-input"
   . "input-editing.html#Generic function presentation-replace-input")
   ("presentation-single-box"
   . "presentation-types.html#Generic function presentation-single-box")
   ("presentation-subtypep"
   . "presentation-types.html#Function presentation-subtypep")
   ("presentation-subtypep"
   . "presentation-types.html#Presentation Method presentation-subtypep")
   ("presentation-type"
   . "presentation-types.html#Generic function presentation-type")
   ("presentation-type-direct-supertypes"
   . "presentation-types.html#Function presentation-type-direct-supertypes")
   ("presentation-type-history"
   . "presentation-types.html#Presentation Method presentation-type-history")
   ("presentation-type-name"
   . "presentation-types.html#Function presentation-type-name")
   ("presentation-type-of"
   . "presentation-types.html#Function presentation-type-of")
   ("presentation-type-options"
   . "presentation-types.html#Function presentation-type-options")
   ("presentation-typep" . "presentation-types.html#Function presentation-typep")
   ("presentation-typep"
   . "presentation-types.html#Presentation Method presentation-typep")
   ("presentation-type-parameters"
   . "presentation-types.html#Function presentation-type-parameters")
   ("presentation-type-specifier-p"
   . "presentation-types.html#Function presentation-type-specifier-p")
   ("presentation-type-specifier-p"
   . "presentation-types.html#Presentation Method presentation-type-specifier-p")
   ("present-to-string" . "presentation-types.html#Function present-to-string")
   (":pretty-name" . "frames.html#Init&nbsp;arg :pretty-name")
   ("print-menu-item" . "menus.html#Function print-menu-item")
   ("process-interrupt" . "clim-sys.html#Function process-interrupt")
   ("process-name" . "clim-sys.html#Function process-name")
   ("process-next-event" . "silica.html#Generic function process-next-event")
   ("processp" . "clim-sys.html#Predicate processp")
   ("process-state" . "clim-sys.html#Function process-state")
   ("process-wait" . "clim-sys.html#Function process-wait")
   ("process-wait-with-timeout"
   . "clim-sys.html#Function process-wait-with-timeout")
   ("process-whostate" . "clim-sys.html#Function process-whostate")
   ("process-yield" . "clim-sys.html#Function process-yield")
   ("prompt-for-accept-1"
   . "presentation-types.html#Function prompt-for-accept-1")
   ("prompt-for-accept"
   . "presentation-types.html#Generic function prompt-for-accept")
   ("propagate-output-record-changes"
   . "redisplay.html#Generic function propagate-output-record-changes")
   ("propagate-output-record-changes-p"
   . "redisplay.html#Generic function propagate-output-record-changes-p")
   (":properties" . "frames.html#Init&nbsp;arg :properties")
   ("push-button" . "gadgets.html#Class push-button")
   ("push-button-pane" . "gadgets.html#Class push-button-pane")
   ("push-button-show-as-default"
   . "gadgets.html#Generic function push-button-show-as-default")
   ("queue-event" . "silica.html#Generic function queue-event")
   ("queue-repaint" . "silica.html#Generic function queue-repaint")
   ("queue-rescan" . "input-editing.html#Generic function queue-rescan")
   ("radio-box" . "gadgets.html#Class radio-box")
   ("radio-box-current-selection"
   . "gadgets.html#Generic function radio-box-current-selection")
   ("radio-box-pane" . "gadgets.html#Class radio-box-pane")
   ("radio-box-selections"
   . "gadgets.html#Generic function radio-box-selections")
   ("raise-frame" . "frames.html#Generic function raise-frame")
   ("raise-mirror" . "silica.html#Generic function raise-mirror")
   ("raise-sheet" . "silica.html#Generic function raise-sheet")
   ("range-gadget-mixin" . "gadgets.html#Class range-gadget-mixin")
   (":range-label-text-style"
   . "gadgets.html#Init&nbsp;arg :range-label-text-style")
   ("ratio" . "presentation-types.html#Presentation Type ratio")
   ("rational" . "presentation-types.html#Presentation Type rational")
   ("read-bitmap-file" . "extensions.html#Generic function read-bitmap-file")
   ("read-command" . "commands.html#Function read-command")
   ("read-command-using-keystrokes"
   . "commands.html#Function read-command-using-keystrokes")
   ("read-frame-command" . "frames.html#Generic function read-frame-command")
   ("read-gesture" . "extended-input.html#Function read-gesture")
   ("read-token" . "input-editing.html#Function read-token")
   ("real" . "presentation-types.html#Presentation Type real")
   ("realize-mirror" . "silica.html#Generic function realize-mirror")
   ("recompute-contents-ok"
   . "redisplay.html#Generic function recompute-contents-ok")
   ("recompute-extent-for-changed-child"
   . "output-recording.html#Generic function recompute-extent-for-changed-child")
   ("recompute-extent-for-new-child"
   . "output-recording.html#Generic function recompute-extent-for-new-child")
   (":record" . "panes.html#Option :record")
   ("rectangle" . "regions.html#Protocol&nbsp;Class rectangle")
   ("rectangle-edges*" . "regions.html#Generic function rectangle-edges*")
   ("rectangle-height" . "regions.html#Generic function rectangle-height")
   ("rectangle-max-point" . "regions.html#Generic function rectangle-max-point")
   ("rectangle-max-x" . "regions.html#Generic function rectangle-max-x")
   ("rectangle-max-y" . "regions.html#Generic function rectangle-max-y")
   ("rectangle-min-point" . "regions.html#Generic function rectangle-min-point")
   ("rectangle-min-x" . "regions.html#Generic function rectangle-min-x")
   ("rectangle-min-y" . "regions.html#Generic function rectangle-min-y")
   ("rectanglep" . "regions.html#Predicate rectanglep")
   ("rectangle-size" . "regions.html#Generic function rectangle-size")
   ("rectangle-width" . "regions.html#Generic function rectangle-width")
   ("rectilinear-transformation-p"
   . "transforms.html#Generic function rectilinear-transformation-p")
   ("+red+" . "colors.html#Constant +red+")
   ("redisplay" . "redisplay.html#Function redisplay")
   ("redisplayable-stream-p"
   . "redisplay.html#Generic function redisplayable-stream-p")
   ("redisplay-frame-pane" . "frames.html#Generic function redisplay-frame-pane")
   ("redisplay-frame-panes"
   . "frames.html#Generic function redisplay-frame-panes")
   ("redisplay-output-record"
   . "redisplay.html#Generic function redisplay-output-record")
   ("redraw-input-buffer"
   . "input-editing.html#Generic function redraw-input-buffer")
   ("reflection-transformation-p"
   . "transforms.html#Generic function reflection-transformation-p")
   ("reflection-underspecified"
   . "transforms.html#Error reflection-underspecified")
   ("region" . "regions.html#Protocol&nbsp;Class region")
   (":region" . "silica.html#Init&nbsp;arg :region")
   ("region-contains-position-p"
   . "regions.html#Generic function region-contains-position-p")
   ("region-contains-region-p"
   . "regions.html#Generic function region-contains-region-p")
   ("region-difference" . "regions.html#Generic function region-difference")
   ("region-equal" . "regions.html#Generic function region-equal")
   ("region-intersection" . "regions.html#Generic function region-intersection")
   ("region-intersects-region-p"
   . "regions.html#Generic function region-intersects-region-p")
   ("regionp" . "regions.html#Predicate regionp")
   ("region-set" . "regions.html#Protocol&nbsp;Class region-set")
   ("region-set-p" . "regions.html#Predicate region-set-p")
   ("region-set-regions" . "regions.html#Generic function region-set-regions")
   ("region-union" . "regions.html#Generic function region-union")
   ("remove-command-from-command-table"
   . "commands.html#Function remove-command-from-command-table")
   ("remove-keystroke-from-command-table"
   . "commands.html#Function remove-keystroke-from-command-table")
   ("remove-menu-item-from-command-table"
   . "commands.html#Function remove-menu-item-from-command-table")
   ("remove-presentation-translator-from-command-table"
   . "commands.html#Function remove-presentation-translator-from-command-table")
   ("reorder-sheets" . "silica.html#Generic function reorder-sheets")
   ("repaint-sheet" . "silica.html#Generic function repaint-sheet")
   ("replace-input" . "input-editing.html#Generic function replace-input")
   ("replay" . "output-recording.html#Function replay")
   ("replay-output-record"
   . "output-recording.html#Generic function replay-output-record")
   ("rescan-if-necessary"
   . "input-editing.html#Generic function rescan-if-necessary")
   ("reset-frame" . "frames.html#Generic function reset-frame")
   ("reset-scan-pointer"
   . "input-editing.html#Generic function reset-scan-pointer")
   ("resize-sheet" . "silica.html#Generic function resize-sheet")
   ("restart-port" . "silica.html#Generic function restart-port")
   ("restart-process" . "clim-sys.html#Function restart-process")
   ("restraining" . "panes.html#Macro restraining")
   ("restraining-pane" . "panes.html#Layout Pane restraining-pane")
   ("rigid-transformation-p"
   . "transforms.html#Generic function rigid-transformation-p")
   ("row-output-record"
   . "table-formatting.html#Protocol&nbsp;Class row-output-record")
   ("row-output-record-p"
   . "table-formatting.html#Predicate row-output-record-p")
   (":row-wise" . "table-formatting.html#Init&nbsp;arg :row-wise")
   ("run-frame-top-level" . "frames.html#Generic function run-frame-top-level")
   ("run-frame-top-level" . "frames.html#Method :around run-frame-top-level")
   ("scaling-transformation-p"
   . "transforms.html#Generic function scaling-transformation-p")
   ("scroll-bar" . "gadgets.html#Class scroll-bar")
   ("scroll-bar-drag-callback"
   . "gadgets.html#Generic function scroll-bar-drag-callback")
   ("scroll-bar-pane" . "gadgets.html#Class scroll-bar-pane")
   ("scroll-bar-scroll-down-line-callback"
   . "gadgets.html#Generic function scroll-bar-scroll-down-line-callback")
   ("scroll-bar-scroll-down-page-callback"
   . "gadgets.html#Generic function scroll-bar-scroll-down-page-callback")
   ("scroll-bar-scroll-to-bottom-callback"
   . "gadgets.html#Generic function scroll-bar-scroll-to-bottom-callback")
   ("scroll-bar-scroll-to-top-callback"
   . "gadgets.html#Generic function scroll-bar-scroll-to-top-callback")
   ("scroll-bar-scroll-up-line-callback"
   . "gadgets.html#Generic function scroll-bar-scroll-up-line-callback")
   ("scroll-bar-scroll-up-page-callback"
   . "gadgets.html#Generic function scroll-bar-scroll-up-page-callback")
   ("scroll-down-line-callback"
   . "gadgets.html#Callback scroll-down-line-callback")
   (":scroll-down-line-callback"
   . "gadgets.html#Init&nbsp;arg :scroll-down-line-callback")
   ("scroll-down-page-callback"
   . "gadgets.html#Callback scroll-down-page-callback")
   (":scroll-down-page-callback"
   . "gadgets.html#Init&nbsp;arg :scroll-down-page-callback")
   ("scroller-pane" . "panes.html#Service Pane scroller-pane")
   ("scroll-extent" . "panes.html#Generic function scroll-extent")
   ("scrolling" . "panes.html#Macro scrolling")
   ("scroll-to-bottom-callback"
   . "gadgets.html#Callback scroll-to-bottom-callback")
   (":scroll-to-bottom-callback"
   . "gadgets.html#Init&nbsp;arg :scroll-to-bottom-callback")
   ("scroll-to-top-callback" . "gadgets.html#Callback scroll-to-top-callback")
   (":scroll-to-top-callback"
   . "gadgets.html#Init&nbsp;arg :scroll-to-top-callback")
   ("scroll-up-line-callback" . "gadgets.html#Callback scroll-up-line-callback")
   (":scroll-up-line-callback"
   . "gadgets.html#Init&nbsp;arg :scroll-up-line-callback")
   ("scroll-up-page-callback" . "gadgets.html#Callback scroll-up-page-callback")
   (":scroll-up-page-callback"
   . "gadgets.html#Init&nbsp;arg :scroll-up-page-callback")
   ("sequence" . "presentation-types.html#Presentation Type sequence")
   ("sequence-enumerated"
   . "presentation-types.html#Presentation Type sequence-enumerated")
   ("(setf check-box-current-selection)"
   . "gadgets.html#Generic function (setf check-box-current-selection)")
   ("(setf client-setting)"
   . "frames.html#Generic function (setf client-setting)")
   ("(setf command-enabled)"
   . "frames.html#Generic function (setf command-enabled)")
   ("(setf cursor-active)"
   . "extended-output.html#Generic function (setf cursor-active)")
   ("(setf* cursor-position)"
   . "extended-output.html#Generic function (setf* cursor-position)")
   ("(setf cursor-state)"
   . "extended-output.html#Generic function (setf cursor-state)")
   ("(setf cursor-visibility)"
   . "extended-output.html#Generic function (setf cursor-visibility)")
   ("(setf delegate-sheet-delegate)"
   . "silica.html#Generic function (setf delegate-sheet-delegate)")
   ("(setf frame-command-table)"
   . "frames.html#Generic function (setf frame-command-table)")
   ("(setf frame-current-layout)"
   . "frames.html#Generic function (setf frame-current-layout)")
   ("(setf frame-manager)" . "frames.html#Generic function (setf frame-manager)")
   ("(setf frame-pretty-name)"
   . "frames.html#Generic function (setf frame-pretty-name)")
   ("(setf frame-properties)"
   . "frames.html#Generic function (setf frame-properties)")
   ("(setf gadget-client)"
   . "gadgets.html#Generic function (setf gadget-client)")
   ("(setf gadget-id)" . "gadgets.html#Generic function (setf gadget-id)")
   ("(setf gadget-label)" . "gadgets.html#Generic function (setf gadget-label)")
   ("(setf gadget-label-align-x)"
   . "gadgets.html#Generic function (setf gadget-label-align-x)")
   ("(setf gadget-label-align-y)"
   . "gadgets.html#Generic function (setf gadget-label-align-y)")
   ("(setf gadget-max-value)"
   . "gadgets.html#Generic function (setf gadget-max-value)")
   ("(setf gadget-min-value)"
   . "gadgets.html#Generic function (setf gadget-min-value)")
   ("(setf gadget-value)" . "gadgets.html#Generic function (setf gadget-value)")
   ("(setf graph-node-children)"
   . "graph-formatting.html#Generic function (setf graph-node-children)")
   ("(setf graph-node-parents)"
   . "graph-formatting.html#Generic function (setf graph-node-parents)")
   ("(setf graph-root-nodes)"
   . "graph-formatting.html#Generic function (setf graph-root-nodes)")
   ("(setf medium-background)"
   . "drawing-options.html#Generic function (setf medium-background)")
   ("(setf medium-background)"
   . "silica.html#Generic function (setf medium-background)")
   ("(setf medium-buffering-output-p)"
   . "extended-output.html#Generic function (setf medium-buffering-output-p)")
   ("(setf medium-clipping-region)"
   . "drawing-options.html#Generic function (setf medium-clipping-region)")
   ("(setf medium-clipping-region)"
   . "silica.html#Generic function (setf medium-clipping-region)")
   ("(setf medium-default-text-style)"
   . "drawing-options.html#Generic function (setf medium-default-text-style)")
   ("(setf medium-default-text-style)"
   . "silica.html#Generic function (setf medium-default-text-style)")
   ("(setf medium-foreground)"
   . "drawing-options.html#Generic function (setf medium-foreground)")
   ("(setf medium-foreground)"
   . "silica.html#Generic function (setf medium-foreground)")
   ("(setf medium-ink)"
   . "drawing-options.html#Generic function (setf medium-ink)")
   ("(setf medium-ink)" . "silica.html#Generic function (setf medium-ink)")
   ("(setf medium-line-style)"
   . "drawing-options.html#Generic function (setf medium-line-style)")
   ("(setf medium-line-style)"
   . "silica.html#Generic function (setf medium-line-style)")
   ("(setf medium-text-style)"
   . "drawing-options.html#Generic function (setf medium-text-style)")
   ("(setf medium-text-style)"
   . "silica.html#Generic function (setf medium-text-style)")
   ("(setf medium-transformation)"
   . "drawing-options.html#Generic function (setf medium-transformation)")
   ("(setf medium-transformation)"
   . "silica.html#Generic function (setf medium-transformation)")
   ("(setf* output-record-end-cursor-position)"
   . "output-recording.html#Generic function (setf* output-record-end-cursor-position)")
   ("(setf* output-record-position)"
   . "output-recording.html#Generic function (setf* output-record-position)")
   ("(setf* output-record-start-cursor-position)"
   . "output-recording.html#Generic function (setf* output-record-start-cursor-position)")
   ("(setf pane-needs-redisplay)"
   . "frames.html#Generic function (setf pane-needs-redisplay)")
   ("(setf pointer-cursor)"
   . "extended-input.html#Generic function (setf pointer-cursor)")
   ("(setf* pointer-position)"
   . "extended-input.html#Generic function (setf* pointer-position)")
   ("(setf pointer-sheet)"
   . "extended-input.html#Generic function (setf pointer-sheet)")
   ("(setf port-keyboard-input-focus)"
   . "silica.html#Generic function (setf port-keyboard-input-focus)")
   ("(setf port-properties)"
   . "silica.html#Generic function (setf port-properties)")
   ("(setf presentation-object)"
   . "presentation-types.html#Generic function (setf presentation-object)")
   ("(setf presentation-single-box)"
   . "presentation-types.html#Generic function (setf presentation-single-box)")
   ("(setf presentation-type)"
   . "presentation-types.html#Generic function (setf presentation-type)")
   ("(setf radio-box-current-selection)"
   . "gadgets.html#Generic function (setf radio-box-current-selection)")
   ("(setf sheet-enabled-p)"
   . "silica.html#Generic function (setf sheet-enabled-p)")
   ("(setf sheet-region)" . "silica.html#Generic function (setf sheet-region)")
   ("(setf sheet-transformation)"
   . "silica.html#Generic function (setf sheet-transformation)")
   ("(setf stream-current-output-record)"
   . "output-recording.html#Generic function (setf stream-current-output-record)")
   ("(setf* stream-cursor-position)"
   . "extended-output.html#Generic function (setf* stream-cursor-position)")
   ("(setf stream-default-view)"
   . "presentation-types.html#Generic function (setf stream-default-view)")
   ("(setf stream-drawing-p)"
   . "output-recording.html#Generic function (setf stream-drawing-p)")
   ("(setf stream-end-of-line-action)"
   . "extended-output.html#Generic function (setf stream-end-of-line-action)")
   ("(setf stream-end-of-page-action)"
   . "extended-output.html#Generic function (setf stream-end-of-page-action)")
   ("(setf stream-input-buffer)"
   . "extended-input.html#Generic function (setf stream-input-buffer)")
   ("(setf stream-insertion-pointer)"
   . "input-editing.html#Generic function (setf stream-insertion-pointer)")
   ("(setf* stream-pointer-position)"
   . "extended-input.html#Generic function (setf* stream-pointer-position)")
   ("(setf stream-recording-p)"
   . "output-recording.html#Generic function (setf stream-recording-p)")
   ("(setf stream-scan-pointer)"
   . "input-editing.html#Generic function (setf stream-scan-pointer)")
   ("(setf stream-text-cursor)"
   . "extended-output.html#Generic function (setf stream-text-cursor)")
   ("(setf stream-text-margin)"
   . "extended-output.html#Generic function (setf stream-text-margin)")
   ("(setf text-style-mapping)"
   . "text-styles.html#Generic function (setf text-style-mapping)")
   ("(setf* window-viewport-position)"
   . "panes.html#Generic function (setf* window-viewport-position)")
   ("set-highlighted-presentation"
   . "presentation-types.html#Function set-highlighted-presentation")
   (":sheet" . "extended-output.html#Init&nbsp;arg :sheet")
   (":sheet" . "silica.html#Init&nbsp;arg :sheet")
   (":sheet" . "silica.html#Init&nbsp;arg :sheet")
   ("sheet" . "silica.html#Protocol&nbsp;Class sheet")
   ("sheet-adopt-child" . "silica.html#Generic function sheet-adopt-child")
   ("sheet-allocated-region"
   . "silica.html#Generic function sheet-allocated-region")
   ("sheet-ancestor-p" . "silica.html#Generic function sheet-ancestor-p")
   ("sheet-children" . "silica.html#Generic function sheet-children")
   ("sheet-delta-transformation"
   . "silica.html#Generic function sheet-delta-transformation")
   ("sheet-device-region" . "silica.html#Generic function sheet-device-region")
   ("sheet-device-transformation"
   . "silica.html#Generic function sheet-device-transformation")
   ("sheet-direct-mirror" . "silica.html#Generic function sheet-direct-mirror")
   ("sheet-disown-child" . "silica.html#Generic function sheet-disown-child")
   ("sheet-enabled-children"
   . "silica.html#Generic function sheet-enabled-children")
   ("sheet-enabled-p" . "silica.html#Generic function sheet-enabled-p")
   ("sheet-event-queue" . "silica.html#Generic function sheet-event-queue")
   ("sheet-grafted-p" . "silica.html#Generic function sheet-grafted-p")
   ("sheet-identity-transformation-mixin"
   . "silica.html#Class sheet-identity-transformation-mixin")
   ("sheet-leaf-mixin" . "silica.html#Class sheet-leaf-mixin")
   ("sheet-medium" . "silica.html#Generic function sheet-medium")
   ("sheet-mirror" . "silica.html#Generic function sheet-mirror")
   ("sheet-mirrored-ancestor"
   . "silica.html#Generic function sheet-mirrored-ancestor")
   ("sheet-multiple-child-mixin"
   . "silica.html#Class sheet-multiple-child-mixin")
   ("sheet-mute-input-mixin" . "silica.html#Class sheet-mute-input-mixin")
   ("sheet-mute-output-mixin" . "silica.html#Class sheet-mute-output-mixin")
   ("sheet-mute-repainting-mixin"
   . "silica.html#Class sheet-mute-repainting-mixin")
   ("sheet-native-region" . "silica.html#Generic function sheet-native-region")
   ("sheet-native-transformation"
   . "silica.html#Generic function sheet-native-transformation")
   ("sheet-occluding-sheets"
   . "silica.html#Generic function sheet-occluding-sheets")
   ("sheetp" . "silica.html#Predicate sheetp")
   ("sheet-parent" . "silica.html#Generic function sheet-parent")
   ("sheet-parent-mixin" . "silica.html#Class sheet-parent-mixin")
   ("sheet-region" . "silica.html#Generic function sheet-region")
   ("sheet-siblings" . "silica.html#Generic function sheet-siblings")
   ("sheet-single-child-mixin" . "silica.html#Class sheet-single-child-mixin")
   ("sheet-transformation" . "silica.html#Generic function sheet-transformation")
   ("sheet-transformation-mixin"
   . "silica.html#Class sheet-transformation-mixin")
   ("sheet-translation-mixin" . "silica.html#Class sheet-translation-mixin")
   ("sheet-viewable-p" . "silica.html#Generic function sheet-viewable-p")
   ("sheet-with-medium-mixin" . "silica.html#Class sheet-with-medium-mixin")
   ("sheet-y-inverting-transformation-mixin"
   . "silica.html#Class sheet-y-inverting-transformation-mixin")
   ("+shift-key+" . "silica.html#Constant +shift-key+")
   (":show-as-default" . "gadgets.html#Init&nbsp;arg :show-as-default")
   (":show-value-p" . "gadgets.html#Init&nbsp;arg :show-value-p")
   ("shrink-frame" . "frames.html#Generic function shrink-frame")
   ("simple-completion-error"
   . "input-editing.html#Condition simple-completion-error")
   ("simple-parse-error" . "input-editing.html#Error simple-parse-error")
   ("simple-parse-error" . "input-editing.html#Function simple-parse-error")
   (":single-box" . "presentation-types.html#Init&nbsp;arg :single-box")
   ("singular-transformation" . "transforms.html#Error singular-transformation")
   (":size" . "output-recording.html#Init&nbsp;arg :size")
   ("slider" . "gadgets.html#Class slider")
   ("slider-drag-callback"
   . "gadgets.html#Generic function slider-drag-callback")
   ("slider-pane" . "gadgets.html#Class slider-pane")
   ("space-requirement" . "panes.html#Class space-requirement")
   ("space-requirement+" . "panes.html#Function space-requirement+")
   ("space-requirement+*" . "panes.html#Function space-requirement+*")
   ("space-requirement-combine"
   . "panes.html#Function space-requirement-combine")
   ("space-requirement-components"
   . "panes.html#Generic function space-requirement-components")
   ("space-requirement-height"
   . "panes.html#Generic function space-requirement-height")
   ("space-requirement-max-height"
   . "panes.html#Generic function space-requirement-max-height")
   ("space-requirement-max-width"
   . "panes.html#Generic function space-requirement-max-width")
   ("space-requirement-min-height"
   . "panes.html#Generic function space-requirement-min-height")
   ("space-requirement-min-width"
   . "panes.html#Generic function space-requirement-min-width")
   ("space-requirement-width"
   . "panes.html#Generic function space-requirement-width")
   ("spacing" . "panes.html#Macro spacing")
   (":spacing" . "panes.html#Option :spacing")
   ("spacing-pane" . "panes.html#Layout Pane spacing-pane")
   ("*standard-activation-gestures*"
   . "input-editing.html#Variable *standard-activation-gestures*")
   ("standard-application-frame"
   . "frames.html#Class standard-application-frame")
   ("standard-bounding-rectangle"
   . "bboxes.html#Class standard-bounding-rectangle")
   ("standard-cell-output-record"
   . "table-formatting.html#Class standard-cell-output-record")
   ("standard-column-output-record"
   . "table-formatting.html#Class standard-column-output-record")
   ("standard-command-table" . "commands.html#Class standard-command-table")
   ("standard-ellipse" . "regions.html#Class standard-ellipse")
   ("standard-elliptical-arc" . "regions.html#Class standard-elliptical-arc")
   ("standard-encapsulating-stream"
   . "encapsulating-streams.html#Class standard-encapsulating-stream")
   ("standard-extended-input-stream"
   . "extended-input.html#Class standard-extended-input-stream")
   ("standard-extended-output-stream"
   . "extended-output.html#Class standard-extended-output-stream")
   ("standard-graph-node-output-record"
   . "graph-formatting.html#Class standard-graph-node-output-record")
   ("standard-graph-output-record"
   . "graph-formatting.html#Class standard-graph-output-record")
   ("standard-input-editing-stream"
   . "input-editing.html#Class standard-input-editing-stream")
   ("standard-input-stream" . "extended-input.html#Class standard-input-stream")
   ("standard-item-list-output-record"
   . "table-formatting.html#Class standard-item-list-output-record")
   ("standard-line" . "regions.html#Class standard-line")
   ("standard-line-style" . "drawing-options.html#Class standard-line-style")
   ("standard-output-recording-stream"
   . "output-recording.html#Class standard-output-recording-stream")
   ("standard-output-stream"
   . "extended-output.html#Class standard-output-stream")
   ("standard-point" . "regions.html#Class standard-point")
   ("standard-pointer" . "extended-input.html#Class standard-pointer")
   ("standard-polygon" . "regions.html#Class standard-polygon")
   ("standard-polyline" . "regions.html#Class standard-polyline")
   ("standard-presentation"
   . "presentation-types.html#Class standard-presentation")
   ("standard-rectangle" . "regions.html#Class standard-rectangle")
   ("standard-region-difference"
   . "regions.html#Class standard-region-difference")
   ("standard-region-intersection"
   . "regions.html#Class standard-region-intersection")
   ("standard-region-union" . "regions.html#Class standard-region-union")
   ("standard-repainting-mixin" . "silica.html#Class standard-repainting-mixin")
   ("standard-row-output-record"
   . "table-formatting.html#Class standard-row-output-record")
   ("standard-sequence-output-history"
   . "output-recording.html#Class standard-sequence-output-history")
   ("standard-sequence-output-record"
   . "output-recording.html#Class standard-sequence-output-record")
   ("standard-sheet-input-mixin"
   . "silica.html#Class standard-sheet-input-mixin")
   ("standard-sheet-output-mixin"
   . "silica.html#Class standard-sheet-output-mixin")
   ("standard-table-output-record"
   . "table-formatting.html#Class standard-table-output-record")
   ("standard-text-cursor" . "extended-output.html#Class standard-text-cursor")
   ("standard-text-style" . "text-styles.html#Class standard-text-style")
   ("standard-tree-output-history"
   . "output-recording.html#Class standard-tree-output-history")
   ("standard-tree-output-record"
   . "output-recording.html#Class standard-tree-output-record")
   ("standard-updating-output-record"
   . "redisplay.html#Class standard-updating-output-record")
   (":state" . "frames.html#Init&nbsp;arg :state")
   (":stream" . "encapsulating-streams.html#Init&nbsp;arg :stream")
   ("stream-accept" . "presentation-types.html#Generic function stream-accept")
   ("stream-add-character-output"
   . "output-recording.html#Generic function stream-add-character-output")
   ("stream-add-output-record"
   . "output-recording.html#Generic function stream-add-output-record")
   ("stream-add-string-output"
   . "output-recording.html#Generic function stream-add-string-output")
   ("stream-advance-to-column"
   . "extended-output.html#Generic function stream-advance-to-column")
   ("stream-advance-to-column"
   . "gray-streams.html#Generic function stream-advance-to-column")
   ("stream-baseline" . "extended-output.html#Generic function stream-baseline")
   ("stream-character-width"
   . "extended-output.html#Generic function stream-character-width")
   ("stream-clear-input"
   . "extended-input.html#Generic function stream-clear-input")
   ("stream-clear-input"
   . "gray-streams.html#Generic function stream-clear-input")
   ("stream-clear-output"
   . "extended-output.html#Generic function stream-clear-output")
   ("stream-clear-output"
   . "gray-streams.html#Generic function stream-clear-output")
   ("stream-close-text-output-record"
   . "output-recording.html#Generic function stream-close-text-output-record")
   ("stream-current-output-record"
   . "output-recording.html#Generic function stream-current-output-record")
   ("stream-cursor-position"
   . "extended-output.html#Generic function stream-cursor-position")
   ("stream-default-view"
   . "presentation-types.html#Generic function stream-default-view")
   ("stream-drawing-p"
   . "output-recording.html#Generic function stream-drawing-p")
   ("stream-element-type"
   . "gray-streams.html#Generic function stream-element-type")
   ("stream-end-of-line-action"
   . "extended-output.html#Generic function stream-end-of-line-action")
   ("stream-end-of-page-action"
   . "extended-output.html#Generic function stream-end-of-page-action")
   ("stream-finish-output"
   . "extended-output.html#Generic function stream-finish-output")
   ("stream-finish-output"
   . "gray-streams.html#Generic function stream-finish-output")
   ("stream-force-output"
   . "extended-output.html#Generic function stream-force-output")
   ("stream-force-output"
   . "gray-streams.html#Generic function stream-force-output")
   ("stream-fresh-line"
   . "extended-output.html#Generic function stream-fresh-line")
   ("stream-fresh-line" . "gray-streams.html#Generic function stream-fresh-line")
   ("stream-increment-cursor-position"
   . "extended-output.html#Generic function stream-increment-cursor-position")
   ("stream-input-buffer"
   . "extended-input.html#Generic function stream-input-buffer")
   ("stream-input-buffer" . "input-editing.html#Method stream-input-buffer")
   ("stream-input-wait"
   . "extended-input.html#Generic function stream-input-wait")
   ("stream-insertion-pointer"
   . "input-editing.html#Generic function stream-insertion-pointer")
   ("stream-line-column"
   . "extended-output.html#Generic function stream-line-column")
   ("stream-line-column"
   . "gray-streams.html#Generic function stream-line-column")
   ("stream-line-height"
   . "extended-output.html#Generic function stream-line-height")
   ("stream-listen" . "extended-input.html#Generic function stream-listen")
   ("stream-listen" . "gray-streams.html#Generic function stream-listen")
   ("stream-output-history"
   . "output-recording.html#Generic function stream-output-history")
   ("stream-output-history-mixin"
   . "output-recording.html#Class stream-output-history-mixin")
   ("streamp" . "gray-streams.html#Generic function streamp")
   ("stream-pathname" . "gray-streams.html#Generic function stream-pathname")
   ("stream-peek-char" . "extended-input.html#Generic function stream-peek-char")
   ("stream-peek-char" . "gray-streams.html#Generic function stream-peek-char")
   ("stream-pointer-position"
   . "extended-input.html#Generic function stream-pointer-position")
   ("stream-present" . "presentation-types.html#Generic function stream-present")
   ("stream-process-gesture"
   . "input-editing.html#Generic function stream-process-gesture")
   ("stream-read-byte" . "gray-streams.html#Generic function stream-read-byte")
   ("stream-read-char" . "extended-input.html#Generic function stream-read-char")
   ("stream-read-char" . "gray-streams.html#Generic function stream-read-char")
   ("stream-read-char-no-hang"
   . "extended-input.html#Generic function stream-read-char-no-hang")
   ("stream-read-char-no-hang"
   . "gray-streams.html#Generic function stream-read-char-no-hang")
   ("stream-read-gesture"
   . "extended-input.html#Generic function stream-read-gesture")
   ("stream-read-gesture" . "input-editing.html#Method stream-read-gesture")
   ("stream-read-line" . "extended-input.html#Generic function stream-read-line")
   ("stream-read-line" . "gray-streams.html#Generic function stream-read-line")
   ("stream-recording-p"
   . "output-recording.html#Generic function stream-recording-p")
   ("stream-redisplaying-p"
   . "redisplay.html#Generic function stream-redisplaying-p")
   ("stream-replay" . "output-recording.html#Generic function stream-replay")
   ("stream-rescanning-p"
   . "input-editing.html#Generic function stream-rescanning-p")
   ("stream-scan-pointer"
   . "input-editing.html#Generic function stream-scan-pointer")
   ("stream-set-input-focus"
   . "extended-input.html#Generic function stream-set-input-focus")
   ("stream-start-line-p"
   . "extended-output.html#Generic function stream-start-line-p")
   ("stream-start-line-p"
   . "gray-streams.html#Generic function stream-start-line-p")
   ("stream-string-width"
   . "extended-output.html#Generic function stream-string-width")
   ("stream-terpri" . "extended-output.html#Generic function stream-terpri")
   ("stream-terpri" . "gray-streams.html#Generic function stream-terpri")
   ("stream-text-cursor"
   . "extended-output.html#Generic function stream-text-cursor")
   ("stream-text-margin"
   . "extended-output.html#Generic function stream-text-margin")
   ("stream-text-output-record"
   . "output-recording.html#Generic function stream-text-output-record")
   ("stream-truename" . "gray-streams.html#Generic function stream-truename")
   ("stream-unread-char"
   . "extended-input.html#Generic function stream-unread-char")
   ("stream-unread-char"
   . "gray-streams.html#Generic function stream-unread-char")
   ("stream-unread-gesture"
   . "extended-input.html#Generic function stream-unread-gesture")
   ("stream-unread-gesture" . "input-editing.html#Method stream-unread-gesture")
   ("stream-vertical-spacing"
   . "extended-output.html#Generic function stream-vertical-spacing")
   ("stream-write-byte" . "gray-streams.html#Generic function stream-write-byte")
   ("stream-write-char"
   . "extended-output.html#Generic function stream-write-char")
   ("stream-write-char" . "gray-streams.html#Generic function stream-write-char")
   ("stream-write-string"
   . "extended-output.html#Generic function stream-write-string")
   ("stream-write-string"
   . "gray-streams.html#Generic function stream-write-string")
   ("string" . "presentation-types.html#Presentation Type string")
   ("subset" . "presentation-types.html#Presentation Type Abbreveation subset")
   ("subset-alist"
   . "presentation-types.html#Presentation Type Abbreveation subset-alist")
   ("subset-completion"
   . "presentation-types.html#Presentation Type subset-completion")
   ("subset-sequence"
   . "presentation-types.html#Presentation Type Abbreveation subset-sequence")
   ("substitute-numeric-argument-marker"
   . "commands.html#Function substitute-numeric-argument-marker")
   ("suggest" . "input-editing.html#Function suggest")
   ("+super-key+" . "silica.html#Constant +super-key+")
   ("surrounding-output-with-border"
   . "bordered-output.html#Macro surrounding-output-with-border")
   ("symbol" . "presentation-types.html#Presentation Type symbol")
   ("t" . "presentation-types.html#Presentation Type t")
   ("table-output-record"
   . "table-formatting.html#Protocol&nbsp;Class table-output-record")
   ("table-output-record-p"
   . "table-formatting.html#Predicate table-output-record-p")
   ("table-pane" . "panes.html#Layout Pane table-pane")
   ("tabling" . "panes.html#Macro tabling")
   ("temporary-medium-sheet-output-mixin"
   . "silica.html#Class temporary-medium-sheet-output-mixin")
   (":test" . "gadgets.html#Init&nbsp;arg :test")
   (":test" . "gadgets.html#Init&nbsp;arg :test")
   ("test-presentation-translator"
   . "presentation-types.html#Function test-presentation-translator")
   (":text-cursor" . "extended-input.html#Init&nbsp;arg :text-cursor")
   ("text-displayed-output-record"
   . "output-recording.html#Protocol&nbsp;Class text-displayed-output-record")
   ("text-displayed-output-record-p"
   . "output-recording.html#Predicate text-displayed-output-record-p")
   ("text-displayed-output-record-string"
   . "output-recording.html#Generic function text-displayed-output-record-string")
   ("text-editor" . "gadgets.html#Class text-editor")
   ("text-editor-pane" . "gadgets.html#Class text-editor-pane")
   (":text-face" . "text-styles.html#Option :text-face")
   (":text-family" . "text-styles.html#Option :text-family")
   ("text-field" . "gadgets.html#Class text-field")
   ("text-field-pane" . "gadgets.html#Class text-field-pane")
   (":text-margin" . "extended-output.html#Init&nbsp;arg :text-margin")
   (":text-margin" . "panes.html#Option :text-margin")
   ("text-size" . "text-styles.html#Generic function text-size")
   (":text-size" . "text-styles.html#Option :text-size")
   (":text-style" . "drawing-options.html#Option :text-style")
   (":text-style" . "extended-output.html#Init&nbsp;arg :text-style")
   (":text-style" . "panes.html#Option :text-style")
   ("text-style" . "text-styles.html#Protocol&nbsp;Class text-style")
   ("text-style-ascent" . "text-styles.html#Generic function text-style-ascent")
   ("text-style-components"
   . "text-styles.html#Generic function text-style-components")
   ("text-style-descent"
   . "text-styles.html#Generic function text-style-descent")
   ("text-style-face" . "text-styles.html#Generic function text-style-face")
   ("text-style-family" . "text-styles.html#Generic function text-style-family")
   ("text-style-fixed-width-p"
   . "text-styles.html#Generic function text-style-fixed-width-p")
   ("text-style-height" . "text-styles.html#Generic function text-style-height")
   ("text-style-mapping"
   . "text-styles.html#Generic function text-style-mapping")
   ("text-style-p" . "text-styles.html#Predicate text-style-p")
   ("text-style-size" . "text-styles.html#Generic function text-style-size")
   ("text-style-width" . "text-styles.html#Generic function text-style-width")
   ("textual-dialog-view" . "presentation-types.html#Class textual-dialog-view")
   ("+textual-dialog-view+"
   . "presentation-types.html#Constant +textual-dialog-view+")
   ("textual-menu-view" . "presentation-types.html#Class textual-menu-view")
   ("+textual-menu-view+"
   . "presentation-types.html#Constant +textual-menu-view+")
   ("textual-view" . "presentation-types.html#Class textual-view")
   ("+textual-view+" . "presentation-types.html#Constant +textual-view+")
   ("throw-highlighted-presentation"
   . "presentation-types.html#Function throw-highlighted-presentation")
   ("timer-event" . "silica.html#Class timer-event")
   (":timestamp" . "silica.html#Init&nbsp;arg :timestamp")
   ("title-pane" . "panes.html#Service Pane title-pane")
   ("toggle-button" . "gadgets.html#Class toggle-button")
   ("toggle-button-indicator-type"
   . "gadgets.html#Generic function toggle-button-indicator-type")
   ("toggle-button-pane" . "gadgets.html#Class toggle-button-pane")
   ("token-or-type"
   . "presentation-types.html#Presentation Type Abbreveation token-or-type")
   ("tracking-pointer" . "extended-input.html#Macro tracking-pointer")
   (":transformation" . "drawing-options.html#Option :transformation")
   ("transformation" . "transforms.html#Protocol&nbsp;Class transformation")
   ("transformation-equal"
   . "transforms.html#Generic function transformation-equal")
   ("transformation-error" . "transforms.html#Error transformation-error")
   ("transformationp" . "transforms.html#Predicate transformationp")
   ("transformation-underspecified"
   . "transforms.html#Error transformation-underspecified")
   ("transform-distance" . "transforms.html#Generic function transform-distance")
   ("transform-position" . "transforms.html#Generic function transform-position")
   ("transform-rectangle*"
   . "transforms.html#Generic function transform-rectangle*")
   ("transform-region" . "transforms.html#Generic function transform-region")
   ("translation-transformation-p"
   . "transforms.html#Generic function translation-transformation-p")
   ("+transparent-ink+" . "colors.html#Constant +transparent-ink+")
   ("tree-recompute-extent"
   . "output-recording.html#Generic function tree-recompute-extent")
   (":type" . "presentation-types.html#Init&nbsp;arg :type")
   ("type-or-string"
   . "presentation-types.html#Presentation Type Abbreveation type-or-string")
   ("*undefined-text-style*"
   . "text-styles.html#Constant *undefined-text-style*")
   ("unhighlight-highlighted-presentation"
   . "presentation-types.html#Function unhighlight-highlighted-presentation")
   (":unique-id" . "redisplay.html#Init&nbsp;arg :unique-id")
   ("unread-gesture" . "extended-input.html#Function unread-gesture")
   ("*unsupplied-argument-marker*"
   . "commands.html#Variable *unsupplied-argument-marker*")
   ("untransform-distance"
   . "transforms.html#Generic function untransform-distance")
   ("untransform-position"
   . "transforms.html#Generic function untransform-position")
   ("untransform-rectangle*"
   . "transforms.html#Generic function untransform-rectangle*")
   ("untransform-region" . "transforms.html#Generic function untransform-region")
   ("updating-output" . "redisplay.html#Macro updating-output")
   ("updating-output-record"
   . "redisplay.html#Protocol&nbsp;Class updating-output-record")
   ("updating-output-record-p"
   . "redisplay.html#Predicate updating-output-record-p")
   ("user-command-table" . "commands.html#Command Table user-command-table")
   ("using-resource" . "clim-sys.html#Macro using-resource")
   (":value" . "gadgets.html#Init&nbsp;arg :value")
   ("value-changed-callback" . "gadgets.html#Callback value-changed-callback")
   (":value-changed-callback"
   . "gadgets.html#Init&nbsp;arg :value-changed-callback")
   ("value-gadget" . "gadgets.html#Class value-gadget")
   (":value-key" . "gadgets.html#Init&nbsp;arg :value-key")
   (":value-key" . "gadgets.html#Init&nbsp;arg :value-key")
   ("vbox-pane" . "panes.html#Layout Pane vbox-pane")
   ("vertically" . "panes.html#Macro vertically")
   (":vertical-spacing" . "extended-output.html#Init&nbsp;arg :vertical-spacing")
   (":vertical-spacing" . "panes.html#Option :vertical-spacing")
   (":view" . "presentation-types.html#Init&nbsp;arg :view")
   ("view" . "presentation-types.html#Protocol&nbsp;Class view")
   ("viewp" . "presentation-types.html#Predicate viewp")
   ("vrack-pane" . "panes.html#Layout Pane vrack-pane")
   ("+white+" . "colors.html#Constant +white+")
   (":width" . "panes.html#Option :width")
   ("window-clear" . "panes.html#Generic function window-clear")
   ("window-configuration-event"
   . "silica.html#Class window-configuration-event")
   ("window-erase-viewport"
   . "panes.html#Generic function window-erase-viewport")
   ("window-event" . "silica.html#Class window-event")
   ("window-event-mirrored-sheet"
   . "silica.html#Generic function window-event-mirrored-sheet")
   ("window-event-native-region"
   . "silica.html#Generic function window-event-native-region")
   ("window-event-region" . "silica.html#Generic function window-event-region")
   ("window-manager-delete-event"
   . "silica.html#Class window-manager-delete-event")
   ("window-manager-event" . "silica.html#Class window-manager-event")
   ("window-refresh" . "panes.html#Generic function window-refresh")
   ("window-repaint-event" . "silica.html#Class window-repaint-event")
   ("window-viewport" . "panes.html#Generic function window-viewport")
   ("window-viewport-position"
   . "panes.html#Generic function window-viewport-position")
   ("with-accept-help" . "input-editing.html#Macro with-accept-help")
   ("with-activation-gestures"
   . "input-editing.html#Macro with-activation-gestures")
   ("with-application-frame" . "frames.html#Macro with-application-frame")
   ("with-bounding-rectangle*" . "bboxes.html#Macro with-bounding-rectangle*")
   ("with-command-table-keystrokes"
   . "commands.html#Macro with-command-table-keystrokes")
   ("with-delimiter-gestures"
   . "input-editing.html#Macro with-delimiter-gestures")
   ("with-drawing-options" . "drawing-options.html#Macro with-drawing-options")
   ("with-end-of-line-action"
   . "extended-output.html#Macro with-end-of-line-action")
   ("with-end-of-page-action"
   . "extended-output.html#Macro with-end-of-page-action")
   ("with-first-quadrant-coordinates"
   . "drawing-options.html#Macro with-first-quadrant-coordinates")
   ("with-frame-manager" . "frames.html#Macro with-frame-manager")
   ("with-graft-locked" . "silica.html#Macro with-graft-locked")
   ("with-identity-transformation"
   . "drawing-options.html#Macro with-identity-transformation")
   (":within-generation-separation"
   . "graph-formatting.html#Init&nbsp;arg :within-generation-separation")
   ("with-input-context" . "presentation-types.html#Macro with-input-context")
   ("with-input-editing" . "input-editing.html#Macro with-input-editing")
   ("with-input-editor-typeout"
   . "input-editing.html#Macro with-input-editor-typeout")
   ("with-input-focus" . "extended-input.html#Macro with-input-focus")
   ("with-local-coordinates"
   . "drawing-options.html#Macro with-local-coordinates")
   ("with-lock-held" . "clim-sys.html#Macro with-lock-held")
   ("with-look-and-feel-realization"
   . "panes.html#Macro with-look-and-feel-realization")
   ("with-menu" . "menus.html#Macro with-menu")
   ("with-new-output-record"
   . "output-recording.html#Macro with-new-output-record")
   ("with-output-as-gadget" . "gadgets.html#Macro with-output-as-gadget")
   ("with-output-as-presentation"
   . "presentation-types.html#Macro with-output-as-presentation")
   ("with-output-buffered" . "extended-output.html#Macro with-output-buffered")
   ("with-output-recording-options"
   . "output-recording.html#Macro with-output-recording-options")
   ("with-output-to-output-record"
   . "output-recording.html#Macro with-output-to-output-record")
   ("with-output-to-pixmap" . "graphics.html#Macro with-output-to-pixmap")
   ("with-output-to-postscript-stream"
   . "extensions.html#Macro with-output-to-postscript-stream")
   ("without-scheduling" . "clim-sys.html#Macro without-scheduling")
   ("with-port-locked" . "silica.html#Macro with-port-locked")
   ("with-presentation-type-decoded"
   . "presentation-types.html#Macro with-presentation-type-decoded")
   ("with-presentation-type-options"
   . "presentation-types.html#Macro with-presentation-type-options")
   ("with-presentation-type-parameters"
   . "presentation-types.html#Macro with-presentation-type-parameters")
   ("with-radio-box" . "gadgets.html#Macro with-radio-box")
   ("with-recursive-lock-held" . "clim-sys.html#Macro with-recursive-lock-held")
   ("with-room-for-graphics"
   . "extended-output.html#Macro with-room-for-graphics")
   ("with-rotation" . "drawing-options.html#Macro with-rotation")
   ("with-scaling" . "drawing-options.html#Macro with-scaling")
   ("with-sheet-medium" . "silica.html#Macro with-sheet-medium")
   ("with-sheet-medium-bound" . "silica.html#Macro with-sheet-medium-bound")
   ("with-text-face" . "text-styles.html#Macro with-text-face")
   ("with-text-family" . "text-styles.html#Macro with-text-family")
   ("with-text-size" . "text-styles.html#Macro with-text-size")
   ("with-text-style" . "text-styles.html#Macro with-text-style")
   ("with-translation" . "drawing-options.html#Macro with-translation")
   ("write-token" . "input-editing.html#Function write-token")
   (":x" . "silica.html#Init&nbsp;arg :x")
   (":x-position" . "output-recording.html#Init&nbsp;arg :x-position")
   (":x-spacing" . "panes.html#Option :x-spacing")
   (":x-spacing" . "table-formatting.html#Init&nbsp;arg :x-spacing")
   (":x-spacing" . "table-formatting.html#Init&nbsp;arg :x-spacing")
   (":y" . "silica.html#Init&nbsp;arg :y")
   ("+yellow+" . "colors.html#Constant +yellow+")
   (":y-position" . "output-recording.html#Init&nbsp;arg :y-position")
   (":y-spacing" . "panes.html#Option :y-spacing")
   (":y-spacing" . "table-formatting.html#Init&nbsp;arg :y-spacing")
   (":y-spacing" . "table-formatting.html#Init&nbsp;arg :y-spacing")))