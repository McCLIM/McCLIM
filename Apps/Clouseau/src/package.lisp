;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018-2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Package definition for the Clouseau inspector application.
;;;

(cl:defpackage #:clouseau
  (:use
   #:clim-lisp
   #:alexandria
   #:clim)

  (:shadow
   #:inspect)

  (:shadowing-import-from #:alexandria
   #:simple-parse-error)

  ;; Formatting utilities
  (:export
   #:call-with-preserved-cursor-x      #:with-preserved-cursor-x
   #:call-with-preserved-cursor-y      #:with-preserved-cursor-y

   #:with-style
   #:call-with-section                 #:with-section
   #:call-with-placeholder-if-empty    #:with-placeholder-if-empty
   #:call-with-output-as-badge         #:with-output-as-badge
   #:badge

   #:call-with-safe-and-terse-printing #:with-safe-and-terse-printing
   #:call-with-error-handling          #:with-error-handling
                                       #:with-print-error-handling)

  ;; Place formatting utilities
  (:export
   #:formatting-place
   #:format-place-cells
   #:format-place-row)

  ;; Place protocol and classes
  (:export
   #:parent
   #:root
   #:children
   #:ensure-child

   #:container
   #:cell
   #:state
   #:ensure-state

   #:supportsp
   #:accepts-value-p
   #:valuep
   #:value                          ; also setf
   #:remove-value

   #:basic-place
   #:read-only-place
   #:sequence-element-place
   #:key-value-place
   #:key-place
   #:value-place
   #:function-backed-place
   #:reader-place
   #:accessor-place
   #:pseudo-place)

  ;; Object state protocol and class
  (:export
   #:place
   #:object
   #:state-applicable-p

   #:object-state-class
   #:make-object-state

   #:inspected-object
   #:inspected-integer
   #:inspected-list
   #:inspected-proper-list
   #:inspected-plist
   #:inspected-alist
   #:inspected-improper-list
   #:inspected-array
   #:inspected-vector
   #:inspected-instance
   #:inspected-hash-table
   #:inspected-function
   #:inspected-class
   #:inspected-method
   #:inspected-generic-function
   #:inspected-sequence)

  ;; Object inspection protocol
  (:export
   #:call-with-root-place
   #:inspect-place
   #:inspect-object
   #:inspect-object-using-state

   #:note-object-occurrence
   #:call-without-noting-occurrences
   #:without-noting-occurrences)

  ;; Inspector state protocol
  (:export
   #:root-place                     ; also setf
   #:root-object                    ; also setf
   #:change-hook
   #:present-inspected-object-graph)

  ;; Command table
  (:export
   #:inspector-command-table)

  ;; Inspector pane protocol and classes
  (:export
   #:queue-redisplay

   #:inspector-pane-mixin
   #:inspector-pane

   #:inspector-pane-command-table)

  ;; User interface
  (:export
   #:inspect))
