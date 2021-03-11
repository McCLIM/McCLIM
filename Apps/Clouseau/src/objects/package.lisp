;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018-2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Places, inspection methods and commands for packages.

(cl:in-package #:clouseau)

;;; Object states

;;; `inspected-package'

(defclass inspected-package (inspected-instance)
  ((%symbol-filter :initarg  :symbol-filter
                   :accessor symbol-filter
                   :initform nil))
  (:default-initargs
   :slot-style nil))

(defmethod (setf symbol-filter) :after ((new-value t)
                                        (object    inspected-package))
  ())

(defmethod object-state-class ((object package) (place t))
  'inspected-package)

;;; Object inspection methods

(defun package-symbols (package &key filter)
  (let ((result (make-array 100 :adjustable t :fill-pointer 0)))
    (do-external-symbols (symbol package)
      (when (and (eq (symbol-package symbol) package)
                 (or (not filter)
                     (funcall filter symbol)))
        (vector-push-extend symbol result)))
    (sort result #'string-lessp :key #'symbol-name)))

(defmethod inspect-object-using-state :after ((object package)
                                              (state  inspected-object)
                                              (style  (eql :badges))
                                              (stream t))
  (when (package-locked-p object)
    (write-char #\Space stream)
    (badge stream "locked")))

;; TODO style symbols grouped by external etc.
(defmethod inspect-object-using-state ((object package)
                                       (state  inspected-package)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (with-preserved-cursor-x (stream)
    (formatting-table (stream)
      (formatting-row (stream)
        (format-place-cells stream object 'reader-place 'package-name
                            :label "Name")
        (format-place-cells stream object 'reader-place 'package-nicknames
                            :label "Nicknames")
        #+sbcl (format-place-cells stream object 'reader-place 'package-locked-p
                                   :label "Locked"))
      (formatting-row (stream)
        (format-place-cells stream object 'reader-place 'package-use-list
                            :label "Uses")
        (format-place-cells stream object 'reader-place 'package-used-by-list
                            :label "Used by"))
      #+sbcl (format-place-row stream object 'reader-place 'sb-ext:package-local-nicknames
                               :label "Local nicknames")))

  (print-documentation object stream)

  ;; Slots (not displayed by default)
  (call-next-method)

  ;; Symbols
  (with-section (stream) "Symbols"
    (with-drawing-options (stream :text-size :smaller)
      (formatting-table (stream)
        (formatting-header (stream) "Symbol" "Value" "Function" "Type")

        (flet ((symbol-row (symbol)
                 (formatting-row (stream)
                   (formatting-place (object 'pseudo-place symbol nil inspect* :place-var place)
                     (formatting-cell (stream) (inspect* stream))
                     ;; Value slot
                     (formatting-place (symbol 'symbol-value-place nil present inspect)
                       (formatting-cell (stream) (present stream) (inspect stream)))
                     ;; Function slot
                     (formatting-place (symbol 'symbol-function-place nil present inspect)
                       (formatting-cell (stream) (present stream) (inspect stream)))
                     ;; Type slot
                     (formatting-place (symbol 'symbol-type-place nil present inspect)
                       (formatting-cell (stream) (present stream) (inspect stream)))))))
          (map nil #'symbol-row (package-symbols object :filter (symbol-filter state))))))))

;; TODO command: trace all symbols
