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

;;; Places

;;; `package-data-place-mixin'

(defclass package-data-place-mixin ()
  ())

(defmethod supportsp ((place     package-data-place-mixin)
                      (operation (eql 'modify-descendants)))
  nil)

(defmethod supportsp :around ((place     package-data-place-mixin)
                              (operation (eql 'setf)))
  (and (not (package-locked-p (container place)))
       (call-next-method)))

;;; `package-name-place'

(defclass package-name-place (package-data-place-mixin
                              basic-place)
  ())

(defmethod accepts-value-p ((place package-name-place) (value t))
  (typep value '(or character string symbol)))

(defmethod value ((place package-name-place))
  (package-name (container place)))

(defmethod (setf value) ((new-value t) (place package-name-place))
  (let ((package (container place)))
    (rename-package package new-value (package-nicknames package))))

;;; `package-nicknames-place'

(defclass package-nicknames-place (package-data-place-mixin
                                   basic-place)
  ())

(defmethod accepts-value-p ((place package-nicknames-place) (value t))
  (and (alexandria:proper-list-p value)
       (every (alexandria:of-type '(or character string symbol)) value)))

(defmethod value ((place package-nicknames-place))
  (package-nicknames (container place)))

(defmethod (setf value) ((new-value t) (place package-nicknames-place))
  (let ((package (container place)))
    (rename-package package (package-name package) new-value)))

(defmethod object-state-class ((object null) (place package-nicknames-place))
  'inspected-symbol-list)

(defmethod object-state-class ((object cons) (place package-nicknames-place))
  'inspected-symbol-list)

;;; `package-list-place-mixin'

(defclass package-list-place-mixin ()
  ())

(defmethod object-state-class ((object null) (place package-list-place-mixin))
  'inspected-package-list)

(defmethod object-state-class ((object cons) (place package-list-place-mixin))
  'inspected-package-list)

;;; `package-use-list-place'

(defclass package-use-list-place (package-data-place-mixin
                                  package-list-place-mixin
                                  basic-place)
  ())

(defmethod accepts-value-p ((place package-use-list-place)
                            (value t))
  (and (alexandria:proper-list-p value)
       (every (alexandria:of-type 'package) value)))

(defmethod value ((place package-use-list-place))
  (package-use-list (container place)))

(defmethod (setf value) ((new-value list) (place package-use-list-place))
  (let* ((package   (container place))
         (old-value (value place))
         (added     (set-difference new-value old-value :test #'eq))
         (removed   (set-difference old-value new-value :test #'eq)))
    (use-package added package)
    (unuse-package removed package)))

;;; `package-used-by-list-place'

(defclass package-used-by-list-place (package-data-place-mixin
                                      package-list-place-mixin
                                      read-only-place)
  ())

(defmethod value ((place package-used-by-list-place))
  (package-used-by-list (container place)))

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

;;; `inspected-package-list'

(defclass inspected-package-list (inspected-proper-list)
  ())

;;; `inspected-symbol-list'

(defclass inspected-symbol-list (inspected-proper-list)
  ())

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
        (format-place-cells stream object 'package-name-place nil
                            :label "Name")
        (format-place-cells stream object 'package-nicknames-place nil
                            :label "Nicknames")
        #+sbcl (format-place-cells stream object 'reader-place 'package-locked-p
                                   :label "Locked"))
      (formatting-row (stream)
        (format-place-cells stream object 'package-use-list-place nil
                            :label "Uses")
        (format-place-cells stream object 'package-used-by-list-place nil
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
