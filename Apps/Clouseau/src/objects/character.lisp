;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019-2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Inspection methods for characters.

(cl:in-package #:clouseau)

;;; Object inspection methods

(defmethod inspect-object-using-state ((object character)
                                       (state  inspected-object)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (inspect-class-as-name (class-of object) stream)
  (write-char #\Space stream)
  (call-next-method))

(defmethod inspect-object-using-state :after ((object character)
                                              (state  inspected-object)
                                              (style  (eql :badges))
                                              (stream t))
  (macrolet ((attribute (predicate label)
               `(when (,predicate object)
                  (write-char #\Space stream)
                  (badge stream ,label))))
    (attribute graphic-char-p "graphic")
    (attribute digit-char-p   "digit")
    (attribute alpha-char-p   "alpha")
    (attribute alphanumericp  "alphanumeric")
    (attribute upper-case-p   "upper-case")
    (attribute lower-case-p   "lower-case")))

(defmethod inspect-object-using-state ((object character)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (formatting-table (stream)
    (format-place-row stream object 'deep-reader-place 'char-name :label "Name")
    (format-place-row stream object 'deep-reader-place 'char-code :label "Code")
    (when-let ((weight (digit-char-p object)))
      (format-place-row stream object 'pseudo-place weight :label "Weight"))))

;;; No circularity tracking for characters.
(defmethod note-object-occurrence ((object       character)
                                   (state        inspected-object)
                                   (presentation t)
                                   (stream       t)))
