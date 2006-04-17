;; FIXME: clim-cairo had more fixes here

(in-package :climi)

(defclass out-compositum (design)
  ((ink  :initarg :ink  :reader compositum-ink)
   (mask :initarg :mask :reader compositum-mask)))

(defmethod print-object ((object out-compositum) stream)
  (print-unreadable-object (object stream :identity nil :type t)
    (format stream "~S ~S ~S ~S"
            :ink (compositum-ink object)
            :mask (compositum-mask object))))

(defmethod compose-out ((ink design) (mask design))
  (make-instance 'out-compositum
    :ink ink
    :mask mask))

(defmethod transform-region (transformation (design design))
  (make-instance 'transformed-design
                 :transformation transformation
                 :design design))

(defmethod clim:handle-repaint :after ((s clim:sheet-with-medium-mixin) r)
  (medium-force-output (sheet-medium s)))
