(in-package :clim-gtk)

(defclass gtk-frame-manager (frame-manager)
  ())

(defmethod adopt-frame :after ((fm gtk-frame-manager) (frame application-frame))
  (log:info "fm=~s frame=~s" fm frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    nil))

(defmethod note-space-requirements-changed :after ((graft gtk-graft) pane)
  ())

(defmethod find-concrete-pane-class ((fm gtk-frame-manager) pane-type &optional errorp)
  (if (eq pane-type 'climi::top-level-sheet-pane)
      (find-class 'gtk-top-level-sheet-pane)
      (call-next-method)))
