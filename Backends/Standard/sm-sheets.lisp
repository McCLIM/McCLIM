(in-package :clim-standard)


(defclass standard-single-mirrored-sheet-mixin (standard-mirrored-sheet-mixin)
    ())

(defmethod sheet-native-transformation ((sheet standard-single-mirrored-sheet-mixin))
  (with-slots (native-transformation) sheet
    (unless native-transformation
      (setf native-transformation +identity-transformation+))
    native-transformation))
