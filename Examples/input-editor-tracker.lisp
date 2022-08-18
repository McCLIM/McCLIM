(in-package #:clim-user)

;;; The input editor has four coursors:
;;;
;;; * open-cursor - beginning of the current input editing context
;;; * scan-cursor - input position for reading
;;; * edit-cursor - input position for writing
;;; * fill-cursor - end of the current input editing context
;;;
;;; OPEN-CURSOR may seem redundant - we could take a beginning of the buffer,
;;; but the input editor allows recursive editing. In that case the rescan
;;; operation repositions the SCAN-CURSOR to the OPEN-CURSOR.
;;;
;;; FILL-CURSOR also seems redundant - it could be the buffer length. Even more
;;; so because the rescan operation may invalidate the parse result of the rest
;;; of the buffer. FILL-CURSOR may be used to implement the structural editor,
;;; where input is a tree, and we want to edit a single token.
;;;
;;; Editing the buffer may move cursors accordingly. Each cursor attached to a
;;; line has a position. When an item is inserted (or the line is split) at the
;;; cursor position, then when it is "left-sticky" then its position is left
;;; unchanged. When it is "right-sticky" then its position is incremented.
;;;
;;; OPEN-CURSOR and SCAN-CURSOR are "left-sticky", EDIT-CURSOR and FILL-CURSOR
;;; are "right-sticky". This reflects the dichotomy of the desired effect when
;;; we write vs when we read the input.
;;;
;;; All cursors should be always positioned between the OPEN-CURSOR and the
;;; FILL-CURSOR. Rescan operation resets the SCAN-CURSOR to OPEN-CURSOR and
;;; reading elements follow up until it is equal to the FILL-CURSOR.
;;;
;;; Editing the buffer triggers the rescan operation if the EDIT-CURSOR is
;;; before the SCAN-CURSOR. Otherwise there is no such need, because the change
;;; was not parsed yet by the editor client (the scan operation is in progress).
;;; Typically edit operations won't be processed during the rescan (so it may be
;;; reasonable to assume, that all editing operations trigger the rescan).

;;; This demo visualizes all cursors while editing and presents a few possible
;;; input editor applications.

(define-application-frame input-editor-tracker ()
  ((editor :initarg :editor :accessor editor))
  (:pane :application))

(defun show (what)
  (declare (notinline show))
  (format *debug-io* "Read: ~s~%" what))

(define-input-editor-tracker-command (com-edit-something :menu t)
    ()
  (with-activation-gestures (*standard-activation-gestures* :override t)
    (with-input-editing (*standard-input* :initial-content "Some input...")
      (with-activation-gestures (#\space :override nil)
        (format *debug-io* "about to read with space!~%")
        (show
         (with-input-editing (*standard-input*)
           (loop
             (format *debug-io* "reading next gesture!~%")
             (read-gesture))))
        (format *debug-io* "done reading with space!"))
      (format *debug-io* "reading outside!")
      (loop (read-gesture)))))

(find-application-frame 'input-editor-tracker)
