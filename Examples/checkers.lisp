;;; ---------------------------------------------------------------------------
;;;   License: BSD-2-Clause.
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; A game of checkers which demonstrates drawing functions, presentations,
;;; translators and commands.
;;;

(defpackage #:clim-demo.checkers
  (:use #:clim-lisp #:clim)
  (:export #:run-checkers #:clim-checkers)
  (:import-from #:alexandria
                #:if-let #:when-let #:when-let* #:iota))
(in-package #:clim-demo.checkers)

(defparameter *square-side* 72)

(defparameter *cols* 8)
(defparameter *rows* 8)

(defparameter *col-labels*
  (mapcar #'princ-to-string (iota *cols* :start 1)))

(defparameter *row-labels*
  (map 'list #'princ-to-string
       (if (> *rows* 26)
           (alexandria:iota *rows* :start 1)
           (subseq "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 0 *rows*))))

(defun make-default-board ()
  (let ((array (make-array (list *rows* *cols*))))
    (dotimes (row *rows*)
      (dotimes (col *cols*)
        (let ((field (make-instance 'field :row row :col col)))
          (setf (aref array row col) field)
          (when-let ((checker
                      (cond ((and (< row 2)
                                  (zerop (mod (+ row col) 2)))
                             (make-instance 'checker :player :player-1))
                            ((and (>= row (- *rows* 2))
                                  (zerop (mod (+ row col) 2)))
                             (make-instance 'checker :player :player-2)))))
            (setf (field checker) field
                  (checker field) checker)))))
    array))

(defclass checker ()
  ((player :initarg :player :reader player)
   (queenp :initarg :queen  :accessor queenp)
   (field  :initarg :field  :accessor field))
  (:default-initargs :queen nil))

(defclass field ()
  ((col :initarg :col :reader col)
   (row :initarg :row :reader row)
   (checker :initform nil :accessor checker)))

(defmethod (setf checker) :before ((val checker) (field field))
  (setf (checker (field val)) nil
        (field val) field))

(defmethod (setf checker) :before ((val null) (field field))
  (when-let ((checker (checker field)))
    (setf (field checker) nil)))

(defclass game-state ()
  ((board  :initarg :board  :reader board)
   (player :initarg :player :accessor player)
   (locked :initarg :locked :accessor locked
           :documentation "Checker locked to take others.")
   (over-field   :initarg :over-field   :accessor over-field)
   (active-field :initarg :active-field :accessor active-field)
   (game-over :initarg :game-over :accessor game-over)
   (%takesp :reader takesp))
  (:default-initargs :board (make-default-board)
                     :player :player-1
                     :locked nil
                     :over-field nil
                     :active-field nil
                     :game-over nil))

(defun switch-player (game-state)
  (setf (locked game-state) nil)
  (setf (active-field game-state) nil)
  (setf (player game-state)
        (ecase (player game-state)
          (:player-1 :player-2)
          (:player-2 :player-1))))

(defun new-game (game-state)
  (with-slots (board player locked over-field active-field game-over)
      game-state
    (setf board (make-default-board)
          player :player-1
          locked nil
          over-field nil
          active-field nil
          game-over nil)
    (slot-makunbound game-state '%takesp)))

(defun get-field (game-state row col)
  (unless (integerp row)
    (setf row (position row *row-labels* :test #'string=)))
  (let ((array (board game-state)))
    (when (array-in-bounds-p array row col)
      (aref array row col))))

(defun preys (game-state checker)
  "Returns a list of prey checkers (or NIL) in each direction."
  (let ((row (row (field checker)))
        (col (col (field checker))))
    (flet ((find-prey (rd cd)
             (do* ((row  #1=(+ row rd)                     #1#)
                   (col  #2=(+ col cd)                     #2#)
                   (this #3=(get-field game-state row col) #3#)
                   (next #4=(get-field game-state #1# #2#) #4#)
                   (prey #5=(and this (checker this))      #5#)
                   (blck #6=(or (not next) (checker next)) #6#))
                  ((null this))
               (when prey
                 (return-from find-prey
                   (if (and (not blck)
                            (not (eql (player checker)
                                      (player prey))))
                       prey
                       nil)))
               (unless (queenp checker)
                 (return-from find-prey nil)))))
      (list (find-prey -1 -1)
            (find-prey -1 +1)
            (find-prey +1 -1)
            (find-prey +1 +1)))))

;;; this could be cached per generation (tester may call valid-move-p
;;; repeatedly for sake of highlighting).
(defun possible-moves (game-state checker &aux moves)
  "Returns valid moves and whenever they involve taking a checker."
  (destructuring-bind (p1 p2 p3 p4)
      (preys game-state checker)
    (flet ((find-dest (row col rd cd)
             (do* ((row  #1=(+ row rd)                     #1#)
                   (col  #2=(+ col cd)                     #2#)
                   (this #3=(get-field game-state row col) #3#))
                  ((or (null this)
                       (checker this)))
               (push this moves)
               (unless (queenp checker)
                 (return-from find-dest nil)))))
      (when p1 (find-dest (+ (row (field p1))) (+ (col (field p1))) -1 -1))
      (when p2 (find-dest (+ (row (field p2))) (+ (col (field p2))) -1 +1))
      (when p3 (find-dest (+ (row (field p3))) (+ (col (field p3))) +1 -1))
      (when p4 (find-dest (+ (row (field p4))) (+ (col (field p4))) +1 +1))
      (unless (or p1 p2 p3 p4)
        (let* ((field (field checker))
               (row (row field))
               (col (col field)))
          (when (or (queenp checker)
                    (eql :player-1 (player checker)))
            (find-dest row col +1 -1)
            (find-dest row col +1 +1))
          (when (or (queenp checker)
                    (eql :player-2 (player checker)))
            (find-dest row col -1 -1)
            (find-dest row col -1 +1)))))
    (values moves (not (null (or p1 p2 p3 p4))))))

;;; this could be cached per generation (tester may call valid-move-p
;;; repeatedly for sake of highlighting).
(defun takes-checker-p (game-state)
  "Can `player' take opponent's checker?"
  (dotimes (row *rows*)
    (dotimes (col *cols*)
      (when-let* ((field (get-field game-state row col))
                  (checker (checker field))
                  (owner-p (eql (player game-state) (player checker)))
                  (takes-p (nth-value 1 (possible-moves game-state checker))))
        (return-from takes-checker-p t)))))

(defmethod slot-unbound (class (instance game-state) (slot (eql '%takesp)))
  (setf (slot-value instance slot)
        (takes-checker-p instance)))

(defun winnerp (game-state)
  (let ((player-1-pawns nil)
        (player-2-pawns nil))
    (dotimes (row *rows*)
      (dotimes (col *cols*)
        (when-let ((checker (checker (get-field game-state row col))))
          (ecase (player checker)
            (:player-1 (setf player-1-pawns t))
            (:player-2 (setf player-2-pawns t)))
          (when (and player-1-pawns player-2-pawns)
            (return-from winnerp nil)))))
    (cond ((not player-1-pawns) :player-2)
          ((not player-2-pawns) :player-1))))

(defun checker-movable-p (game-state checker)
  (and (eql (player game-state)
            (player checker))
       (member (locked game-state)
               (list checker nil))))

(defun valid-move-p (game-state checker dst-field)
  "Second value indicates if taking move is possible for the checker."
  (unless (checker-movable-p game-state checker)
    (return-from valid-move-p nil))
  (multiple-value-bind (moves takesp)
      (possible-moves game-state checker)
    (if (and (member dst-field moves)
             (or takesp (not (takesp game-state))))
        (values dst-field takesp)
        (values nil takesp))))

(defun move-checker (game-state checker dst-field &aux (src-field (field checker)))
  (unless (valid-move-p game-state checker dst-field)
    (format *debug-io* "Invalid move, igorning.~%")
    (return-from move-checker))
  (slot-makunbound game-state '%takesp)
  (do* ((row-sig (signum (- (row dst-field)  (row src-field))))
        (col-sig (signum (- (col dst-field)  (col src-field))))
        (mid-row (+ (row src-field) row-sig) (+ mid-row row-sig))
        (mid-col (+ (col src-field) col-sig) (+ mid-col col-sig))
        (takenp nil))
       ((or takenp (= mid-row (row dst-field)))
        ;; Move the checker
        (setf (checker dst-field) checker
              (active-field game-state) nil)
        ;; If we took a checker and after the move we still can then
        ;; there is is no promotion, checker is locked to move and we
        ;; do not switch player.
        (if (and takenp (some #'identity (preys game-state checker)))
            (setf (locked game-state) checker
                  (active-field game-state) dst-field)
            (progn
              (cond
                ;; Promote checker and carry on
                ((and (eql :player-1 (player checker))
                      (= (row dst-field) (1- *rows*)))
                 (setf (queenp checker) t))
                ((and (eql :player-2 (player checker))
                      (= (row dst-field) 0))
                 (setf (queenp checker) t)))
              (switch-player game-state))))
    ;; Remove the interim checker
    (when-let* ((mid-field (get-field game-state mid-row mid-col))
                (checker (checker mid-field)))
      (setf (checker mid-field) nil
            ;; flag that move may continue
            takenp t))))


(defmethod activep ((field field))
  (and (boundp '*application-frame*)
       (eql field (active-field *application-frame*))))

(defclass board () ())
(defclass board-view (gadget-view) ())

(defun player-color (player-name)
  (ecase player-name
    (:player-1 +dark-red+)
    (:player-2 +black+)))

(define-presentation-method present
    ((checker checker) (type checker) stream (view board-view) &key)
  (when-let* ((field (field checker))
              (x (+ (col field) .5))
              (y (+ (row field) .5))
              (ink (player-color (player checker))))
    (draw-circle* stream x y .4 :ink ink)
    (when (queenp checker)
      (draw-circle* stream x y .2
                    :ink +grey+
                    :filled nil
                    :line-thickness 5
                    :line-unit :normal
                    ;; XXX: bug in clx backend: too big scaling and
                    ;; :coordinate unit lands us in a debugger.

                    ;; XXX: bug in clim core: line-unit is not
                    ;; recorded. Repaint sheet with custom unit to have it
                    ;; default unit style.

                    ;; :line-thickness 0.1
                    ;; :line-unit :coordinate
                    ))))

(define-presentation-method present
    ((field field) (type field) stream (view board-view) &key)
  (let* ((x (col field))
         (y (row field))
         (ink (cond ((eql field (active-field *application-frame*))
                     +grey42+)
                    ((zerop (mod (+ x y) 2))
                     +dark-gray+)
                    (t
                     +light-grey+))))
    (draw-rectangle* stream x y (1+ x) (1+ y) :ink ink)
    (when-let ((checker (checker field)))
      (present checker 'checker :stream stream :view view))))

(define-presentation-method present (object (type board) stream (view board-view) &key)
  ;; Testing (player turn marker)
  (ecase (player *application-frame*)
    (:player-1 (draw-circle* stream 0.75 0.75 0.1))
    (:player-2 (draw-circle* stream 0.75 (+ *rows* 1.25) 0.1))
    (:setup    (draw-circle* stream (+ *cols* 1.25) (+ *rows* 1.25) 0.1)))
  ;; Draw vertical coordinates
  (loop
     for col from 1
     for col-name in *col-labels*
     for local-x1 = (+ col .5)
     for local-y1 = 0.75
     for local-y2 = (+ (1+ *rows*) .25)
     do
       (draw-text* stream col-name local-x1 local-y1
                   :align-x :center
                   :align-y :center)
       (multiple-value-bind (native-x native-y)
           (transform-position (sheet-device-transformation stream) local-x1 local-y2)
         (with-identity-transformation (stream)
           (draw-text* stream col-name native-x native-y
                       :align-x :center
                       :align-y :center
                       :transformation (make-rotation-transformation* pi native-x native-y)
                       :transform-glyphs t))))
  ;; Draw horizontal coordinates
  (loop
     for row from 1
     for row-name in *row-labels*
     for local-x1 = 0.75
     for local-x2 = (+ (1+ *cols*) .25)
     for local-y1 = (+ row .5)
     do
       (draw-text* stream row-name local-x1 local-y1
                   :align-x :center
                   :align-y :center)
       (multiple-value-bind (native-x native-y)
           (transform-position (sheet-device-transformation stream) local-x2 local-y1)
         (with-identity-transformation (stream)
           (draw-text* stream row-name native-x native-y
                       :align-x :center
                       :align-y :center
                       :transformation (make-rotation-transformation* pi native-x native-y)
                       :transform-glyphs t))))

  (with-translation (stream 1 1)
    (if-let ((winner (game-over *application-frame*)))
      (let ((x (/ *cols* 2))
            (y (/ *rows* 2)))
        (draw-circle* stream x y 2 :ink (player-color winner)))
      (dotimes (row *rows*)
        (dotimes (col *cols*)
          ;; slow down to test with: incremental redisplay,
          ;; double-buffering, medium-buffering and such.
          ;(sleep 0.001)
          (present (get-field *application-frame* row col) 'field :view view))))))

(define-application-frame clim-checkers (standard-application-frame game-state)
  ()
  (:pane :application
   :display-function 'display
   ;; :incremental-redisplay t
   :scroll-bars nil
   :width  (* (1+ *cols*) *square-side*)
   :min-width  (* (1+ *cols*) *square-side*)
   :max-width  (* (1+ *cols*) *square-side*)
   :height (* (1+ *rows*) *square-side*)
   :min-height (* (1+ *rows*) *square-side*)
   :max-height (* (1+ *rows*) *square-side*)
   :default-view (make-instance 'board-view))
  (:menu-bar nil))

(defun display (frame pane)
  (with-first-quadrant-coordinates (pane 0 (* (1+ *rows*) *square-side*))
    (with-scaling (pane *square-side* *square-side*)
      (with-translation (pane -.5 -.5)
        (setf (game-over frame) (winnerp frame))
        (present nil 'board :stream pane :single-box t)))))

(defun test-checker (object &rest args)
  (declare (ignore args))
  (eql (player object)
       (player *application-frame*)))

(defun test-field (dst-field &rest args)
  (declare (ignore args))
  (when-let* ((game-state *application-frame*)
              (src-field (active-field game-state))
              (checker (checker src-field))
              (valid-p (valid-move-p game-state checker dst-field)))
    t))

(define-command (com-select-checker :name t :command-table clim-checkers)
    ((checker checker :gesture nil))
  (let ((frame *application-frame*)
        (field (field checker)))
    (setf (active-field frame) (if (eql field (active-field frame))
                                   nil
                                   field))))

(define-command (com-select-field :name t :command-table clim-checkers)
    ((dst-field field :gesture (:select :tester test-field)))
  (let* ((frame *application-frame*)
         (src-field (active-field frame))
         (checker (checker src-field)))
    (move-checker frame checker dst-field)))

(define-command (com-move-checker :name t :command-table clim-checkers)
    ((checker checker) (field field))
  (move-checker *application-frame* checker field))

(define-command (com-new-game :name t :command-table clim-checkers) ()
  (new-game *application-frame*))

(define-drag-and-drop-translator tr-move-checker
    (checker command (or field checker) clim-checkers
             :feedback (lambda (frame presentation stream x0 y0 x1 y1 state)
                         (declare (ignore frame))
                         (case state
                           (:highlight
                            ;; Delay feedback until the pointer has
                            ;; moved a bit. This avoids the feedback
                            ;; icon popping up very briefly when the
                            ;; users clicks to selects.
                            (when (> (sqrt (+ (expt (- x1 x0) 2) (expt (- y1 y0) 2))) 8)
                              (let* ((checker (presentation-object presentation))
                                     (ink (player-color (player checker))))
                                (with-output-recording-options (stream :draw t :record nil)
                                  (draw-circle* stream x1 y1 32 :ink ink)))))
                           (:unhighlight
                            (repaint-sheet stream (make-rectangle*
                                                   (- x1 32)
                                                   (- y1 32)
                                                   (+ x1 32)
                                                   (+ y1 32))))))
             :tester
             ((object) ; XXX: fix-args should allow i.e &rest args
              (let ((frame *application-frame*))
                (and (checker-movable-p frame object)
                     (multiple-value-bind (moves takep)
                         (possible-moves frame object)
                       (and moves (or takep (not (takesp frame))))))))
             :destination-tester
             ((object destination-object)
              (or (eq object destination-object)
                  (valid-move-p *application-frame* object destination-object)))
             :multiple-window nil
             :menu nil)
    (checker destination-object)
  (if (eq checker destination-object)
      `(com-select-checker ,checker)
      `(com-move-checker ,checker ,destination-object)))

(define-presentation-to-command-translator tr-new-game
    (board com-new-game clim-checkers
           :gesture :select
           :tester ((board) (game-over *application-frame*)))
    (object)
  `())
