;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998-2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2001-2002 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2019 by Daniel Kochmański (daniel@turtleware.eu)

;;; Implementation of drag-and-drop translators as defined in 23.7.

(in-package #:climi)

(defvar *dragged-presentation* nil
  "Bound to the presentation dragged in a drag-and-drop context")
(defvar *dragged-object* nil
  "Bound to the object dragged in a drag-and-drop context")

(defclass drag-n-drop-translator (presentation-translator)
  ((destination-type :reader destination-type :initarg :destination-type)
   (feedback :reader feedback :initarg :feedback)
   (highlighting :reader highlighting :initarg :highlighting)
   (destination-translator :reader destination-translator
                           :initarg :destination-translator)))

;;; According to the Franz User's guide, the destination object is
;;; available in the tester, documentation, and translator function
;;; as destination-object. Therefore OBJECT is the dragged object. In
;;; our scheme the tester function, translator function etc. is
;;; really called on the destination object. So, we do a little
;;; shuffling of arguments here. We don't do that for the destination
;;; translator because we can call that ourselves in frame-drag-and-drop.
;;;
;;; Also, in Classic CLIM the destination presentation is passed as a
;;; destination-presentation keyword argument; hence the presentation
;;; argument is the dragged presentation.

(defmethod initialize-instance :after ((obj drag-n-drop-translator)
                                       &key documentation
                                         pointer-documentation
                                         destination-translator)
  (declare (ignore destination-translator))
  (setf (slot-value obj 'documentation) documentation)
  (when pointer-documentation
    (setf (slot-value obj 'pointer-documentation) pointer-documentation)))

(defmacro define-drag-and-drop-translator
    (name (from-type to-type destination-type command-table
                     &rest args &key
                     (gesture :select)
                     (tester 'default-translator-tester)
                     documentation
                     (pointer-documentation nil pointer-doc-p)
                     (menu t)
                     (priority 0)
                     (feedback 'frame-drag-and-drop-feedback)
                     (highlighting 'frame-drag-and-drop-highlighting))
     arglist &body body)
  (declare (ignore tester gesture documentation pointer-documentation
                   menu priority))
  (let* ((real-dest-type (expand-presentation-type-abbreviation
                          destination-type))
         (name-string (command-name-from-symbol name))
         (drag-string (format nil "Drag to ~A" name-string))
         (pointer-doc (if pointer-doc-p
                          nil
                          `(:pointer-documentation
                            ((object destination-object stream)
                             (declare (ignore object))
                             (write-string (if destination-object
                                               ,name-string
                                               ,drag-string)
                                           stream))))))
    (with-keywords-removed (args (:feedback :highlighting))
      (with-gensyms (object)
        `(define-presentation-translator ,name
             (,from-type ,to-type ,command-table
                         :tester-definitive t
                         ,@args
                         ,@pointer-doc
                         :feedback #',feedback
                         :highlighting #',highlighting
                         :destination-type ',real-dest-type
                         :destination-translator #',(make-translator-fun arglist body)
                         :translator-class drag-n-drop-translator)
             (,object presentation context-type frame event window x y)
           (declare (ignore ,object))
           (invoke-drag-and-drop ',name ',command-table
                                 presentation context-type
                                 frame event window x y))))))

;;; It is unspecified what happens when there are multiple dnd
;;; translators which are applicable at the same time. McCLIM sets
;;; default feedback and highlight functions to those associated with
;;; the translator which got us here. When we drag over presentation
;;; which has applicable dnd translator we use its functions until the
;;; sensitive area is left. -- jd 2019-07-06
(defun invoke-drag-and-drop (translator-name command-table
                            from-presentation context-type frame event window
                            x y)
  (declare (ignore command-table))
  (let* ((*dragged-presentation* from-presentation)
         (*dragged-object* (presentation-object from-presentation))
         (translators (mapcan (lambda (trans)
                                (and (typep trans 'drag-n-drop-translator)
                                     (funcall (tester trans)
                                              (presentation-object from-presentation)
                                              :presentation from-presentation
                                              :context-type context-type
                                              :frame frame
                                              :window window
                                              :x x
                                              :y y
                                              :event event)
                                     (list trans)))
                              (find-presentation-translators
                               (presentation-type from-presentation)
                               context-type
                               (frame-command-table frame))))
         ;; Default feedback and highlight functions are those of the
         ;; translator that got us here. Initial highlight value nil
         ;; will cause the source presentation of the dragged object
         ;; to be unhighlighted at start. -- jd 2019-07-06
         (translator (find translator-name translators :key #'name))
         (feedback-fn (feedback translator))
         (highlight-fn nil)
         (destination-presentation nil)
         (initial-x x)
         (initial-y y)
         (last-presentation nil)
         (feedback-activated nil)
         (last-event nil))
    (flet ((find-dest-translator (presentation window x y)
             (loop for translator in translators
                when (and (presentation-subtypep (presentation-type presentation)
                                                 (destination-type translator))
                          (test-presentation-translator translator
                                                        presentation
                                                        context-type
                                                        frame
                                                        window x y))
                do (return-from find-dest-translator translator))
             nil)
           (do-feedback (window x y state)
             (when (and feedback-activated window)
               (maybe-funcall feedback-fn frame from-presentation window
                              initial-x initial-y x y state)))
           (do-highlight (presentation window state)
             (when presentation
               (maybe-funcall highlight-fn frame presentation window state)))
           (last-point ()
             (if last-event
                 (values (event-sheet last-event)
                         (pointer-event-x last-event)
                         (pointer-event-y last-event))
                 (values nil nil nil))))
      (block do-tracking
        (tracking-pointer (window :context-type `(or ,@(mapcar #'destination-type translators))
                                  ;; context-type should be T and we
                                  ;; should do closer examination
                                  ;; inside :presentation clause
                                  :highlight nil
                                  :multiple-window t)
          (:presentation (&key presentation window event x y)
            (let ((dest-translator (find-dest-translator presentation window x y)))
              (multiple-value-call #'do-feedback (last-point) :unhighlight)
              (setq feedback-activated t)
              (do-highlight last-presentation (last-point) :unhighlight)
              (setq last-event event
                    last-presentation presentation)
              (if dest-translator
                  (setf feedback-fn (feedback dest-translator)
                        highlight-fn (highlighting dest-translator))
                  (setf feedback-fn (feedback translator)
                        highlight-fn (highlighting translator)))
              (do-highlight presentation window :highlight)
              (do-feedback window x y :highlight)
              (multiple-value-call #'document-drag-n-drop
                (if dest-translator
                    (values dest-translator presentation)
                    (values translator      nil))
                context-type frame event window
                x y)))
          (:pointer-motion (&key event window x y)
            (multiple-value-call #'do-feedback (last-point) :unhighlight)
            (setq feedback-activated t)
            (do-highlight last-presentation (last-point) :unhighlight)
            (setq last-event event
                  last-presentation nil)
            (do-feedback window x y :highlight)
            (document-drag-n-drop translator nil
                                  context-type frame event window
                                  x y))
          ;; XXX only support finish-on-release for now.
          #-(and)(:presentation-button-press ())
          (:presentation-button-release (&key presentation event)
            (setq destination-presentation presentation
                  last-event event)
            (return-from do-tracking nil))
          #-(and)(:button-press ())
          (:pointer-button-release (&key event)
            (setq last-event event)
            (return-from do-tracking nil))))
      ;;
      ;; XXX Assumes x y from :button-release are the same as for the preceding
      ;; button-motion; is that correct?
      (multiple-value-call #'do-feedback (last-point) :unhighlight)
      (do-highlight last-presentation (last-point) :unhighlight)
      (when-let ((stream *pointer-documentation-output*))
        (window-clear stream))

      (if destination-presentation
          (let ((final-translator (multiple-value-call #'find-dest-translator
                                    destination-presentation (last-point))))
            (if final-translator
                (funcall (destination-translator final-translator)
                         *dragged-object*
                         :presentation *dragged-presentation*
                         :destination-object (presentation-object destination-presentation)
                         :destination-presentation destination-presentation
                         :context-type context-type
                         :frame frame
                         :event event
                         :window window
                         :x x
                         :y y)
                (values nil nil)))
          (values nil nil)))))

(defun document-drag-n-drop
    (translator presentation context-type frame event window x y)
  (when-let ((stream *pointer-documentation-output*))
    (let ((function (pointer-documentation translator)))
      (window-clear stream)
      (with-end-of-page-action (stream :allow)
        (with-end-of-line-action (stream :allow)
          (funcall function
                   *dragged-object*
                   :presentation *dragged-presentation*
                   :destination-object (and presentation
                                            (presentation-object presentation))
                   :destination-presentation presentation
                   :context-type context-type
                   :frame frame
                   :event event
                   :window window
                   :x x
                   :y y
                   :stream stream))))))
