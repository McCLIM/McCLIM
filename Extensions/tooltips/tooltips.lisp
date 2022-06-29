;;;   Copyright (C) 1993-2004 by SRI International.  All rights reserved.
;;;
;;; Permission granted by SRI for use in McClim under the McClim license.
;;;
;;; $Id: utils.lisp,v 1.8 2007/07/14 02:14:21 gilham Exp $
;;;

(defpackage #:mcclim-tooltips
  (:use #:clim :clim-lisp)
  (:export #:*tooltip-color*
           #:*tooltip-ink*
           #:*tooltip-text-style*
           #:*tooltip-wrap-p*
           #:*tooltip-wrap-width*
           ;; #:*tooltip-wrap-slop*
           ;; #:*tooltip-max-height*
           ;; #:*tooltip-enforce-max-height-p*
           #:*tooltip-delay*
           #:draw-tooltip #:erase-tooltip
           #:orec-relative->absolute-region))

(in-package #:mcclim-tooltips)

;;;============================= CLIM Tooltips ===============================
;;; paley:Jun-19-2007

;;; To use tooltips w/ clim presentations, define after methods for the clim
;;; presentation-method clim:highlight-presentation.  When the state argument is
;;; :highlight, the method should call draw-tooltip.  When the state argument is
;;; :unhighlight, the method should call erase-tooltip.  Here's a sample call
;;; (which assumes the fn get-tooltip-text returns a string or NIL):
;;;
#+ (or)
(clim:define-presentation-method clim:highlight-presentation :after
  ((type t) record stream (state (eql :highlight)))
  (draw-tooltip stream (get-tooltip-text record)
                :region (orec-relative->absolute-region record stream)))
#+ (or)
(clim:define-presentation-method clim:highlight-presentation :after
  ((type t) record stream (state (eql :unhighlight)))
  (declare (ignore record))
  (erase-tooltip stream))
;;;
;;; At some point, we may want to extend the tooltip functionality to allow for
;;; arbitrary output w/in a tooltip (i.e. not just a string, maybe graphics
;;; too).

;;; Tooltip parameters.  Intended to be globally customizable but
;;; overridable at the time DRAW-TOOLTIP is called.
(defparameter *tooltip-color* (make-rgb-color 1.0 1.0 0.85))
(defparameter *tooltip-ink* +black+)
(defparameter *tooltip-text-style* (make-text-style :sans-serif :roman :small))
(defparameter *tooltip-wrap-p* t)
(defparameter *tooltip-wrap-width* 40) ; characters.
#+ (or) (defparameter *tooltip-wrap-slop* 5)    ; characters.
#+ (or) (defparameter *tooltip-max-height* 200)
#+ (or) (defparameter *tooltip-enforce-max-height-p* nil)
(defparameter *tooltip-delay* 0.25)

(defvar *tooltip-lock* (clim-sys:make-lock "tooltip"))
(defvar *tooltip-process* nil) ; Current tooltip drawing process.
(defvar *tooltip-orec* nil)  ; The currently drawn tooltip.

;;; ============================================================== draw-tooltip
;;; [API]
;;; paley:Jun-19-2007
;;;    Description : Draw a box containing text, to be used as a tooltip.  If x-
;;;                  and y-coordinates are supplied, the box will have its upper
;;;                  left corner at that position.  If no coordinates are
;;;                  supplied but a clim region is, the box will be positioned
;;;                  close to that region, but not overlapping it. The entire box
;;;                  is positioned within the viewport.  If no position and no
;;;                  region are supplied, the box is drawn "near" the pointer. We
;;;                  actually output the text twice, once to compute its size and
;;;                  the second time to actually render it -- we may be able to
;;;                  make this more efficient.
;;;
;;;      Arguments : stream: the clim stream to output to
;;;                  text: a string or NIL
;;;                  region: a clim region (optional) -- the tooltip should be
;;;                   positioned near this region.
;;;                  x,y: integers (optional) -- if supplied, the tooltip will
;;;                   have its upper left corner at this position.
;;;        Returns : nothing
;;;   Side Effects : sets *tooltip-orec*
;;; Update History : gilham:Jun-20-2007
;;;                  Make text rendering function an argument to remove
;;;                  dependency on pathway tools code.
;;;
;;;                  gilham:Jul-10-2007
;;;                  More intelligent placement of tooltip.
;;;
;;;                  gilham:Jul-13-2007
;;;                  Even more intelligent placement of tooltip.
;;;
;;;                  Note that if the region has a large bounding box it may not
;;;                  be reasonable to avoid having the tooltip obscure part of
;;;                  it.
;;;
;;;                  gilham:Jul-16-2007
;;;                  Cleanup and additional fixes to deal properly with newlines
;;;                  in the string.
;;;
;;;                  gilham:Aug-1-2007
;;;                  Work around problem with weird coordinates in (we think)
;;;                  filling-output.  Also, add a configurable delay before the
;;;                  tooltip is drawn.

(defvar *whitespace-bag* '(#\space #\tab #\newline))
(defun split-string-by-length (string wrap-length)
  "Take a string and return the front of it up to a given length,
  splitting it at a whitespace character if possible.  Return the
  string and the remainder of the string.  The strings have whitespace
  trimmed."
  (let* ((string-length (length string))
         (end wrap-length)
         (newline-position (position #\newline string :start 0 :end (min string-length end))))
    ;; Newlines are part of the formatting.  So we split at newlines
    ;; even if the string is shorter than the wrap length.
    (if newline-position
        (setf end newline-position)
      (if (< string-length wrap-length)
          ;; No newline and the length of the string is less than the
          ;; wrap length.  Return the original (trimmed) string and
          ;; nil.  The nil can be used by the caller to detect that
          ;; the string has been used up.
          (return-from split-string-by-length (values (string-trim *whitespace-bag* string) nil))
        ;; Look for whitespace.
        (let ((whitespace-position
               (position-if #'(lambda (char) (member char *whitespace-bag*))
                            string
                            :end wrap-length
                            :from-end t)))
          (when whitespace-position
            (setf end whitespace-position)))))
    ;; Note that the default (if no whitespace found) will be to have
    ;; the string violently split at the wrap-length.
    (values (string-trim *whitespace-bag* (subseq string 0 end))
            (string-left-trim *whitespace-bag* (subseq string end string-length)))))


(defun draw-tooltip (stream text
                     &key region x y
                          (text-render-fn #'draw-text*)
                          (background *tooltip-color*)
                          (ink *tooltip-ink*)
                          (text-style *tooltip-text-style*)
                          (wrap-p *tooltip-wrap-p*)
                          (wrap-width *tooltip-wrap-width*)
                          ;;(max-height *tooltip-max-height*)
                          ;;(enforce-max-height-p *tooltip-enforce-max-height-p*)
                          )
  #+ (or) (declare (ignore max-height enforce-max-height-p))
  ;; Suggested new args.  Most or all should have defaults based on global vars.
  ;;   To wrap or not to wrap, and width limit
  ;;   Height limit, and whether or not to enforce a height limit
  ;;   Background color
  ;;   Foreground color
  ;;   Timeout after which tooltip disappears (only if it is easy to do)
  ;;   Font and Font size
  ;; Can you figure out how to get the CLIM documentation text to appear as a tooltip?
  ;; Use a really pale yellow if possible...
  (flet ((process-draw-tooltip ()
           (sleep *tooltip-delay*)
           (when (and text (not (equal text "")))
               (with-drawing-options (stream :text-style text-style
                                             :ink ink)
                 (let ((margin 2)
                       (text-height (+ (stream-line-height stream) 2))
                       tt-x tt-y
                       strings
                       wd ht)
                   (if wrap-p ; Do it this way in case the text has newlines in it.
                       ;; You are running a fast computer, right?
                       (progn
                         (multiple-value-setq (wd ht)
                           (bounding-rectangle-size
                            (with-output-to-output-record (stream)
                              (let (this-line
                                    (left-over text)
                                    (i 0))
                                (loop
                                  (multiple-value-setq (this-line left-over)
                                    (split-string-by-length left-over wrap-width))
                                  (unless (equal this-line "")
                                    ;;				  (format *error-output* "end ~A: ~A~%" i this-line)
                                    (push this-line strings)
                                    (funcall text-render-fn stream this-line
                                             0 (+ (* i text-height) margin)))
                                  (incf i)
                                  ;;				(format *error-output* "left-over: ~A~%" left-over)
                                  (unless left-over (return)))
                                )
                              (setf strings (nreverse strings))))
                           )
                         (setf *tooltip-orec*
                               (with-output-recording-options (stream :draw nil :record t)
                                 (with-new-output-record (stream)
                                   (draw-rectangle* stream (- margin) (- margin)
                                                    (+ wd margin) (+ ht margin)
                                                    :ink background)
                                   (draw-rectangle* stream (- margin) (- margin)
                                                    (+ wd margin) (+ ht margin)
                                                    :filled nil)
                                   (do* ((string-list strings (cdr string-list))
                                         (string (car string-list) (car string-list))
                                         (i 0 (1+ i))
                                         )
                                        ((endp string-list))
                                     ;;			       (format *error-output* "~A~%" string)
                                     (funcall text-render-fn stream string
                                              0 (* i text-height) :align-x :left :align-y :top)
                                     )))))
                       (progn
                         (multiple-value-setq (wd ht)
                           (bounding-rectangle-size
                            (with-output-to-output-record (stream)
                              (funcall text-render-fn stream text 0 0))))
                         (setf *tooltip-orec*
                               (with-output-recording-options (stream :draw nil :record t)
                                 (with-new-output-record (stream)
                                   (draw-rectangle* stream (- margin) (- margin)
                                                    (+ wd margin) (+ ht margin)
                                                    :ink background)
                                   (draw-rectangle* stream (- margin) (- margin)
                                                    (+ wd margin) (+ ht margin)
                                                    :filled nil)
                                   (funcall text-render-fn stream text
                                            0 0 :align-x :left :align-y :top))))))
                   ;; This tries to put the tool tip near the region and near the pointer.
                   (when (and region (not (and x y)))
                     (multiple-value-bind (ptr-x ptr-y) (stream-pointer-position stream)
                       (let* ((viewport-br (window-viewport stream))
                              (viewport-max-x (bounding-rectangle-max-x viewport-br))
                              (viewport-min-x (bounding-rectangle-min-x viewport-br))
                              (viewport-max-y (bounding-rectangle-max-y viewport-br))
                              (viewport-min-y (bounding-rectangle-min-y viewport-br)))
                         (with-bounding-rectangle* (tooltip-left tooltip-top tooltip-right tooltip-bottom)
                                                   *tooltip-orec*
                           (declare (ignore tooltip-left tooltip-top))

                           ;; gilham:Aug-1-2007
                           ;; First, check to see if the bottom of the region is
                           ;; near the pointer.  If not, two things might be the
                           ;; case:
                           ;;
                           ;; 1) The region is very large in height.
                           ;; 2) We are encountering the problem with
                           ;;    the coordinates of certain records
                           ;;    being strange. (This may be a CLIM bug
                           ;;    and is still being investigated.)
                           ;;
                           ;; In either case, we want to ignore the
                           ;; region and just put the tool tip
                           ;;
                           ;; 1) Near the pointer, and
                           ;; 2) In the viewport.
                           ;;
                           ;; The latter constraint is very
                           ;; important---tooltips are useless if they
                           ;; are not visible in the viewport.
                           ;;
                           (if (> (- (clim:bounding-rectangle-max-y region) ptr-y) 40)
                               (progn
                                 (setf tt-x (+ ptr-x 10)
                                       tt-y (- ptr-y 30)))
                               (progn
                                 ;; Make it seem as if the pointer hit the
                                 ;; record from the bottom every time.
                                 (setf ptr-y (bounding-rectangle-max-y region))
                                 ;;
                                 ;; The idea is to not have the tooltip
                                 ;; cover either the pointer or the region
                                 ;; (that is, the text in the presentation
                                 ;; that is being highlighted). It may not
                                 ;; be possible to avoid the latter, if it
                                 ;; would put the tooltip too far from the
                                 ;; pointer, but at least we try to make
                                 ;; it not too egregious.
                                 (setf tt-x (+ ptr-x 10)
                                       tt-y (- (bounding-rectangle-min-y region) ht 10))
                                 ))
                           (when (< tt-y (- ptr-y (+ ht 40)))
                             (setf tt-y (- ptr-y (+ ht 40))))
                           ;; Try to keep the tool tip in the viewport.
                           (when (> (+ tt-x tooltip-right) viewport-max-x)
                             (decf tt-x (+ (- (+ tt-x tooltip-right) viewport-max-x) margin))
                             (when (< tt-x viewport-min-x) (setf tt-x viewport-min-x)))
                           ;; If the tooltip would go above the viewport, put it below the pointer.
                           (when (< tt-y viewport-min-y)
                             (setf tt-y (+ ptr-y 40))
                             (when (> tt-y viewport-max-y)
                               (setf tt-y (- viewport-max-y tooltip-bottom))))))))
                   (setq x (or x tt-x 0)
                         y (or y tt-y 0)))))
           (setf (output-record-position *tooltip-orec*) (values x y))
           (tree-recompute-extent *tooltip-orec*)
           (replay *tooltip-orec* stream)
           (force-output stream)
           (setf *tooltip-process* nil)))

    (erase-tooltip stream) ;; clear previous tooltip, if there is one
    ;; gilham:Aug-1-2007
    ;; We create a process that will wait a configurable delay period
    ;; before drawing the tooltip.  If the unhighlight method runs
    ;; before this process wakes up, it kills the process off and so
    ;; prevents the tooltip from being drawn.
    (clim-sys:with-lock-held (*tooltip-lock*)
      (setf *tooltip-process*
            (clim-sys:make-process #'process-draw-tooltip :name "Draw-Tooltip")))))


;;; paley:Jun-19-2007 Erase a tooltip drawn by draw-tooltip
;;;   Side Effects : sets *tooltip-orec* to nil
(defun erase-tooltip (stream)
  ;; gilham:Aug-1-2007 See if there's a process waiting to draw a
  ;; tooltip.  If so, kill it.
  (clim-sys:with-lock-held (*tooltip-lock*)
    (when (and *tooltip-process*
               (clim-sys:process-state *tooltip-process*))
      (clim-sys:destroy-process *tooltip-process*)))
  (when *tooltip-orec*
    (erase-output-record *tooltip-orec* stream nil)
    (setf *tooltip-orec* nil)))

;;; ============================================ orec-relative->absolute-region
;;; [API]
;;; paley:Jun-19-2007    Description : Given an output record, return a clim
;;;  region that reflects its actual position in the window.
;;;      Arguments : orec: an output-record
;;;                  stream: the stream on which orec was displayed
;;;        Returns : a clim region
;;;   Side Effects : none
;;; Update History :

(defun orec-relative->absolute-region (orec stream)
  (declare (ignore stream))
    (transform-region +identity-transformation+ orec))
