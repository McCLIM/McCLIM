;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Alexey Dejneka (adejneka@comail.ru)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :CLIM-INTERNALS)

(defgeneric menu-choose
    (items
     &key associated-window printer presentation-type default-item
     text-style label cache unique-id id-test cache-value cache-test
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y scroll-bars pointer-documentation))

(defgeneric frame-manager-menu-choose
    (frame-manager items
     &key associated-window printer presentation-type default-item
     text-style label cache unique-id id-test cache-value cache-test
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y scroll-bars pointer-documentation))

(defgeneric menu-choose-from-drawer
    (menu presentation-type drawer
     &key x-position y-position cache unique-id id-test cache-value cache-test
     default-presentation pointer-documentation))

;;;
(defun menu-item-value (menu-item)
  (cond ((atom menu-item)
         menu-item)
        ((atom (cdr menu-item))
         (cdr menu-item))
        (t (getf (cdr menu-item) :value (car menu-item)))))

(defun menu-item-display (menu-item)
  (if (atom menu-item)
      menu-item
      (car menu-item)))

(defun menu-item-options (menu-item)
  (if (and (consp menu-item)
           (consp (cdr menu-item)))
      (cdr menu-item) ; XXX Remove :VALUE?
      nil))

(defun print-menu-item (menu-item &optional (stream *standard-output*))
  (princ (menu-item-display menu-item) stream))

(defun draw-standard-menu
    (stream presentation-type items default-item
     &key (item-printer #'print-menu-item)
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     (cell-align-x :left) (cell-align-y :top))
  (declare (ignore default-item max-width max-height n-rows n-columns
                   row-wise)) ; FIXME!!!
  (formatting-table (stream :x-spacing x-spacing
                            :y-spacing y-spacing)
    (dolist (item items)
      (formatting-row (stream)
        (formatting-cell (stream :align-x cell-align-x
                                 :align-y cell-align-y)
          (with-output-as-presentation (stream (menu-item-value item)
                                               presentation-type)
            (funcall item-printer item stream)))))))


(defmacro with-menu ((menu &optional associated-window
                           &key (deexpose t))
                     &body body)
  (check-type menu symbol)
  (with-gensyms (with-menu-cont)
    `(flet ((,with-menu-cont (,menu)
              ,@body))
       (declare (dynamic-extent #',with-menu-cont))
       (invoke-with-menu #',with-menu-cont
                         ,associated-window ; XXX
                         ',deexpose)))) ; XXX!!!

(defun invoke-with-menu (continuation associated-window deexpose)
  (declare (ignore deexpose))           ; FIXME!!!
  (let* ((associated-window (or associated-window *application-frame*))
         (fm (frame-manager associated-window))
         (stream (make-pane-1 fm associated-window 'command-menu-pane))
         (frame (make-menu-frame stream)))
    (adopt-frame fm frame)
    (change-space-requirements stream :width 100 :height 100)
    (unwind-protect
         (progn
           (setf (stream-end-of-line-action stream) :allow
                 (stream-end-of-page-action stream) :allow)
           (funcall continuation stream))
      (disown-frame fm frame))))

(define-presentation-type menu-item ())

;;;
(defmethod menu-choose
    (items &rest args &key associated-window &allow-other-keys)
  (let ((frame-manager (frame-manager (or associated-window
                                          *application-frame*))))
    (apply #'frame-manager-menu-choose frame-manager items args)))

(defmethod frame-manager-menu-choose
    (frame-manager items                ; XXX STANDARD-FRAME-MANAGER
     &key associated-window printer presentation-type
     (default-item nil default-item-p)
     text-style label cache unique-id id-test cache-value cache-test
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y scroll-bars pointer-documentation)
  (with-menu (menu)
    (menu-choose-from-drawer menu (or presentation-type 'menu-item)
                             (lambda (stream type)
                               (draw-standard-menu stream type items
                                                   (if default-item-p
                                                       default-item
                                                       (first items))))
                             )))

(defmethod menu-choose-from-drawer
    (menu presentation-type drawer
     &key x-position y-position cache unique-id id-test cache-value cache-test
     default-presentation pointer-documentation)
  (funcall drawer menu presentation-type)
  (when (typep menu 'command-menu-pane)
    (with-bounding-rectangle* (x1 y1 x2 y2)
        (stream-output-history menu)
      (declare (ignorable x1 y1 x2 y2))
      (change-space-requirements menu
                                 :width x2
                                 :height y2
                                 :resize-frame t)))
  (accept presentation-type :stream menu :prompt nil))