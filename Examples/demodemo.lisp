;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: The demo demo
;;;   Created: 2002-02-11
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

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

(in-package :clim-demo)

(defun make-demo-button (title demo-frame-class)
  (make-pane 'push-button
             :label title
             :activate-callback
             (let ((runningp nil)
                   (frame nil))
               (lambda (&rest ignore)
                 (declare (ignore ignore))
                 (unless runningp
                   (make-application-frame demo-frame-class))))))

(define-application-frame demodemo 
    () ()
    (:panes
     (title :text-field 
            :value "FreeCLIM Demos"
            :text-style (make-text-style :sans-serif :roman :huge))
     
     (colorslider-btn (make-demo-button "Colorslider" 'colorslider))
     (calculator-btn (make-demo-button "Calculator"   'calculator)) )
    (:layouts
     (default
         (vertically ()
           (spacing (:width 10 :height 10) title)
           colorslider-btn
           calculator-btn))))
           
