;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

(cl:in-package :drei-tests)

(def-suite rectangle-tests :description "The test suite for
rectangle-editing related tests." :in drei-tests)

(in-suite rectangle-tests)

(test map-rectangle-lines
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (macrolet ((check (startcol endcol)
                 `(progn
                    (is-true (beginning-of-line-p mark))
                    (is (= (line-number mark) (incf line)))
                    (is (> 4 line))
                    (is (= startcol ,startcol))
                    (is (= endcol ,endcol)))))
      (beginning-of-buffer *current-point*)
      (end-of-buffer *current-mark*)
      (let ((line -1))
        (map-rectangle-lines *current-buffer*
                             #'(lambda (mark startcol endcol)
                                 (check 0 16))
                             *current-point*
                             *current-mark*)
        (is (= line 3)))
      (let ((line -1))
        (map-rectangle-lines *current-buffer*
                             #'(lambda (mark startcol endcol)
                                 (check 0 16))
                             *current-mark*
                             *current-point*)
        (is (= line 3)))
      (setf (offset *current-point*) 2)
      (setf (offset *current-mark*) 63)
      (let ((line -1))
        (map-rectangle-lines *current-buffer*
                             #'(lambda (mark startcol endcol)
                                 (check 2 13))
                             *current-point*
                             *current-mark*)
        (is (= line 3)))
      (let ((line -1))
        (map-rectangle-lines *current-buffer*
                             #'(lambda (mark startcol endcol)
                                 (check 2 13))
                             *current-mark*
                             *current-point*)
        (is (= line 3)))
      (beginning-of-buffer *current-point*)
      (beginning-of-buffer *current-mark*)
      (let ((line -1))
        (map-rectangle-lines *current-buffer*
                             #'(lambda (mark startcol endcol)
                                 (check 0 0))
                             *current-point*
                             *current-mark*)
        (is (= line 0))))))

(test extract-and-delete-rectangle-line
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (end-of-buffer *current-mark*)
    (is (equal (map-rectangle-lines *current-buffer*
                                    #'extract-and-delete-rectangle-line
                                    *current-point*
                                    *current-mark*)
               '("Line number one "
                 "Line number two "
                 "Line number thre"
                 "Line number four")))
    (buffer-is "

e
"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (end-of-buffer *current-mark*)
    (beginning-of-line *current-mark*)
    (is (equal (map-rectangle-lines *current-buffer*
                                    #'extract-and-delete-rectangle-line
                                    *current-point*
                                    *current-mark*)
               '(""
                 ""
                 ""
                 "")))
    (buffer-is "Line number one
Line number two
Line number three
Line number four"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (forward-line *current-point* *current-syntax*)
    (forward-object *current-point* 5)
    
    (end-of-buffer *current-mark*)
    (backward-line *current-mark* *current-syntax*)
    (beginning-of-line *current-mark*)
    (forward-object *current-mark* 12)

    (is (equal (map-rectangle-lines *current-buffer*
                                    #'extract-and-delete-rectangle-line
                                    *current-point*
                                    *current-mark*)
               '("number "
                 "number ")))
    (buffer-is "Line number one
Line two
Line three
Line number four")))

(test open-rectangle-line
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (end-of-buffer *current-mark*)
    (map-rectangle-lines *current-buffer*
                         #'open-rectangle-line
                         *current-point*
                         *current-mark*)
    (buffer-is "                Line number one
                Line number two
                Line number three
                Line number four"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (end-of-buffer *current-mark*)
    (beginning-of-line *current-mark*)
    (map-rectangle-lines *current-buffer*
                         #'open-rectangle-line
                         *current-point*
                         *current-mark*)
    (buffer-is "Line number one
Line number two
Line number three
Line number four"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (forward-line *current-point* *current-syntax*)
    (forward-object *current-point* 5)
    
    (end-of-buffer *current-mark*)
    (backward-line *current-mark* *current-syntax*)
    (beginning-of-line *current-mark*)
    (forward-object *current-mark* 12)

    (map-rectangle-lines *current-buffer*
                         #'open-rectangle-line
                         *current-point*
                         *current-mark*)
    (buffer-is "Line number one
Line        number two
Line        number three
Line number four")))

(test clear-rectangle-line
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (end-of-buffer *current-mark*)
    (map-rectangle-lines *current-buffer*
                         #'clear-rectangle-line
                         *current-point*
                         *current-mark*)
    (buffer-is "               
               
                e
                "))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (end-of-buffer *current-mark*)
    (beginning-of-line *current-mark*)
    (map-rectangle-lines *current-buffer*
                         #'clear-rectangle-line
                         *current-point*
                         *current-mark*)
    (buffer-is "Line number one
Line number two
Line number three
Line number four"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (forward-line *current-point* *current-syntax*)
    (forward-object *current-point* 5)
    
    (end-of-buffer *current-mark*)
    (backward-line *current-mark* *current-syntax*)
    (beginning-of-line *current-mark*)
    (forward-object *current-mark* 12)

    (map-rectangle-lines *current-buffer*
                         #'clear-rectangle-line
                         *current-point*
                         *current-mark*)
    (buffer-is "Line number one
Line        two
Line        three
Line number four")))

(test delete-rectangle-line-whitespace
  (with-drei-environment (:initial-contents "   Line number one
      Line number two
 Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (end-of-buffer *current-mark*)
    (map-rectangle-lines *current-buffer*
                         #'delete-rectangle-line-whitespace
                         *current-point*
                         *current-mark*)
    (buffer-is "Line number one
Line number two
Line number three
Line number four"))
  (with-drei-environment (:initial-contents "   Line number one
      Line number two
 Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (end-of-buffer *current-mark*)
    (beginning-of-line *current-mark*)
    (map-rectangle-lines *current-buffer*
                         #'delete-rectangle-line-whitespace
                         *current-point*
                         *current-mark*)
    (buffer-is "Line number one
Line number two
Line number three
Line number four"))
  (with-drei-environment (:initial-contents "   Line number one
      Line number two
 Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (forward-line *current-point* *current-syntax*)
    
    (end-of-buffer *current-mark*)
    (backward-line *current-mark* *current-syntax*)
    (beginning-of-line *current-mark*)

    (map-rectangle-lines *current-buffer*
                         #'delete-rectangle-line-whitespace
                         *current-point*
                         *current-mark*)
    (buffer-is "   Line number one
Line number two
Line number three
Line number four")))

(test replace-rectangle-line
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (end-of-buffer *current-mark*)
    (map-rectangle-lines *current-buffer*
                         #'(lambda (mark startcol endcol)
                             (replace-rectangle-line mark startcol endcol "DREI"))
                         *current-point*
                         *current-mark*)
    (buffer-is "DREI
DREI
DREIe
DREI"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (end-of-buffer *current-mark*)
    (beginning-of-line *current-mark*)
    (map-rectangle-lines *current-buffer*
                         #'(lambda (mark startcol endcol)
                             (replace-rectangle-line mark startcol endcol "DREI"))
                         *current-point*
                         *current-mark*)
    (buffer-is "DREILine number one
DREILine number two
DREILine number three
DREILine number four"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (forward-line *current-point* *current-syntax*)
    (forward-object *current-point* 5)
    
    (end-of-buffer *current-mark*)
    (backward-line *current-mark* *current-syntax*)
    (beginning-of-line *current-mark*)
    (forward-object *current-mark* 12)

    (map-rectangle-lines *current-buffer*
                         #'(lambda (mark startcol endcol)
                             (replace-rectangle-line mark startcol endcol "DREI"))
                         *current-point*
                         *current-mark*)
    (buffer-is "Line number one
Line DREItwo
Line DREIthree
Line number four")))

(test insert-in-rectangle-line
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (end-of-buffer *current-mark*)
    (map-rectangle-lines *current-buffer*
                         #'(lambda (mark startcol endcol)
                             (insert-in-rectangle-line mark startcol endcol "DREI"))
                         *current-point*
                         *current-mark*)
    (buffer-is "DREILine number one
DREILine number two
DREILine number three
DREILine number four"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (end-of-buffer *current-mark*)
    (beginning-of-line *current-mark*)
    (map-rectangle-lines *current-buffer*
                         #'(lambda (mark startcol endcol)
                             (insert-in-rectangle-line mark startcol endcol "DREI"))
                         *current-point*
                         *current-mark*)
    (buffer-is "DREILine number one
DREILine number two
DREILine number three
DREILine number four"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer *current-point*)
    (forward-line *current-point* *current-syntax*)
    (forward-object *current-point* 5)
    
    (end-of-buffer *current-mark*)
    (backward-line *current-mark* *current-syntax*)
    (beginning-of-line *current-mark*)
    (forward-object *current-mark* 12)

    (map-rectangle-lines *current-buffer*
                         #'(lambda (mark startcol endcol)
                             (insert-in-rectangle-line mark startcol endcol "DREI"))
                         *current-point*
                         *current-mark*)
    (buffer-is "Line number one
Line DREInumber two
Line DREInumber three
Line number four")))

(test insert-rectangle-at-mark
  (macrolet ((check (before rectangle offset after)
               (check-type before string)
               (check-type rectangle list)
               (check-type after string)
               `(with-drei-environment (:initial-contents ,before)
                  (setf (offset *current-point*) ,offset)
                  (insert-rectangle-at-mark
                   *current-buffer* *current-point*
                   ,rectangle)
                  (buffer-is ,after))))
    (check "Line number one
Line number two
Line number three
Line number four
"
           '("Line number one "
             "Line number two "
             "Line number thre"
             "Line number four")
           0
           "Line number one Line number one
Line number two Line number two
Line number threLine number three
Line number fourLine number four
")
    (check "Line number one
Line number two
Line number three
Line number four
"
           '("DREI   "
             "CLIMACS")
           20
           "Line number one
LineDREI    number two
LineCLIMACS number three
Line number four
")
    (check "Line number one
Line number two
Line number three
Line number four
"
           '("DREI   "
             "CLIMACS")
           66
           "Line number one
Line number two
Line number three
Line number fourDREI   
                CLIMACS
")))
