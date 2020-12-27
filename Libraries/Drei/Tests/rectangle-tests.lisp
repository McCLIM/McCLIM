;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2006-2008 Troels Henriksen <athas@sigkill.dk>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Tests for the rectangle editing functionality.

(cl:in-package #:drei-tests)

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
      (beginning-of-buffer (point))
      (end-of-buffer (mark))
      (let ((line -1))
        (map-rectangle-lines (current-view)
                             #'(lambda (mark startcol endcol)
                                 (check 0 16))
                             (point)
                             (mark))
        (is (= line 3)))
      (let ((line -1))
        (map-rectangle-lines (current-view)
                             #'(lambda (mark startcol endcol)
                                 (check 0 16))
                             (mark)
                             (point))
        (is (= line 3)))
      (setf (offset (point)) 2)
      (setf (offset (mark)) 63)
      (let ((line -1))
        (map-rectangle-lines (current-view)
                             #'(lambda (mark startcol endcol)
                                 (check 2 13))
                             (point)
                             (mark))
        (is (= line 3)))
      (let ((line -1))
        (map-rectangle-lines (current-view)
                             #'(lambda (mark startcol endcol)
                                 (check 2 13))
                             (mark)
                             (point))
        (is (= line 3)))
      (beginning-of-buffer (point))
      (beginning-of-buffer (mark))
      (let ((line -1))
        (map-rectangle-lines (current-view)
                             #'(lambda (mark startcol endcol)
                                 (check 0 0))
                             (point)
                             (mark))
        (is (= line 0))))))

(test extract-and-delete-rectangle-line
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer (point))
    (end-of-buffer (mark))
    (is (equal (map-rectangle-lines (current-view)
                                    #'extract-and-delete-rectangle-line
                                    (point)
                                    (mark))
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
    (beginning-of-buffer (point))
    (end-of-buffer (mark))
    (beginning-of-line (mark))
    (is (equal (map-rectangle-lines (current-view)
                                    #'extract-and-delete-rectangle-line
                                    (point)
                                    (mark))
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
    (beginning-of-buffer (point))
    (forward-line (point) (current-syntax))
    (forward-object (point) 5)

    (end-of-buffer (mark))
    (backward-line (mark) (current-syntax))
    (beginning-of-line (mark))
    (forward-object (mark) 12)

    (is (equal (map-rectangle-lines (current-view)
                                    #'extract-and-delete-rectangle-line
                                    (point)
                                    (mark))
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
    (beginning-of-buffer (point))
    (end-of-buffer (mark))
    (map-rectangle-lines (current-view)
                         #'open-rectangle-line
                         (point)
                         (mark))
    (buffer-is "                Line number one
                Line number two
                Line number three
                Line number four"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer (point))
    (end-of-buffer (mark))
    (beginning-of-line (mark))
    (map-rectangle-lines (current-view)
                         #'open-rectangle-line
                         (point)
                         (mark))
    (buffer-is "Line number one
Line number two
Line number three
Line number four"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer (point))
    (forward-line (point) (current-syntax))
    (forward-object (point) 5)

    (end-of-buffer (mark))
    (backward-line (mark) (current-syntax))
    (beginning-of-line (mark))
    (forward-object (mark) 12)

    (map-rectangle-lines (current-view)
                         #'open-rectangle-line
                         (point)
                         (mark))
    (buffer-is "Line number one
Line        number two
Line        number three
Line number four")))

(test clear-rectangle-line
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer (point))
    (end-of-buffer (mark))
    (map-rectangle-lines (current-view)
                         #'clear-rectangle-line
                         (point)
                         (mark))
    (buffer-is "               
               
                e
                "))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer (point))
    (end-of-buffer (mark))
    (beginning-of-line (mark))
    (map-rectangle-lines (current-view)
                         #'clear-rectangle-line
                         (point)
                         (mark))
    (buffer-is "Line number one
Line number two
Line number three
Line number four"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer (point))
    (forward-line (point) (current-syntax))
    (forward-object (point) 5)

    (end-of-buffer (mark))
    (backward-line (mark) (current-syntax))
    (beginning-of-line (mark))
    (forward-object (mark) 12)

    (map-rectangle-lines (current-view)
                         #'clear-rectangle-line
                         (point)
                         (mark))
    (buffer-is "Line number one
Line        two
Line        three
Line number four")))

(test delete-rectangle-line-whitespace
  (with-drei-environment (:initial-contents "   Line number one
      Line number two
 Line number three
Line number four")
    (beginning-of-buffer (point))
    (end-of-buffer (mark))
    (map-rectangle-lines (current-view)
                         #'delete-rectangle-line-whitespace
                         (point)
                         (mark))
    (buffer-is "Line number one
Line number two
Line number three
Line number four"))
  (with-drei-environment (:initial-contents "   Line number one
      Line number two
 Line number three
Line number four")
    (beginning-of-buffer (point))
    (end-of-buffer (mark))
    (beginning-of-line (mark))
    (map-rectangle-lines (current-view)
                         #'delete-rectangle-line-whitespace
                         (point)
                         (mark))
    (buffer-is "Line number one
Line number two
Line number three
Line number four"))
  (with-drei-environment (:initial-contents "   Line number one
      Line number two
 Line number three
Line number four")
    (beginning-of-buffer (point))
    (forward-line (point) (current-syntax))

    (end-of-buffer (mark))
    (backward-line (mark) (current-syntax))
    (beginning-of-line (mark))

    (map-rectangle-lines (current-view)
                         #'delete-rectangle-line-whitespace
                         (point)
                         (mark))
    (buffer-is "   Line number one
Line number two
Line number three
Line number four")))

(test replace-rectangle-line
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer (point))
    (end-of-buffer (mark))
    (map-rectangle-lines (current-view)
                         #'(lambda (mark startcol endcol)
                             (replace-rectangle-line mark startcol endcol "DREI"))
                         (point)
                         (mark))
    (buffer-is "DREI
DREI
DREIe
DREI"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer (point))
    (end-of-buffer (mark))
    (beginning-of-line (mark))
    (map-rectangle-lines (current-view)
                         #'(lambda (mark startcol endcol)
                             (replace-rectangle-line mark startcol endcol "DREI"))
                         (point)
                         (mark))
    (buffer-is "DREILine number one
DREILine number two
DREILine number three
DREILine number four"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer (point))
    (forward-line (point) (current-syntax))
    (forward-object (point) 5)

    (end-of-buffer (mark))
    (backward-line (mark) (current-syntax))
    (beginning-of-line (mark))
    (forward-object (mark) 12)

    (map-rectangle-lines (current-view)
                         #'(lambda (mark startcol endcol)
                             (replace-rectangle-line mark startcol endcol "DREI"))
                         (point)
                         (mark))
    (buffer-is "Line number one
Line DREItwo
Line DREIthree
Line number four")))

(test insert-in-rectangle-line
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer (point))
    (end-of-buffer (mark))
    (map-rectangle-lines (current-view)
                         #'(lambda (mark startcol endcol)
                             (insert-in-rectangle-line mark startcol endcol "DREI"))
                         (point)
                         (mark))
    (buffer-is "DREILine number one
DREILine number two
DREILine number three
DREILine number four"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer (point))
    (end-of-buffer (mark))
    (beginning-of-line (mark))
    (map-rectangle-lines (current-view)
                         #'(lambda (mark startcol endcol)
                             (insert-in-rectangle-line mark startcol endcol "DREI"))
                         (point)
                         (mark))
    (buffer-is "DREILine number one
DREILine number two
DREILine number three
DREILine number four"))
  (with-drei-environment (:initial-contents "Line number one
Line number two
Line number three
Line number four")
    (beginning-of-buffer (point))
    (forward-line (point) (current-syntax))
    (forward-object (point) 5)

    (end-of-buffer (mark))
    (backward-line (mark) (current-syntax))
    (beginning-of-line (mark))
    (forward-object (mark) 12)

    (map-rectangle-lines (current-view)
                         #'(lambda (mark startcol endcol)
                             (insert-in-rectangle-line mark startcol endcol "DREI"))
                         (point)
                         (mark))
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
                  (setf (offset (point)) ,offset)
                  (insert-rectangle-at-mark
                   (current-view) (point)
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
