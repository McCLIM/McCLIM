(cl:in-package #:clim-tests)

(def-suite* :mcclim.utils
  :in :mcclim)

(test remove-duplicated-points
  (is (equal nil (climi::remove-duplicated-points nil)))
  (let ((p1 (make-point 1 1))
        (p2 (make-point 2 2)))
    (is (equal (list p1 p2)    (climi::remove-duplicated-points (list p1 p2))))
    (is (equal (list p1 p2)    (climi::remove-duplicated-points (list p1 p2 p2))))
    (is (equal (list p1 p2)    (climi::remove-duplicated-points (list p1 p2 p2 p2))))
    (is (equal (list p1 p2 p1) (climi::remove-duplicated-points (list p1 p2 p2 p1))))
    (is (equal (list p1 p2 p1) (climi::remove-duplicated-points (list p1 p2 p2 p1 p1))))
    (is (equal (list p1 p2)    (climi::remove-duplicated-points (list p1 p2 p2 p1 p1) t)))))


;;; Line splitting utility
;;; ============================================================================

;;; orthogonal dimensions:
;;; - string is lesser, equal or greater than first margin
;;; - after first split string is lesser, equal or greater than {1,2,3} margin
;;; - initial-offset is negative, zero or positive number
;;; - final margin is a negative number, zero , lesser than initial-offset, equal, grater
;;; - width-fn is a number or a fixed width function
;;; - start boundary is 0, is first split, is last split
;;; - end boundary is first split, is last split
;;; - width/margin dimensions are scaled by 1/2, 1, 3 multiplier
;;;
;;; Not tested here (see demos with wrapping):
;;; - width-fn is a function with a not fixed character size (not tested here)
;;; - start boundary is right before/after {1,2,...,n-1,n} split
;;; - end boundary is just before/after {1,2,3,..,n-1,n} split
(test %line-breaks-1.smoke
  "Smoke test for the %LINE-BREAKS-1 function."
  (flet ((tbl (len off mar &rest splits)
           (let* ((str (make-string len))
                  (fix-fn (lambda (string start end)
                            (declare (ignore string))
                            (- end start)))
                  (f/2-fn (lambda (string start end)
                            (/ (funcall fix-fn string start end) 2)))
                  (3*f-fn (lambda (string start end)
                            (* (funcall fix-fn string start end) 3))))
             (is (equal (climi::%line-breaks-1 str 1      off mar 0 len) splits) nil)
             (is (equal (climi::%line-breaks-1 str fix-fn off mar 0 len) splits) nil)
             (is (equal (climi::%line-breaks-1 str 1/2    (/ off 2) (/ mar 2) 0 len) splits))
             (is (equal (climi::%line-breaks-1 str f/2-fn (/ off 2) (/ mar 2) 0 len) splits))
             (is (equal (climi::%line-breaks-1 str 3      (* off 3) (* mar 3) 0 len) splits))
             (is (equal (climi::%line-breaks-1 str 3*f-fn (* off 3) (* mar 3) 0 len) splits))
             (alexandria:when-let ((first-split (car splits)))
               (let ((start* first-split)
                     (end* len)
                     (splits* (cdr splits)))
                 ;; after the first split there are no offsets
                 (is (equal (climi::%line-breaks-1 str 1      0 mar       start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str fix-fn 0 mar       start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str 1/2    0 (/ mar 2) start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str f/2-fn 0 (/ mar 2) start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str 3      0 (* mar 3) start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str 3*f-fn 0 (* mar 3) start* end*) splits*))))
             (alexandria:when-let ((last-split (car (last splits))))
               (let ((start* last-split)
                     (end* len)
                     (splits* nil))
                 ;; after the first split there are no offsets
                 (is (equal (climi::%line-breaks-1 str 1      0 mar       start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str fix-fn 0 mar       start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str 1/2    0 (/ mar 2) start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str f/2-fn 0 (/ mar 2) start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str 3      0 (* mar 3) start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str 3*f-fn 0 (* mar 3) start* end*) splits*))))
             (alexandria:when-let ((last-split (car (last splits))))
               (let ((start* 0)
                     (end* last-split)
                     (splits* (butlast splits)))
                 (is (equal (climi::%line-breaks-1 str 1      off       mar       start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str fix-fn off       mar       start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str 1/2    (/ off 2) (/ mar 2) start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str f/2-fn (/ off 2) (/ mar 2) start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str 3      (* off 3) (* mar 3) start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str 3*f-fn (* off 3) (* mar 3) start* end*) splits*))))
             (alexandria:when-let ((start* (car splits))
                                   (end* (car (last splits))))
               (let ((splits* (cdr (butlast splits))))
                 (is (equal (climi::%line-breaks-1 str 1      0 mar       start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str fix-fn 0 mar       start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str 1/2    0 (/ mar 2) start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str f/2-fn 0 (/ mar 2) start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str 3      0 (* mar 3) start* end*) splits*))
                 (is (equal (climi::%line-breaks-1 str 3*f-fn 0 (* mar 3) start* end*) splits*)))))))
    (tbl 0     0   -3)
    (tbl 1     0   -3)
    (tbl 2     0   -3    1)
    (tbl 2    -5   -3)
    (tbl 2    -4   -3    1)
    (tbl 2    -3   -3    1)
    (tbl 2    -2   -3    1)
    (tbl 4    -2   -3    1 2 3)
    (tbl 4    -2    1    3)
    (tbl 4    -1    1    2 3)
    (tbl 4     0    0    1 2 3)
    (tbl 4     1    0    0 1 2 3)
    (tbl 4     1    1    0 1 2 3)
    (tbl 4     1    2    1 3)
    (tbl 0     0   80)
    (tbl 10    0   80)
    (tbl 10    1   80)
    (tbl 10   -1   80)
    (tbl 10   70   80)
    (tbl 10   71   80    9)
    (tbl 90    0   80    80)
    (tbl 90    1   80    79)
    (tbl 90  -10   80)
    (tbl 90   -9   80    89)
    (tbl 90   79   80    1 81)
    (tbl 90   80   80    0 80)
    (tbl 90   81   80    0 80)
    (tbl 800  -20  80    100 180 260 340 420 500 580 660 740)
    (tbl 800  20   80    60 140 220 300 380 460 540 620 700 780)
    (tbl 800  40   80    40 120 200 280 360 440 520 600 680 760)
    (tbl 800  60   80    20 100 180 260 340 420 500 580 660 740)))

(defun list-lines (string start breaks)
  (climi::collect (line)
    (do ((start start (car breaks))
         (breaks breaks (rest breaks)))
        ((null breaks)
         (line (string-trim '(#\space) (subseq string start))))
      (line (string-trim '(#\space) (subseq string start (car breaks)))))))

(defun lines (string start margin &optional (offset 0))
  (list-lines string start
              (climi::line-breaks string 1 :margin margin :initial-offset offset :start start)))

(test line-breaks.smoke
  (let ((string "ala ma kota a kot ma alę")
        (string2 "Xala ma kota a kot ma alę"))
    ;; We skip #\space at the line end (lines after break start with a character
    ;; which is not a space). LINES trims spaces from the right.
    (is (equal '("ala" "m" "a" "k" "o" "t" "a" "a" "k" "o" "t" "m" "a" "a" "l" "ę") (lines string 0 0 -3)))
    (is (equal '("a" "l" "a" "m" "a" "k" "o" "t" "a" "a" "k" "o" "t" "m" "a" "a" "l" "ę") (lines string 0 0)))
    (is (equal '("a" "l" "a" "m" "a" "k" "o" "t" "a" "a" "k" "o" "t" "m" "a" "a" "l" "ę") (lines string 0 1)))
    (is (equal '("al" "a" "ma" "ko" "ta" "a" "ko" "t" "ma" "al" "ę") (lines string 0 2)))
    (is (equal '("ala" "ma" "kot" "a a" "kot" "ma" "alę") (lines string 0 3)))
    (is (equal '("ala" "ma" "kota" "a" "kot" "ma" "alę") (lines string 0 4)))
    (is (equal '("ala" "ma" "kota" "a kot" "ma" "alę") (lines string 0 5)))
    (is (equal '("ala ma" "kota a" "kot ma" "alę") (lines string 0 6)))
    (is (equal '("ala" "ma" "kota a" "kot ma" "alę") (lines string 0 6 3)))
    (is (equal (list string) (lines string 0 64)))
    (is (equal (list string) (lines string 0 64 3)))
    ;; ignore first character
    (is (equal '("ala" "m" "a" "k" "o" "t" "a" "a" "k" "o" "t" "m" "a" "a" "l" "ę") (lines string2 1 0 -3)))
    (is (equal '("a" "l" "a" "m" "a" "k" "o" "t" "a" "a" "k" "o" "t" "m" "a" "a" "l" "ę") (lines string2 1 0)))
    (is (equal '("a" "l" "a" "m" "a" "k" "o" "t" "a" "a" "k" "o" "t" "m" "a" "a" "l" "ę") (lines string2 1 1)))
    (is (equal '("al" "a" "ma" "ko" "ta" "a" "ko" "t" "ma" "al" "ę") (lines string2 1 2)))
    (is (equal '("ala" "ma" "kot" "a a" "kot" "ma" "alę") (lines string2 1 3)))
    (is (equal '("ala" "ma" "kota" "a" "kot" "ma" "alę") (lines string2 1 4)))
    (is (equal '("ala" "ma" "kota" "a kot" "ma" "alę") (lines string2 1 5)))
    (is (equal '("ala ma" "kota a" "kot ma" "alę") (lines string2 1 6)))
    (is (equal '("ala" "ma" "kota a" "kot ma" "alę") (lines string2 1 6 3)))))

;;; for interactive testing
(defun print-lines (string start margin &optional (offset 0) bstr
                                        &aux (pre (if (>= offset 0)
                                                      ""
                                                      (make-string (abs offset) :initial-element #\space))))
  (princ pre)
  (princ "|")
  (dotimes (i margin)
    (princ "-"))
  (princ "|")
  (let ((lines (list-lines string start
                           (climi::line-breaks string 1 :margin margin
                                                        :initial-offset offset
                                                        :start start
                                                        :break-strategy bstr)))
        (raggedness 0)
        (last-score 0))
    (format t "~16tLine width: ~s" margin)
    (flet ((pline (line off margin &aux (counter 0))
             (dotimes (i off) (princ #\space))
             (princ line)
             (dotimes (i (- margin (length line) off))
               (incf counter)
               (princ #\space))
             (princ "|")
             (format t "~16tRemaining space: ~s" counter)
             (setf last-score (expt counter 2))
             (incf raggedness last-score)))
      (terpri)
      (princ "|")
      (pline (first lines) offset margin)
      (dolist (line (rest lines))
        (terpri)
        (princ pre)
        (princ "|")
        (pline line 0 margin))
      (princ " (ignored)")
      (terpri)
      (princ pre)
      (princ "|")
      (dotimes (i margin)
        (princ "-"))
      (princ "|")
      (decf raggedness last-score)
      (format t "~16tLines: ~s; Raggedness: ~s"  (length lines) raggedness))))
