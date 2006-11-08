;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :drei-tests)

(defmultitest previous-line.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 16)
      (previous-line mark nil 2)
      (offset mark)))
  0)

(defmultitest previous-line.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 19)
      (previous-line mark 2 2)
      (offset mark)))
  2)

(defmultitest previous-line.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 7)
      (previous-line mark)
      (offset mark)))
  7)

(defmultitest previous-line.test-4
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 7)
      (previous-line mark 2)
      (offset mark)))
  2)

(defmultitest previous-line.test-5
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 0)
      (previous-line mark)
      (offset mark)))
  0)

(defmultitest previous-line.test-6
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 0)
      (previous-line mark 2)
      (offset mark)))
  2)

(defmultitest previous-line.test-7
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs2")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 15)
      (previous-line mark)
      (offset mark)))
  7)

(defmultitest next-line.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 6)
      (next-line mark nil 2)
      (offset mark)))
  22)

(defmultitest next-line.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 3)
      (next-line mark 2 2)
      (offset mark)))
  18)

(defmultitest next-line.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 8)
      (next-line mark)
      (offset mark)))
  8)

(defmultitest next-line.test-4
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 8)
      (next-line mark 2)
      (offset mark)))
  10)

(defmultitest next-line.test-5
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 15)
      (next-line mark)
      (offset mark)))
  15)

(defmultitest next-line.test-6
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 15)
      (next-line mark 2)
      (offset mark)))
  10)

(defmultitest next-line.test-7
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 0)
      (next-line mark)
      (offset mark)))
  8)

(defmultitest open-line.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 0)
      (open-line mark 2)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "

climacs" 0)

(defmultitest open-line.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 0)
      (open-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "
climacs" 0)

(defmultitest open-line.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 7)
      (open-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "climacs
" 7)

(defmultitest open-line.test-4
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 7)
      (open-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "climacs
" 7)

(defmultitest delete-line.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 0)
      (delete-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  #() 0)

(defmultitest delete-line.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 0)
      (delete-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  #() 0)

(defmultitest delete-line.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 7)
      (delete-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "climacs" 7)

(defmultitest delete-line.test-4
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 7)
      (delete-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "climacs" 7)

(defmultitest delete-line.test-5
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 7)
      (delete-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "climacsclimacs" 7)

(defmultitest delete-line.test-6
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 7)
      (delete-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "climacsclimacs" 7)

(defmultitest empty-line-p.test-1
  (let* ((buffer (make-instance %%buffer))
	 (m1 (clone-mark (low-mark buffer) :left))
	 (m2 (clone-mark (low-mark buffer) :right)))
    (values (empty-line-p m1) (empty-line-p m2)))
  t t)

(defmultitest empty-line-p.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-object buffer 0 #\a)
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right)))
      (values (empty-line-p m1) (empty-line-p m2))))
  nil nil)

(defmultitest empty-line-p.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-object buffer 0 #\a)
    (let ((m1 (clone-mark (high-mark buffer) :left))
	  (m2 (clone-mark (high-mark buffer) :right)))
      (values (empty-line-p m1) (empty-line-p m2))))
  nil nil)

(defmultitest empty-line-p.test-4
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "a
b")
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1
	    (offset m2) 1)
      (values (empty-line-p m1) (empty-line-p m2))))
  nil nil)

(defmultitest line-indentation.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "  	climacs")
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right))
	  (m3 (clone-mark (low-mark buffer) :left))
	  (m4 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 0
	    (offset m2) 0
	    (offset m3) 10
	    (offset m4) 10)
      (values
       (line-indentation m1 8)
       (line-indentation m2 8)
       (line-indentation m3 8)
       (line-indentation m4 8)
       (offset m1)
       (offset m2)
       (offset m3)
       (offset m4))))
  10 10 10 10 0 0 10 10)

(defmultitest line-indentation.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "  		climacs")
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right))
	  (m3 (clone-mark (low-mark buffer) :left))
	  (m4 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 0
	    (offset m2) 0
	    (offset m3) 11
	    (offset m4) 11)
      (values
       (line-indentation m1 8)
       (line-indentation m2 8)
       (line-indentation m3 8)
       (line-indentation m4 8)
       (offset m1)
       (offset m2)
       (offset m3)
       (offset m4))))
  18 18 18 18 0 0 11 11)

(defmultitest line-indentation.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "  	climacs	")
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right))
	  (m3 (clone-mark (low-mark buffer) :left))
	  (m4 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 0
	    (offset m2) 0
	    (offset m3) 11
	    (offset m4) 11)
      (values
       (line-indentation m1 8)
       (line-indentation m2 8)
       (line-indentation m3 8)
       (line-indentation m4 8)
       (offset m1)
       (offset m2)
       (offset m3)
       (offset m4))))
  10 10 10 10 0 0 11 11)

(defmultitest buffer-number-of-lines-in-region.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (print (drei-buffer::buffer-line-number buffer 15))
    (values
     (drei-base::buffer-number-of-lines-in-region buffer 0 6)
     (drei-base::buffer-number-of-lines-in-region buffer 0 7)
     (drei-base::buffer-number-of-lines-in-region buffer 0 8)
     (drei-base::buffer-number-of-lines-in-region buffer 0 10)
     (drei-base::buffer-number-of-lines-in-region buffer 0 13)
     (drei-base::buffer-number-of-lines-in-region buffer 0 14)
     (drei-base::buffer-number-of-lines-in-region buffer 7 10)
     (drei-base::buffer-number-of-lines-in-region buffer 8 13)
     (drei-base::buffer-number-of-lines-in-region buffer 8 14)))
    0 0 1 1 1 1 1 0 0)

(defmultitest buffer-display-column.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "		cli	macs")
    (values
     (buffer-display-column buffer 0 8)
     (buffer-display-column buffer 1 8)
     (buffer-display-column buffer 2 8)
     (buffer-display-column buffer 5 8)
     (buffer-display-column buffer 6 8)))
  0 8 16 19 24)

(defmultitest number-of-lines-in-region.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "
climacs
climacs
")
    (let ((m1l (clone-mark (low-mark buffer) :left))
	  (m1r (clone-mark (low-mark buffer) :right))
	  (m2l (clone-mark (low-mark buffer) :left))
	  (m2r (clone-mark (low-mark buffer) :right))
	  (m3l (clone-mark (low-mark buffer) :left))
	  (m3r (clone-mark (low-mark buffer) :right))
	  (m4l (clone-mark (low-mark buffer) :left))
	  (m4r (clone-mark (low-mark buffer) :right))
	  (m5l (clone-mark (low-mark buffer) :left))
	  (m5r (clone-mark (low-mark buffer) :right))
	  (m6l (clone-mark (low-mark buffer) :left))
	  (m6r (clone-mark (low-mark buffer) :right)))
      (setf (offset m1l) 0
	    (offset m1r) 0
	    (offset m2l) 1
	    (offset m2r) 1
	    (offset m3l) 3
	    (offset m3r) 3
	    (offset m4l) 8
	    (offset m4r) 8
	    (offset m5l) 15
	    (offset m5r) 15
	    (offset m6l) 16
	    (offset m6r) 16)
      (values
       (number-of-lines-in-region m1l m1r)
       (number-of-lines-in-region m1r m1l)
       (number-of-lines-in-region m1l m2l)
       (number-of-lines-in-region m2r m1r)
       (number-of-lines-in-region m1l m2r)
       (number-of-lines-in-region m2r m1l)
       (number-of-lines-in-region m1r m2l)
       (number-of-lines-in-region m1l m3l)
       (number-of-lines-in-region m1r m3r)
       (number-of-lines-in-region m4r m1l)
       (number-of-lines-in-region m4l m1r)
       (number-of-lines-in-region m3l m5l)
       (number-of-lines-in-region m5r m4r)
       (number-of-lines-in-region m5l m6l)
       (number-of-lines-in-region m6r m5r)
       (number-of-lines-in-region m6l m6r)
       (number-of-lines-in-region m1l m6r)
       (number-of-lines-in-region m3r m6l))))
  0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 2 1)

(defmultitest number-of-lines-in-region.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m1l (clone-mark (low-mark buffer) :left))
	  (m1r (clone-mark (low-mark buffer) :right))
	  (m2l (clone-mark (low-mark buffer) :left))
	  (m2r (clone-mark (low-mark buffer) :right)))
      (setf (offset m1l) 6
	    (offset m1r) 6
	    (offset m2l) 7
	    (offset m2r) 7)
      (values
       (number-of-lines-in-region m1l 10)
       (number-of-lines-in-region 10 m1l)
       (number-of-lines-in-region m1r 10)
       (number-of-lines-in-region 10 m1r)
       (number-of-lines-in-region m1l 3)
       (number-of-lines-in-region 3 m2l)
       (number-of-lines-in-region 3 m2r)
       (number-of-lines-in-region m2l 10)
       (number-of-lines-in-region 10 m2r))))
  1 1 1 1 0 0 0 1 1)

(defmultitest constituentp.test-1 ; NOTE: more tests may be needed for sbcl
  (values
   (constituentp #\a)
   (constituentp #\Newline)
   (constituentp #\Space)
   (constituentp #\Tab)
   (constituentp "a")
   (constituentp #\Null))
  t nil nil nil nil #-sbcl nil #+sbcl t)

(defmultitest buffer-whitespacep.test-1
  (values
   (not (null (buffer-whitespacep #\a)))
   (not (null (buffer-whitespacep #\Newline)))
   (not (null (buffer-whitespacep #\Space)))
   (not (null (buffer-whitespacep #\Tab)))
   (not (null (buffer-whitespacep " ")))
   (not (null (buffer-whitespacep #\Null))))
  nil t t t nil nil)

;; Words are not recognized by DREI-BASE, setup syntax-aware
;; tests. Until then, these are disabled.
#||
(defmultitest forward-to-word-boundary.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "  climacs
climacs")
    (let ((m0l (clone-mark (low-mark buffer) :left))
	  (m0r (clone-mark (low-mark buffer) :right))
	  (m1l (clone-mark (low-mark buffer) :left))
	  (m1r (clone-mark (low-mark buffer) :right))
	  (m2l (clone-mark (low-mark buffer) :left))
	  (m2r (clone-mark (low-mark buffer) :right)))
      (setf (offset m0l) 0
	    (offset m0r) 0
	    (offset m1l) 5
	    (offset m1r) 5
	    (offset m2l) 17
	    (offset m2r) 17)
      (values
       (progn (drei-base::forward-to-word-boundary m0l) (offset m0l))
       (progn (drei-base::forward-to-word-boundary m0r) (offset m0r))
       (progn (drei-base::forward-to-word-boundary m1l) (offset m1l))
       (progn (drei-base::forward-to-word-boundary m1r) (offset m1r))
       (progn (drei-base::forward-to-word-boundary m2l) (offset m2l))
       (progn (drei-base::forward-to-word-boundary m2r) (offset m2r)))))
  2 2 5 5 17 17)

(defmultitest backward-to-word-boundary.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs  ")
    (let ((m0l (clone-mark (low-mark buffer) :left))
	  (m0r (clone-mark (low-mark buffer) :right))
	  (m1l (clone-mark (low-mark buffer) :left))
	  (m1r (clone-mark (low-mark buffer) :right))
	  (m2l (clone-mark (low-mark buffer) :left))
	  (m2r (clone-mark (low-mark buffer) :right)))
      (setf (offset m0l) 17
	    (offset m0r) 17
	    (offset m1l) 10
	    (offset m1r) 10
	    (offset m2l) 0
	    (offset m2r) 0)
      (values
       (progn (drei-base::backward-to-word-boundary m0l) (offset m0l))
       (progn (drei-base::backward-to-word-boundary m0r) (offset m0r))
       (progn (drei-base::backward-to-word-boundary m1l) (offset m1l))
       (progn (drei-base::backward-to-word-boundary m1r) (offset m1r))
       (progn (drei-base::backward-to-word-boundary m2l) (offset m2l))
       (progn (drei-base::backward-to-word-boundary m2r) (offset m2r)))))
  15 15 10 10 0 0)

(defmultitest forward-word.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "  climacs
climacs")
    (let ((m0l (clone-mark (low-mark buffer) :left))
	  (m0r (clone-mark (low-mark buffer) :right))
	  (m1l (clone-mark (low-mark buffer) :left))
	  (m1r (clone-mark (low-mark buffer) :right))
	  (m2l (clone-mark (low-mark buffer) :left))
	  (m2r (clone-mark (low-mark buffer) :right)))
      (setf (offset m0l) 0
	    (offset m0r) 0
	    (offset m1l) 5
	    (offset m1r) 15
	    (offset m2l) 17
	    (offset m2r) 17)
      (values
       (progn (forward-word m0l) (offset m0l))
       (progn (forward-word m0r) (offset m0r))
       (progn (forward-word m1l) (offset m1l))
       (progn (forward-word m1r) (offset m1r))
       (progn (forward-word m2l) (offset m2l))
       (progn (forward-word m2r) (offset m2r)))))
  9 9 9 17 17 17)

(defmultitest backward-word.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs  ")
    (let ((m0l (clone-mark (low-mark buffer) :left))
	  (m0r (clone-mark (low-mark buffer) :right))
	  (m1l (clone-mark (low-mark buffer) :left))
	  (m1r (clone-mark (low-mark buffer) :right))
	  (m2l (clone-mark (low-mark buffer) :left))
	  (m2r (clone-mark (low-mark buffer) :right)))
      (setf (offset m0l) 17
	    (offset m0r) 17
	    (offset m1l) 10
	    (offset m1r) 5
	    (offset m2l) 0
	    (offset m2r) 0)
      (values
       (progn (backward-word m0l) (offset m0l))
       (progn (backward-word m0r) (offset m0r))
       (progn (backward-word m1l) (offset m1l))
       (progn (backward-word m1r) (offset m1r))
       (progn (backward-word m2l) (offset m2l))
       (progn (backward-word m2r) (offset m2r)))))
  8 8 8 0 0 0)

(defmultitest delete-word.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 3)
      (delete-word m)
      (values
       (buffer-sequence buffer 0 (size buffer))
       (offset m))))
  "cli" 3)

(defmultitest delete-word.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "  climacs climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 0)
      (delete-word m 2)
      (values
       (buffer-sequence buffer 0 (size buffer))
       (offset m))))
  #() 0)

(defmultitest backward-delete-word.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 3)
      (backward-delete-word m)
      (values
       (buffer-sequence buffer 0 (size buffer))
       (offset m))))
  "macs" 0)

(defmultitest backward-delete-word.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs climacs  ")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 17)
      (backward-delete-word m 2)
      (values
       (buffer-sequence buffer 0 (size buffer))
       (offset m))))
  #() 0)

(defmultitest previous-word.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs climacs")
    (let ((m0 (clone-mark (low-mark buffer) :right))
	  (m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m0) 7
	    (offset m1) 8
	    (offset m2) 10)
      (values
       (drei-base::previous-word m0)
       (drei-base::previous-word m1)
       (drei-base::previous-word m2))))
  "climacs" #() "cl")
||#

(defmultitest downcase-buffer-region.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "CLi	mac5")
    (drei-base::downcase-buffer-region buffer 1 (size buffer))
    (buffer-sequence buffer 0 (size buffer)))
  "Cli	mac5")

(defmultitest downcase-region.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1
	    (offset m2) 8)
      (downcase-region m2 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_cli	mac5_")

(defmultitest downcase-region.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1)
      (downcase-region 8 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_cli	mac5_")

(defmultitest downcase-region.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (clone-mark (low-mark buffer) :left)))
      (setf (offset m1) 8)
      (downcase-region 1 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_cli	mac5_")

#+(or)(defmultitest downcase-word.test-1
          (let ((buffer (make-instance %%buffer)))
            (insert-buffer-sequence buffer 0 "CLI MA CS CLIMACS")
            (let ((m (clone-mark (low-mark buffer) :right)))
              (setf (offset m) 0)
              (downcase-word m 3)
              (values
               (buffer-sequence buffer 0 (size buffer))
               (offset m))))
        "cli ma cs CLIMACS" 9)

(defmultitest upcase-buffer-region.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "cli	mac5")
    (drei-base::upcase-buffer-region buffer 1 (size buffer))
    (buffer-sequence buffer 0 (size buffer)))
  "cLI	MAC5")

(defmultitest upcase-region.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1
	    (offset m2) 8)
      (upcase-region m2 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_CLI	MAC5_")

(defmultitest upcase-region.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1)
      (upcase-region 8 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_CLI	MAC5_")

(defmultitest upcase-region.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (clone-mark (low-mark buffer) :left)))
      (setf (offset m1) 8)
      (upcase-region 1 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_CLI	MAC5_")

#+(or)(defmultitest upcase-word.test-1
          (let ((buffer (make-instance %%buffer)))
            (insert-buffer-sequence buffer 0 "cli ma cs climacs")
            (let ((m (clone-mark (low-mark buffer) :right)))
              (setf (offset m) 0)
              (upcase-word m 3)
              (values
               (buffer-sequence buffer 0 (size buffer))
               (offset m))))
        "CLI MA CS climacs" 9)

(defmultitest capitalize-buffer-region.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "cli ma cs")
    (drei-base::capitalize-buffer-region buffer 1 (size buffer))
    (buffer-sequence buffer 0 (size buffer)))
  "cLi Ma Cs")

(defmultitest capitalize-buffer-region.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "CLI mA Cs")
    (drei-base::capitalize-buffer-region buffer 0 (size buffer))
    (buffer-sequence buffer 0 (size buffer)))
  "Cli Ma Cs")

(defmultitest capitalize-region.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1
	    (offset m2) 8)
      (capitalize-region m2 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_Cli	Mac5_")

(defmultitest capitalize-region.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1)
      (capitalize-region 8 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_Cli	Mac5_")

(defmultitest capitalize-region.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (clone-mark (low-mark buffer) :left)))
      (setf (offset m1) 8)
      (capitalize-region 1 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_Cli	Mac5_")

#+(or)(defmultitest capitalize-word.test-1
          (let ((buffer (make-instance %%buffer)))
            (insert-buffer-sequence buffer 0 "cli ma cs climacs")
            (let ((m (clone-mark (low-mark buffer) :right)))
              (setf (offset m) 0)
              (capitalize-word m 3)
              (values
               (buffer-sequence buffer 0 (size buffer))
               (offset m))))
        "Cli Ma Cs climacs" 9)

(defmultitest tabify-buffer-region.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "c       l       im              acs")
    (drei-base::tabify-buffer-region buffer 0 (size buffer) 8)
    (buffer-sequence buffer 0 (size buffer)))
  "c	l	im		acs")

(defmultitest tabify-buffer-region.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "c      l       im              acs")
    (drei-base::tabify-buffer-region buffer 0 (size buffer) 8)    
    (buffer-sequence buffer 0 (size buffer)))
  "c      l       im	       acs")

(defmultitest tabify-region.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "clim    acs")
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 3
	    (offset m2) 7)
      (tabify-region m2 m1 4)
      (buffer-sequence buffer 0 (size buffer))))
  "clim	acs")

(defmultitest tabify-region.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "clim    acs")
    (let ((m1 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 3)
      (tabify-region 7 m1 4)
      (buffer-sequence buffer 0 (size buffer))))
  "clim	acs")

(defmultitest tabify-region.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "clim    acs")
    (let ((m1 (clone-mark (low-mark buffer) :left)))
      (setf (offset m1) 7)
      (tabify-region 3 m1 4)
      (buffer-sequence buffer 0 (size buffer))))
  "clim	acs")

(defmultitest untabify-buffer-region.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "c	l	im		acs")
    (drei-base::untabify-buffer-region buffer 0 (size buffer) 8)
    (buffer-sequence buffer 0 (size buffer)))
  "c       l       im              acs")

(defmultitest untabify-buffer-region.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "c      l       im	       acs")
    (drei-base::untabify-buffer-region buffer 0 (size buffer) 8)    
    (buffer-sequence buffer 0 (size buffer)))
  "c      l       im              acs")

(defmultitest untabify-region.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "clim	acs")
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 3
	    (offset m2) 5)
      (untabify-region m2 m1 4)
      (buffer-sequence buffer 0 (size buffer))))
  "clim    acs")

(defmultitest untabify-region.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "clim	acs")
    (let ((m1 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 3)
      (untabify-region 5 m1 4)
      (buffer-sequence buffer 0 (size buffer))))
  "clim    acs")

(defmultitest untabify-region.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "clim	acs")
    (let ((m1 (clone-mark (low-mark buffer) :left)))
      (setf (offset m1) 5)
      (untabify-region 3 m1 4)
      (buffer-sequence buffer 0 (size buffer))))
  "clim    acs")

(defmultitest indent-line.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "  	climacs   ")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 3)
      (indent-line m 4 nil)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  0 "    climacs   ")

(defmultitest indent-line.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "  	climacs   ")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 4)
      (indent-line m 5 4)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  3 "	 climacs   ")

(defmultitest indent-line.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "  	climacs   ")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (indent-line m 5 4)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  2 "	 climacs   ")

(defmultitest delete-indentation.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "
  	climacs   ")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 3)
      (delete-indentation m)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  0 "climacs   ")

(defmultitest delete-indentation.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "
  	climacs   ")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 7)
      (delete-indentation m)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  0 "climacs   ")

(defmultitest delete-indentation.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "   climacs   ")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 7)
      (delete-indentation m)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  0 "   climacs   ")

(defmultitest delete-indentation.test-4
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
   climacs   ")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 12)
      (delete-indentation m)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  8 "climacs climacs   ")

(defmultitest delete-indentation.test-5
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "

   climacs   ")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 12)
      (delete-indentation m)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  0 "climacs   ")

(defmultitest fill-line.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs  climacs  climacs  climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 25)
      (fill-line m #'(lambda (m) (declare (ignore m)) 8) 10 8 t)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  25 "climacs
	climacs
	climacs  climacs")

(defmultitest fill-line.test-1a
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs  climacs  climacs  climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 25)
      (fill-line m #'(lambda (m) (declare (ignore m)) 8) 10 8 t nil)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  27 "climacs 
	climacs 
	climacs  climacs")

(defmultitest fill-line.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs	climacs	climacs	climacs")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 25)
      (fill-line m #'(lambda (m) (declare (ignore m)) 8) 10 8 t)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  27 "climacs
	climacs
	climacs	climacs")

(defmultitest fill-line.test-2a
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs	climacs	climacs	climacs")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 25)
      (fill-line m #'(lambda (m) (declare (ignore m)) 8) 10 8 nil)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  27 "climacs
	climacs
	climacs	climacs")

(defmultitest fill-line.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "c l i m a c s")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 1)
      (fill-line m #'(lambda (m) (declare (ignore m)) 8) 0 8 t)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  1 "c l i m a c s")

(defmultitest fill-line.test-3a
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "c l i m a c s")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 1)
      (fill-line m #'(lambda (m) (declare (ignore m)) 8) 0 8 t nil)
      (values
       (offset m)
       (buffer-sequence buffer 0 (size buffer)))))
  1 "c l i m a c s")

(defmultitest buffer-looking-at.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs ")
    (values
     (buffer-looking-at buffer 0 "climacs")
     (buffer-looking-at buffer 0 "CLIMACS" :test #'char-equal)
     (buffer-looking-at buffer 0 "")
     (buffer-looking-at buffer 8 "")
     (buffer-looking-at buffer 9 "")
     (buffer-looking-at buffer 10 "")))
  t t t t nil nil)

(defmultitest buffer-looking-at.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 " climacs")
    (buffer-looking-at buffer 0 "climacs"))
  nil)

(defmultitest buffer-looking-at.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climac")
    (buffer-looking-at buffer 0 "climacs"))
  nil)

(defmultitest looking-at.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1
	    (offset m2) 3)
      (values
       (looking-at m1 "lima")
       (looking-at m2 "mac")
       (looking-at m1 "lIMa" :test #'char-equal)
       (looking-at m2 "Mac" :test #'char-equal)
       (looking-at m1 "climacs")
       (looking-at m2 "climacs")
       (looking-at m1 "")
       (looking-at m2 "")
       (offset m1)
       (offset m2))))
  t t t t nil nil t t 1 3)

(defmultitest buffer-search-forward.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "
climacs")
    (values
     (buffer-search-forward buffer 0 "clim")
     (buffer-search-forward buffer 0 "CLIM" :test #'char-equal)
     (buffer-search-forward buffer 0 "macs")
     (buffer-search-forward buffer 0 "")
     (buffer-search-forward buffer 2 "clim")
     (buffer-search-forward buffer 8 "")
     (buffer-search-forward buffer 9 "")
     (buffer-search-forward buffer 10 "")))
  1 1 4 0 nil 8 nil nil)

(defmultitest buffer-search-backward.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
")
    (values
     (buffer-search-backward buffer 8 "macs")
     (buffer-search-backward buffer 8 "MACS" :test #'char-equal)
     (buffer-search-backward buffer 4 "clim")
     (buffer-search-backward buffer 8 "")
     (buffer-search-backward buffer 6 "macs")
     (buffer-search-backward buffer -1 "")
     (buffer-search-backward buffer 0 "")
     (buffer-search-backward buffer 1 "")))
  3 3 0 8 nil nil 0 1)

(defmultitest buffer-re-search-forward.test-1
  (let ((buffer (make-instance %%buffer))
	(a1 (automaton::determinize
	     (regexp-automaton (string-regexp "i[mac]+s"))))
	(a2 (automaton::determinize
	     (regexp-automaton (string-regexp "[^aeiou][aeiou]"))))
	(a3 (regexp-automaton (string-regexp "imacs"))))
    (insert-buffer-sequence buffer 0 "
climacs")
      (multiple-value-call
	  #'list
	(buffer-re-search-forward a1 buffer 0)
	(buffer-re-search-forward a2 buffer 1)
	(buffer-re-search-forward a3 buffer 1)
	(buffer-re-search-forward a1 buffer 4)
	(buffer-re-search-forward a2 buffer 6)
	(buffer-re-search-forward a3 buffer 6)))
  (3 8 2 4 3 8 nil nil nil))

(defmultitest buffer-re-search-backward.test-1
  (let ((buffer (make-instance %%buffer))
	(a1 (drei-base::reversed-deterministic-automaton
	     (regexp-automaton (string-regexp "i[ma]+c"))))
	(a2 (drei-base::reversed-deterministic-automaton
	     (regexp-automaton (string-regexp "[^aeiou][aeiou]"))))
	(a3 (regexp-automaton (string-regexp "cami"))))
    (insert-buffer-sequence buffer 0 "
climacs")
      (multiple-value-call
	  #'list
	(buffer-re-search-backward a1 buffer 7)
	(buffer-re-search-backward a2 buffer 7)
	(buffer-re-search-backward a3 buffer 7)
	(buffer-re-search-backward a1 buffer 5)
	(buffer-re-search-backward a2 buffer 2)
	(buffer-re-search-backward a3 buffer 5)))
  (3 7 4 6 3 7 nil nil nil))

(defmultitest search-forward.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "
climacs")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 0)
      (search-forward m "Mac" :test #'char-equal)
      (offset m)))
  7)

(defmultitest search-forward.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (search-forward m "Mac" :test #'char-equal)
      (offset m)))
  6)

(defmultitest search-forward.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (search-forward m "klimaks")
      (offset m)))
  3)

(defmultitest search-backward.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 8)
      (search-backward m "Mac" :test #'char-equal)
      (offset m)))
  3)

(defmultitest search-backward.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 6)
      (search-backward m "Mac" :test #'char-equal)
      (offset m)))
  3)

(defmultitest search-backward.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (search-backward m "klimaks")
      (offset m)))
  3)

(defmultitest re-search-forward.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "
climacs")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 0)
      (re-search-forward m "[mac]{3}")
      (offset m)))
  7)

(defmultitest re-search-forward.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (re-search-forward m "[mac]{3}")
      (offset m)))
  6)

(defmultitest re-search-forward.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (re-search-forward m "klimaks")
      (offset m)))
  3)

(defmultitest re-search-backward.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 8)
      (re-search-backward m "[mac]{3}")
      (offset m)))
  3)

(defmultitest re-search-backward.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 6)
      (re-search-backward m "[mac]{3}")
      (offset m)))
  3)

(defmultitest re-search-backward.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (re-search-backward m "klimaks")
      (offset m)))
  3)

#+(or)(defmultitest buffer-search-word-forward.test-1
          (let ((buffer (make-instance %%buffer)))
            (insert-buffer-sequence buffer 0 "
 climacs")
            (values
             (drei-base::buffer-search-word-forward buffer 0 "climacs")
             (drei-base::buffer-search-word-forward buffer 3 "climacs")
             (drei-base::buffer-search-word-forward buffer 0 "clim")
             (drei-base::buffer-search-word-forward buffer 5 "macs")
             (drei-base::buffer-search-word-forward buffer 0 "")))
        2 nil nil nil 0)

#+(or)(defmultitest buffer-search-word-backward.test-1
          (let ((buffer (make-instance %%buffer)))
            (insert-buffer-sequence buffer 0 "climacs 
")
            (values
             (drei-base::buffer-search-word-backward buffer 8 "climacs")
             (drei-base::buffer-search-word-backward buffer 5 "climacs")
             (drei-base::buffer-search-word-backward buffer 4 "clim")
             (drei-base::buffer-search-word-backward buffer 8 "macs")
             (drei-base::buffer-search-word-backward buffer 8 "")))
        0 nil nil nil 8)