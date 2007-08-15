;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
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
;;;
;;; The test cases in this files test the functions of the DREI-BASE
;;; package built on top of the buffer protocol.

(cl:in-package :drei-tests)

(def-suite base-tests :description "The test suite for DREI-BASE
related tests." :in drei-tests)

(in-suite base-tests)

(buffer-test previous-line
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 16)
      (previous-line mark nil 2)
      (is (= (offset mark) 0))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 19)
      (previous-line mark 2 2)
      (is (= (offset mark) 2))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 7)
      (previous-line mark)
      (is (= (offset mark) 7))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 7)
      (previous-line mark 2)
      (is (= (offset mark) 2))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 0)
      (previous-line mark)
      (is (= (offset mark) 0))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 0)
      (previous-line mark 2)
      (is (= (offset mark) 2))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs2")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 15)
      (previous-line mark)
      (is (= (offset mark) 7)))))

(buffer-test next-line
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 6)
      (next-line mark nil 2)
      (is (= (offset mark) 22))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 3)
      (next-line mark 2 2)
      (is (= (offset mark) 18))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 8)
      (next-line mark)
      (is (= (offset mark) 8))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 8)
      (is (next-line mark 2))
      (= (offset mark) 10)))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 15)
      (next-line mark)
      (is (= (offset mark) 15))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 15)
      (next-line mark 2)
      (is (= (offset mark) 10))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 0)
      (next-line mark)
      (is (= (offset mark) 8)))))

(buffer-test open-line
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 0)
      (open-line mark 2)
      (is (string= (buffer-contents buffer)
                   #.(format nil "~%~%climacs")))
      (is (= (offset mark) 0))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 0)
      (open-line mark)
      (is (string= (buffer-contents buffer)
                   #. (format nil "~%climacs")))
      (is (= (offset mark) 0))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 7)
      (open-line mark)
      (is (string= (buffer-contents buffer)
                   #.(format nil "climacs~%")))
      (is (= (offset mark) 7))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 7)
      (open-line mark)
      (is (string= (buffer-contents buffer)
                   #.(format nil "climacs~%")))
      (is (= (offset mark) 7)))))


(buffer-test delete-line
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 0)
      (delete-line mark)
      (is (string= (buffer-contents buffer) ""))
      (is (= (offset mark) 0))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 0)
      (delete-line mark)
      (is (string= (buffer-contents buffer) ""))
      (is (= (offset mark) 0))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 7)
      (delete-line mark)
      (is (string= (buffer-contents buffer) "climacs"))
      (is (= (offset mark) 7))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 7)
      (delete-line mark)
      (values (buffer-contents buffer) (offset mark))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :left)))
      (setf (offset mark) 7)
      (delete-line mark)
      (is (string= (buffer-contents buffer) "climacsclimacs"))
      (is (= (offset mark) 7))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (clone-mark (low-mark buffer) :right)))
      (setf (offset mark) 7)
      (delete-line mark)
      (is (string= (buffer-contents buffer) "climacsclimacs"))
      (is (= (offset mark) 7)))))

(buffer-test empty-line-p
  (let* ((buffer (make-instance %%buffer))
	 (m1 (clone-mark (low-mark buffer) :left))
	 (m2 (clone-mark (low-mark buffer) :right)))
    (is-true (empty-line-p m1))
    (is-true (empty-line-p m2)))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-object buffer 0 #\a)
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right)))
      (is-false (empty-line-p m1))
      (is-false (empty-line-p m2))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-object buffer 0 #\a)
    (let ((m1 (clone-mark (high-mark buffer) :left))
          (m2 (clone-mark (high-mark buffer) :right)))
      (is-false (empty-line-p m1))
      (is-false (empty-line-p m2))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "a
b")
    (let ((m1 (clone-mark (low-mark buffer) :left))
          (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1
            (offset m2) 1)
      (is-false (empty-line-p m1))
      (is-false (empty-line-p m2)))))

(buffer-test line-indentation
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
      (is (= (line-indentation m1 8) 10))
      (is (= (line-indentation m2 8) 10))
      (is (= (line-indentation m3 8) 10))
      (is (= (line-indentation m4 8) 10))
      (is (= (offset m1) 0))
      (is (= (offset m2) 0))
      (is (= (offset m3) 10))
      (is (= (offset m4) 10))))
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
      (is (= (line-indentation m1 8) 18))
      (is (= (line-indentation m2 8) 18))
      (is (= (line-indentation m3 8) 18))
      (is (= (line-indentation m4 8) 18))
      (is (= (offset m1) 0))
      (is (= (offset m2) 0))
      (is (= (offset m3) 11))
      (is (= (offset m4) 11))))
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
      (is (= (line-indentation m1 8) 10))
      (is (= (line-indentation m2 8) 10))
      (is (= (line-indentation m3 8) 10))
      (is (= (line-indentation m4 8) 10))
      (is (= (offset m1) 0))
      (is (= (offset m2) 0))
      (is (= (offset m3) 11))
      (is (= (offset m4) 11)))))

(buffer-test buffer-number-of-lines-in-region
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (is (= (drei-base::buffer-number-of-lines-in-region buffer 0 6) 0))
    (is (= (drei-base::buffer-number-of-lines-in-region buffer 0 7) 0))
    (is (= (drei-base::buffer-number-of-lines-in-region buffer 0 8) 1))
    (is (= (drei-base::buffer-number-of-lines-in-region buffer 0 10) 1))
    (is (= (drei-base::buffer-number-of-lines-in-region buffer 0 13) 1))
    (is (= (drei-base::buffer-number-of-lines-in-region buffer 0 14) 1))
    (is (= (drei-base::buffer-number-of-lines-in-region buffer 7 10) 1))
    (is (= (drei-base::buffer-number-of-lines-in-region buffer 8 13) 0))
    (is (= (drei-base::buffer-number-of-lines-in-region buffer 8 14) 0))))

(buffer-test buffer-display-column
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "		cli	macs")
    (is (= (buffer-display-column buffer 0 8) 0))
    (is (= (buffer-display-column buffer 1 8) 8))
    (is (= (buffer-display-column buffer 2 8) 16))
    (is (= (buffer-display-column buffer 5 8) 19))
    (is (= (buffer-display-column buffer 6 8) 24))))

(buffer-test number-of-lines-in-region
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
      (is (= (number-of-lines-in-region m1l m1r) 0))
      (is (= (number-of-lines-in-region m1r m1l) 0))
      (is (= (number-of-lines-in-region m1l m2l) 1))
      (is (= (number-of-lines-in-region m2r m1r) 1))
      (is (= (number-of-lines-in-region m1l m2r) 1))
      (is (= (number-of-lines-in-region m2r m1l) 1))
      (is (= (number-of-lines-in-region m1r m2l) 1))
      (is (= (number-of-lines-in-region m1l m3l) 1))
      (is (= (number-of-lines-in-region m1r m3r) 1))
      (is (= (number-of-lines-in-region m4r m1l) 1))
      (is (= (number-of-lines-in-region m4l m1r) 1))
      (is (= (number-of-lines-in-region m3l m5l) 1))
      (is (= (number-of-lines-in-region m5r m4r) 1))
      (is (= (number-of-lines-in-region m5l m6l) 0))
      (is (= (number-of-lines-in-region m6r m5r) 0))
      (is (= (number-of-lines-in-region m6l m6r) 0))
      (is (= (number-of-lines-in-region m1l m6r) 2))
      (is (= (number-of-lines-in-region m3r m6l) 1))))
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
      (is (= (number-of-lines-in-region m1l 10) 1))
      (is (= (number-of-lines-in-region 10 m1l) 1))
      (is (= (number-of-lines-in-region m1r 10) 1))
      (is (= (number-of-lines-in-region 10 m1r) 1))
      (is (= (number-of-lines-in-region m1l 3) 0))
      (is (= (number-of-lines-in-region 3 m2l) 0))
      (is (= (number-of-lines-in-region 3 m2r) 0))
      (is (= (number-of-lines-in-region m2l 10) 1))
      (is (= (number-of-lines-in-region 10 m2r) 1)))))

(test constituentp           ; NOTE: more tests may be needed for sbcl
  (is-true (constituentp #\a))
  (is-false (constituentp #\Newline))
  (is-false (constituentp #\Space))
  (is-false (constituentp #\Tab))
  (is-false (constituentp "a"))
  (is-false (constituentp #\Null)))

(test buffer-whitespacep
  (is-false (buffer-whitespacep #\a))
  (is-true (buffer-whitespacep #\Newline))
  (is-true (buffer-whitespacep #\Space))
  (is-true (buffer-whitespacep #\Tab))
  (is-false (buffer-whitespacep " "))
  (is-false (buffer-whitespacep #\Null)))

(buffer-test downcase-buffer-region
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "CLi~Amac5" #\Tab))
    (drei-base::downcase-buffer-region buffer 1 (size buffer))
    (is (string= (buffer-contents buffer)
                 "Cli	mac5"))))

(buffer-test downcase-region
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "_CLi~Amac5_" #\Tab))
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1
	    (offset m2) 8)
      (downcase-region m2 m1)
      (is (string= (buffer-contents buffer)
                   #.(format nil "_cli~Amac5_" #\Tab)))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "_CLi~Amac5_" #\Tab))
    (let ((m1 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1)
      (downcase-region 8 m1)
      (is (string= (buffer-contents buffer)
                   #.(format nil "_cli~Amac5_" #\Tab)))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "_CLi~Amac5_" #\Tab))
    (let ((m1 (clone-mark (low-mark buffer) :left)))
      (setf (offset m1) 8)
      (downcase-region 1 m1)
      (is (string= (buffer-contents buffer)
                   #.(format nil "_cli~Amac5_" #\Tab))))))

(buffer-test upcase-buffer-region
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "_cli~Amac5_" #\Tab))
    (drei-base::upcase-buffer-region buffer 1 (size buffer))
    (is (string= (buffer-contents buffer)
                 #.(format nil "_CLI~AMAC5_" #\Tab)))))

(buffer-test upcase-region
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "_cli~Amac5_" #\Tab))
    (let ((m1 (clone-mark (low-mark buffer) :left))
	  (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1
	    (offset m2) 8)
      (upcase-region m2 m1)
      (is (string= (buffer-contents buffer)
                   #.(format nil "_CLI~AMAC5_" #\Tab)))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "_Cli~Amac5_" #\Tab))
    (let ((m1 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1)
      (upcase-region 8 m1)
      (is (string= (buffer-contents buffer)
                   #.(format nil "_CLI~AMAC5_" #\Tab)))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "_Cli~Amac5_" #\Tab))
    (let ((m1 (clone-mark (low-mark buffer) :left)))
      (setf (offset m1) 8)
      (upcase-region 1 m1)
      (is (string= (buffer-contents buffer)
                   #.(format nil "_CLI~AMAC5_" #\Tab))))))

(buffer-test capitalize-buffer-region
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "cli ma cs")
    (drei-base::capitalize-buffer-region buffer 1 (size buffer))
    (is (string= (buffer-contents buffer)
                 "cLi Ma Cs")))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "CLI mA Cs")
    (drei-base::capitalize-buffer-region buffer 0 (size buffer))
    (is (string= (buffer-contents buffer)
                 "Cli Ma Cs"))))

(buffer-test capitalize-region
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #. (format nil "_Cli~Amac5_" #\Tab))
    (let ((m1 (clone-mark (low-mark buffer) :left))
          (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1
            (offset m2) 8)
      (capitalize-region m2 m1)
      (is (string= (buffer-contents buffer)
                   #.(format nil "_Cli~AMac5_" #\Tab)))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "_Cli~Amac5_" #\Tab))
    (let ((m1 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1)
      (capitalize-region 8 m1)
      (is (string= (buffer-contents buffer)
                   #.(format nil "_Cli~AMac5_" #\Tab)))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "_Cli~Amac5_" #\Tab))
    (let ((m1 (clone-mark (low-mark buffer) :left)))
      (setf (offset m1) 8)
      (capitalize-region 1 m1)
      (is (string= (buffer-contents buffer)
                   #.(format nil "_Cli~AMac5_" #\Tab))))))

(buffer-test tabify-buffer-region
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "c       l       im              acs")
    (drei-base::tabify-buffer-region buffer 0 (size buffer) 8)
    (is (string= (buffer-contents buffer)
                 #.(format nil "c~Al~Aim~A~Aacs" #\Tab #\Tab #\Tab #\Tab))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "c      l       im              acs")
    (drei-base::tabify-buffer-region buffer 0 (size buffer) 8)    
    (is (string= (buffer-contents buffer)
                 #.(format nil "c      l       im~A       acs" #\Tab)))))

(buffer-test tabify-region
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "clim    acs")
    (let ((m1 (clone-mark (low-mark buffer) :left))
          (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 3
            (offset m2) 7)
      (tabify-region m2 m1 4)
      (is (string= (buffer-contents buffer)
                   #. (format nil "clim~Aacs" #\Tab)))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "clim    acs")
    (let ((m1 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 3)
      (tabify-region 7 m1 4)
      (is (string= (buffer-contents buffer)
                   #.(format nil "clim~Aacs" #\Tab)))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "clim    acs")
    (let ((m1 (clone-mark (low-mark buffer) :left)))
      (setf (offset m1) 7)
      (tabify-region 3 m1 4)
      (is (string= (buffer-contents buffer)
                   #.(format nil "clim~Aacs" #\Tab))))))

(buffer-test untabify-buffer-region
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "c~Al~Aim~A~Aacs" #\Tab #\Tab #\Tab #\Tab))
    (drei-base::untabify-buffer-region buffer 0 (size buffer) 8)
    (is (string= (buffer-contents buffer)
                 "c       l       im              acs")))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "c      l       im~A       acs" #\Tab))
    (drei-base::untabify-buffer-region buffer 0 (size buffer) 8)    
    (is (string= (buffer-contents buffer)
                 "c      l       im              acs")))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "clim~Aacs" #\Tab))
    (let ((m1 (clone-mark (low-mark buffer) :left))
          (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 3
            (offset m2) 5)
      (untabify-region m2 m1 4)
      (is (string= (buffer-contents buffer)
                   "clim    acs"))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "clim~Aacs" #\Tab))
    (let ((m1 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 3)
      (untabify-region 5 m1 4)
      (is (string= (buffer-contents buffer)
                   "clim    acs"))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "clim~Aacs" #\Tab))
    (let ((m1 (clone-mark (low-mark buffer) :left)))
      (setf (offset m1) 5)
      (untabify-region 3 m1 4)
      (is (string= (buffer-contents buffer)
                   "clim    acs")))))

(buffer-test indent-line
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "  ~Aclimacs   " #\Tab))
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 3)
      (indent-line m 4 nil)
      (is (= (offset m) 0))
      (is (string= (buffer-contents buffer)
                   "    climacs   "))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "  	climacs   ")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 4)
      (indent-line m 5 4)
      (is (= (offset m) 3))
      (is (string= (buffer-contents buffer)
                   #.(format nil "~A climacs   " #\Tab)))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "  	climacs   ")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (indent-line m 5 4)
      (is (= (offset m) 2))
      (is (string= (buffer-contents buffer)
                   #.(format nil "~A climacs   " #\Tab))))))

(buffer-test buffer-looking-at
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs ")
    (is-true (buffer-looking-at buffer 0 "climacs"))
    (is-true (buffer-looking-at buffer 0 "CLIMACS" :test #'char-equal))
    (is-true (buffer-looking-at buffer 0 ""))
    (is-true (buffer-looking-at buffer 8 ""))
    (is-false (buffer-looking-at buffer 9 ""))
    (is-false (buffer-looking-at buffer 10 "")))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 " climacs")
    (is-false (buffer-looking-at buffer 0 "climacs")))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climac")
    (is-false (buffer-looking-at buffer 0 "climacs")))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m1 (clone-mark (low-mark buffer) :left))
          (m2 (clone-mark (low-mark buffer) :right)))
      (setf (offset m1) 1
            (offset m2) 3)
      (is-true (looking-at m1 "lima"))
      (is-true (looking-at m2 "mac"))
      (is-true (looking-at m1 "lIMa" :test #'char-equal))
      (is-true (looking-at m2 "Mac" :test #'char-equal))
      (is-false (looking-at m1 "climacs"))
      (is-false (looking-at m2 "climacs"))
      (is-true (looking-at m1 ""))
      (is-true (looking-at m2 ""))
      (is (= (offset m1) 1))
      (is (= (offset m2) 3)))))

(buffer-test buffer-search-forward
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "
climacs")
    (is (= (buffer-search-forward buffer 0 "clim") 1))
    (is (= (buffer-search-forward buffer 0 "CLIM" :test #'char-equal) 1))
    (is (= (buffer-search-forward buffer 0 "macs") 4))
    (is (= (buffer-search-forward buffer 0 "") 0))
    (is-false (buffer-search-forward buffer 2 "clim"))
    (is (= (buffer-search-forward buffer 8 "") 8))
    (is-false (buffer-search-forward buffer 9 ""))
    (is-false (buffer-search-forward buffer 10 ""))))

(buffer-test buffer-search-backward
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
")
    (is (= (buffer-search-backward buffer 8 "macs") 3))
    (is (= (buffer-search-backward buffer 8 "MACS" :test #'char-equal) 3))
    (is (= (buffer-search-backward buffer 4 "clim") 0))
    (is (= (buffer-search-backward buffer 8 "") 8))
    (is-false (buffer-search-backward buffer 6 "macs"))
    (is-false (buffer-search-backward buffer -1 ""))
    (is (= (buffer-search-backward buffer 0 "") 0))
    (is (= (buffer-search-backward buffer 1 "") 1))))

(buffer-test buffer-re-search-forward
  (let ((buffer (make-instance %%buffer))
        (a1 (automaton::determinize
             (regexp-automaton (string-regexp "i[mac]+s"))))
        (a2 (automaton::determinize
             (regexp-automaton (string-regexp "[^aeiou][aeiou]"))))
        (a3 (regexp-automaton (string-regexp "imacs"))))
    (insert-buffer-sequence buffer 0 "
climacs")
    (is (equal (multiple-value-list (buffer-re-search-forward a1 buffer 0))
               '(3 8)))
    (is (equal (multiple-value-list (buffer-re-search-forward a2 buffer 1))
               '(2 4)))
    (is (equal (multiple-value-list (buffer-re-search-forward a3 buffer 1))
               '(3 8)))
    (is-false (buffer-re-search-forward a1 buffer 4))
    (is-false (buffer-re-search-forward a2 buffer 6))
    (is-false (buffer-re-search-forward a3 buffer 6))))

(buffer-test buffer-re-search-backward
  (let ((buffer (make-instance %%buffer))
        (a1 (drei-base::reversed-deterministic-automaton
             (regexp-automaton (string-regexp "i[ma]+c"))))
        (a2 (drei-base::reversed-deterministic-automaton
             (regexp-automaton (string-regexp "[^aeiou][aeiou]"))))
        (a3 (regexp-automaton (string-regexp "cami"))))
    (insert-buffer-sequence buffer 0 "
climacs")
    (is (equal (multiple-value-list (buffer-re-search-backward a1 buffer 7))
               '(3 7)))
    (is (equal (multiple-value-list (buffer-re-search-backward a2 buffer 7))
               '(4 6)))
    (is (equal (multiple-value-list (buffer-re-search-backward a3 buffer 7))
               '(3 7)))
    (is-false (buffer-re-search-backward a1 buffer 5))
    (is-false (buffer-re-search-backward a2 buffer 2))
    (is-false (buffer-re-search-backward a3 buffer 5))))

(buffer-test search-forward
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "
climacs")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 0)
      (search-forward m "Mac" :test #'char-equal)
      (is (= (offset m) 7))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (search-forward m "Mac" :test #'char-equal)
      (is (= (offset m) 6))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (search-forward m "klimaks")
      (is (= (offset m) 3)))))

(buffer-test search-backward
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 8)
      (search-backward m "Mac" :test #'char-equal)
      (is (= (offset m) 3))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 6)
      (search-backward m "Mac" :test #'char-equal)
      (is (= (offset m) 3))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (search-backward m "klimaks")
      (is (= (offset m) 3)))))

(buffer-test re-search-forward
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "
climacs")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 0)
      (re-search-forward m "[mac]{3}")
      (is (= (offset m) 7))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (re-search-forward m "[mac]{3}")
      (is (= (offset m) 6))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (re-search-forward m "klimaks")
      (is (= (offset m) 3)))))

(buffer-test re-search-backward
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
")
    (let ((m (clone-mark (low-mark buffer) :left)))
      (setf (offset m) 8)
      (re-search-backward m "[mac]{3}")
      (is (= (offset m) 3))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 6)
      (re-search-backward m "[mac]{3}")
      (is (= (offset m) 3))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (clone-mark (low-mark buffer) :right)))
      (setf (offset m) 3)
      (re-search-backward m "klimaks")
      (is (= (offset m) 3)))))

(buffer-test buffer-search-word-forward
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "
 climacs")
    (is (= (buffer-search-word-forward buffer 0 "climacs") 2))
    (is-false (buffer-search-word-forward buffer 3 "climacs"))
    (is-false (buffer-search-word-forward buffer 0 "clim"))
    (is-false (buffer-search-word-forward buffer 5 "macs"))
    (is (= (buffer-search-word-forward buffer 0 "") 0))))

(buffer-test buffer-search-word-backward
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs 
")
    (is (= (buffer-search-word-backward buffer 8 "climacs") 0))
    (is-false (buffer-search-word-backward buffer 5 "climacs"))
    (is-false (buffer-search-word-backward buffer 4 "clim"))
    (is-false (buffer-search-word-backward buffer 8 "macs"))
    (is (= (buffer-search-word-backward buffer 8 "") 8))))
