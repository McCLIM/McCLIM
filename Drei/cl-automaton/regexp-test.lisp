;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :automaton-user)

(deftest string-regexp.test-1
  (regexp-equal
   (string-regexp "#")
   (automaton::make-regexp :empty))
  t)

(deftest string-regexp.test-2
  (regexp-equal
   (string-regexp "foo")
   (make-instance 'automaton::regexp :kind :string :s "foo"))
  t)

(deftest string-regexp.test-2a
  (regexp-equal
   (string-regexp "\"foo\"")
   (make-instance 'automaton::regexp :kind :string :s "foo"))
  t)

(deftest string-regexp.test-2b
  (regexp-equal
   (string-regexp "()")
   (make-instance 'automaton::regexp :kind :string :s ""))
  t)

(deftest string-regexp.test-3
  (regexp-equal
   (string-regexp "c")
   (make-instance 'automaton::regexp :kind :char :c #\c))
  t)

(deftest string-regexp.test-3a
  (regexp-equal
   (string-regexp "\c")
   (make-instance 'automaton::regexp :kind :char :c #\c))
  t)

(deftest string-regexp.test-3b
  (regexp-equal
   (string-regexp "\\c")
   (make-instance 'automaton::regexp :kind :char :c #\c))
  t)

(deftest string-regexp.test-4
  (regexp-equal
   (string-regexp ".")
   (automaton::make-regexp :anychar))
  t)

(deftest string-regexp.test-5
  (regexp-equal
   (string-regexp "@")
   (automaton::make-regexp :anystring))
  t)

(deftest string-regexp.test-6
  (regexp-equal
   (string-regexp "<11-15>")
   (make-instance 'automaton::regexp :kind :interval
		  :minr 11 :maxr 15 :digits 2))
  t)

(deftest string-regexp.test-6a
  (regexp-equal
   (string-regexp "<11-115>")
   (make-instance 'automaton::regexp :kind :interval
		  :minr 11 :maxr 115 :digits 0))
  t)

(deftest string-regexp.test-6b
  (regexp-equal
   (string-regexp "<115-11>")
   (make-instance 'automaton::regexp :kind :interval
		  :minr 11 :maxr 115 :digits 0))
  t)

(deftest string-regexp.test-7
  (regexp-equal
   (string-regexp "<sub>")
   (make-instance 'automaton::regexp :kind :automaton :s "sub"))
  t)

(deftest string-regexp.test-8
  (regexp-equal
   (string-regexp "[a-z]")
   (make-instance 'automaton::regexp :kind :char-range :from #\a :to #\z))
  t)

(deftest string-regexp.test-8a
  (regexp-equal
   (string-regexp "[a]")
   (make-instance 'automaton::regexp :kind :char :c #\a))
  t)

(deftest string-regexp.test-9
  (regexp-equal
   (string-regexp "[a][b][c]")
   (make-instance 'automaton::regexp :kind :string :s "abc"))
  t)

(deftest string-regexp.test-10
  (regexp-equal
   (string-regexp "[ab]")
   (automaton::make-regexp
    :union (make-instance 'automaton::regexp :kind :char :c #\a)
    (make-instance 'automaton::regexp :kind :char :c #\b)))
  t)

(deftest string-regexp.test-11
  (regexp-equal
   (string-regexp "[^a-c0-3]")
   (automaton::make-regexp
    :intersection
    (automaton::make-regexp :anychar)
    (automaton::make-regexp
     :complement
     (automaton::make-regexp
      :union
      (make-instance 'automaton::regexp :kind :char-range
		     :from #\a :to #\c)
      (make-instance 'automaton::regexp :kind :char-range
		     :from #\0 :to #\3)))))
  t)

(deftest string-regexp.test-11a
  (regexp-equal
   (string-regexp "[a^b-c]")
   (automaton::make-regexp
    :union
    (automaton::make-regexp
     :union (make-instance 'automaton::regexp :kind :char :c #\a)
     (make-instance 'automaton::regexp :kind :char :c #\^))
    (make-instance 'automaton::regexp :kind :char-range
		   :from #\b :to #\c)))
  t)

(deftest string-regexp.test-12
  (regexp-equal
   (string-regexp "~[a-c]")
   (automaton::make-regexp
    :complement (make-instance 'automaton::regexp :kind :char-range
			       :from #\a :to #\c)))
  t)

(deftest string-regexp.test-13
  (regexp-equal
   (string-regexp "f?")
   (automaton::make-regexp
    :optional (make-instance 'automaton::regexp :kind :char :c #\f)))
  t)

(deftest string-regexp.test-14
  (regexp-equal
   (string-regexp "(\"foo\")?")
   (automaton::make-regexp
    :optional (make-instance 'automaton::regexp :kind :string :s "foo")))
  t)

(deftest string-regexp.test-15
  (regexp-equal
   (string-regexp "[a-c]*")
   (automaton::make-regexp
    :repeat (make-instance 'automaton::regexp :kind :char-range
			   :from #\a :to #\c)))
  t)

(deftest string-regexp.test-16
  (regexp-equal
   (string-regexp "(\"foo\")+")
   (make-instance
    'automaton::regexp :kind :repeat-min
    :exp1 (make-instance 'automaton::regexp :kind :string :s "foo")
    :minr 1))
  t)

(deftest string-regexp.test-17
  (regexp-equal
   (string-regexp "[a-c]{3}")
   (make-instance
    'automaton::regexp :kind :repeat-minmax
    :exp1 (make-instance 'automaton::regexp :kind :char-range
			 :from #\a :to #\c)
    :minr 3 :maxr 3))
  t)

(deftest string-regexp.test-18
  (regexp-equal
   (string-regexp "(~c){1,2}")
   (make-instance
    'automaton::regexp :kind :repeat-minmax
    :exp1 (automaton::make-regexp
	   :complement (make-instance 'automaton::regexp :kind :char :c #\c))
    :minr 1 :maxr 2))
  t)

(deftest string-regexp.test-19
  (regexp-equal
   (string-regexp "[a-z]~[0-9]")
   (automaton::make-regexp
    :concatenation
    (make-instance 'automaton::regexp :kind :char-range :from #\a :to #\z)
    (automaton::make-regexp
     :complement (make-instance 'automaton::regexp :kind :char-range
				:from #\0 :to #\9))))
  t)

(deftest string-regexp.test-20
  (regexp-equal
   (string-regexp "(ab+)&(a+b)|c")
   (automaton::make-regexp
    :union
    (automaton::make-regexp
     :intersection
     (automaton::make-regexp
      :concatenation
      (make-instance 'automaton::regexp :kind :char :c #\a)
      (make-instance
       'automaton::regexp :kind :repeat-min
       :exp1 (make-instance 'automaton::regexp :kind :char :c #\b)
       :minr 1))
     (automaton::make-regexp
      :concatenation
      (make-instance
       'automaton::regexp :kind :repeat-min
       :exp1 (make-instance 'automaton::regexp :kind :char :c #\a)
       :minr 1)
      (make-instance 'automaton::regexp :kind :char :c #\b)))
    (make-instance 'automaton::regexp :kind :char :c #\c)))
  t)

(deftest string-regexp.test-21
  (regexp-equal
   (string-regexp "a\"b\"+c")
   (automaton::make-regexp
    :concatenation
    (make-instance 'automaton::regexp :kind :char :c #\a)
    (automaton::make-regexp
     :concatenation
     (make-instance
      'automaton::regexp :kind :repeat-min
      :exp1 (make-instance 'automaton::regexp :kind :string :s "b")
      :minr 1)
     (make-instance 'automaton::regexp :kind :char :c #\c))))
  t)