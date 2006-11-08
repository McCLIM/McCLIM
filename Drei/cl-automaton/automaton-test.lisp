;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :automaton-user)

(defmacro defmultitest (name form &rest results)
  (let ((name-string (symbol-name name)))
    (flet ((%dts (prefixes)
	     (loop for v1 in prefixes nconc
		  (loop for v2 in '(t nil) collect
		       `(deftest ,(intern
				    (concatenate
				     'string
				     (symbol-name v1)
				     (if v2 "" "-LIGHT")
				     "."
				     name-string))
			  (let ((automaton::*minimization*
				 ',(intern (symbol-name v1) :automaton))
				(automaton::*minimize-always* ,v2))
			    ,form)
			  ,@results)))))
      `(progn ,@(%dts '(hopcroft huffman brzozowski))))))

(defmultitest regexp-automaton.test-1
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "#"))))
     (and (not (run a ""))
	  (not (run a "#"))
	  a))
   (automaton::minimize (automaton::empty-automaton)))
  t)

(defmultitest regexp-automaton.test-2
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "foo"))))
     (and (run a "foo") a))
   (automaton::minimize (automaton::string-automaton "foo")))
  t)

(defmultitest regexp-automaton.test-2a
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "()"))))
     (and (run a "") (not (run a " ")) a))
   (automaton::minimize (automaton::string-automaton "")))
  t)

(defmultitest regexp-automaton.test-2b
  (automaton-equal
   (regexp-automaton (string-regexp "()"))
   (automaton::minimize (automaton::empty-automaton)))
  nil)

(defmultitest regexp-automaton.test-3
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "c"))))
     (and (run a "c") (not (run a "C")) a))
   (automaton::minimize (automaton::char-automaton (char-code #\c))))
  t)

(defmultitest regexp-automaton.test-4
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "."))))
     (and (run a "x") (not (run a "xx")) a))
   (automaton::minimize (automaton::any-char-automaton)))
  t)

(defmultitest regexp-automaton.test-5
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "@"))))
     (and (run a "foo") a))
   (automaton::minimize (automaton::any-string-automaton)))
  t)

(defmultitest regexp-automaton.test-6
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "<11-15>"))))
     (and (run a "13") (not (run a "10")) (not (run a "16"))
	  (not (run a "20")) (not (run a "011")) a))
   (automaton::minimize (automaton::interval-automaton 11 15 2)))
  t)

(defmultitest regexp-automaton.test-6a
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "<11-115>"))))
     (and (run a "13") (run a "113") (not (run a "116")) (run a "00114")
	  (run a "20") (not (run a "200")) (run a "011") a))
   (automaton::minimize (automaton::interval-automaton 11 115 0)))
  t)

(defmultitest regexp-automaton.test-6b
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "<115-11>"))))
     (and (run a "13") (run a "113") (not (run a "116")) (run a "00114")
	  (run a "20") (not (run a "200")) (run a "011") a))
   (automaton::minimize (automaton::interval-automaton 11 115 0)))
  t)

(defmultitest regexp-automaton.test-7
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "sub" ht) (automaton::empty-automaton))
    (automaton-equal
     (let ((a (regexp-automaton (string-regexp "<sub>") ht)))
       (and (not (run a "foo")) a))
     (automaton::minimize (automaton::empty-automaton))))
  t)

(defmultitest regexp-automaton.test-8
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "[a-z]"))))
     (and (run a "a") (run a "z") (not (run a "A")) a))
   (automaton::minimize
    (automaton::char-range-automaton (char-code #\a) (char-code #\z))))
  t)

(defmultitest regexp-automaton.test-8a
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "[a]"))))
     (and (run a "a") (not (run a "A")) a))
   (automaton::minimize (automaton::char-automaton (char-code #\a))))
  t)

(defmultitest regexp-automaton.test-9
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "[a][b][c]"))))
     (and (run a "abc") (not (run a "ab")) (not (run a "a"))
	  (not (run a "A")) a))
   (automaton::minimize (automaton::string-automaton "abc")))
  t)

(defmultitest regexp-automaton.test-10
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "[ab]"))))
     (and (run a "a") (run a "b") (not (run a "ab"))
	  (not (run a "aa")) (not (run a "A")) a))
   (automaton::minimize
    (automaton::aunion
     (automaton::char-automaton (char-code #\a))
     (automaton::char-automaton (char-code #\b)))))
  t)

(defmultitest regexp-automaton.test-11
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "[^a-c0-3]"))))
     (and (run a "d") (not (run a "a")) (not (run a "0"))
	  (run a "4") (not (run a "dd")) (not (run a "00")) a))
   (automaton::minimize
    (automaton::aintersection
     (automaton::any-char-automaton)
     (automaton::acomplement
      (automaton::aunion
       (automaton::char-range-automaton (char-code #\a) (char-code #\c))
       (automaton::char-range-automaton (char-code #\0) (char-code #\3)))))))
  t)

(defmultitest regexp-automaton.test-11a
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "[a^b-c]"))))
     (and (run a "a") (run a "^") (run a "b") (run a "c")
	  (not (run a "d")) (not (run a "ad")) a))
   (automaton::minimize
    (automaton::aunion
     (automaton::aunion
      (automaton::char-automaton (char-code #\a))
      (automaton::char-automaton (char-code #\^)))
     (automaton::char-range-automaton (char-code #\b) (char-code #\c)))))
  t)

(defmultitest regexp-automaton.test-12
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "~[a-c]"))))
     (and (run a "d") (not (run a "a")) (not (run a "b")) (not (run a "c"))
	  (run a "dd") (run a "cc") (run a "A") a))
   (automaton::minimize
    (automaton::acomplement
     (automaton::char-range-automaton (char-code #\a) (char-code #\c)))))
  t)

(defmultitest regexp-automaton.test-13
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "f?"))))
     (and (run a "") (run a "f") (not (run a "ff")) (not (run a "F")) a))
   (automaton::minimize
    (automaton::optional (automaton::char-automaton (char-code #\f)))))
  t)

(defmultitest regexp-automaton.test-14
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "(\"foo\")?"))))
     (and (run a "") (run a "foo") (not (run a "foofoo"))
	  (not (run a "FOO")) a))
   (automaton::minimize
    (automaton::optional (automaton::string-automaton "foo"))))
  t)

(defmultitest regexp-automaton.test-15
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "[a-c]*"))))
     (and (run a "a") (run a "bb") (run a "ccc") (run a "abcabc")
	  (not (run a "d")) (run a "") a))
   (automaton::minimize
    (automaton::repeat
     (automaton::char-range-automaton (char-code #\a) (char-code #\c)))))
  t)

(defmultitest regexp-automaton.test-16
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "(\"foo\")+"))))
     (and (not (run a "")) (run a "foo") (run a "foofoo")
	  (not (run a "FOO")) a))
   (automaton::minimize
    (automaton::repeat-min (automaton::string-automaton "foo") 1)))
  t)

(defmultitest regexp-automaton.test-17
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "[a-c]{3}"))))
     (and (run a "abc") (run a "aaa") (not (run a "a")) (not (run a "aaaa"))
	  (not (run a "AAA")) a))
   (automaton::minimize
    (automaton::repeat-minmax
     (automaton::char-range-automaton (char-code #\a) (char-code #\c)) 3 3)))
  t)

(defmultitest regexp-automaton.test-18
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "(~c){1,2}"))))
     (and (run a "aa") (run a "AA") (run a "foofoo") (run a "foo")
	  (not (run a "c")) (run a "cc") (run a "ccc") a))
   (automaton::minimize
    (automaton::repeat-minmax
     (automaton::acomplement
      (automaton::char-automaton (char-code #\c))) 1 2)))
  t)

(defmultitest regexp-automaton.test-18a
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "~(c{1,2})"))))
     (and (run a "aa") (run a "AA") (run a "foofoo") (run a "foo")
	  (not (run a "c")) (not (run a "cc")) (run a "ccc") a))
   (automaton::minimize
    (automaton::acomplement
     (automaton::repeat-minmax
      (automaton::char-automaton (char-code #\c)) 1 2))))
  t)

(defmultitest regexp-automaton.test-19
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "[a-z]~[0-9]"))))
     (and (run a "aa") (run a "a") (not (run a "a0"))
	  (not (run a "")) (run a "abc") a))
   (automaton::minimize
    (automaton::aconcatenate
     (automaton::char-range-automaton (char-code #\a) (char-code #\z))
     (automaton::acomplement
      (automaton::char-range-automaton (char-code #\0) (char-code #\9))))))
  t)

(defmultitest regexp-automaton.test-20
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "(ab+)&(a+b)|c"))))
     (and (run a "ab") (run a "c") (not (run a "abb")) (not (run a "aab")) a))
   (automaton::minimize
    (automaton::aunion
     (automaton::aintersection
      (automaton::aconcatenate
       (automaton::char-automaton (char-code #\a))
       (automaton::repeat-min (automaton::char-automaton (char-code #\b)) 1))
      (automaton::aconcatenate
       (automaton::repeat-min (automaton::char-automaton (char-code #\a)) 1)
       (automaton::char-automaton (char-code #\b))))
     (automaton::char-automaton (char-code #\c)))))
  t)

(defmultitest regexp-automaton.test-21
  (automaton-equal
   (let ((a (regexp-automaton (string-regexp "a\"b\"+c"))))
     (and (run a "abc") (run a "abbc") (not (run a "ab")) (not (run a "ac")) a))
   (automaton::minimize
    (automaton::aconcatenate
     (automaton::char-automaton (char-code #\a))
     (automaton::aconcatenate
      (automaton::repeat-min (automaton::string-automaton "b") 1)
      (automaton::char-automaton (char-code #\c))))))
  t)

(defmultitest run.test-1
  (let ((a (regexp-automaton (string-regexp "[Cc]limacs"))))
    (and (run a "climacs")
	 (run a "Climacs")
	 (not (run a "Klimaks"))
	 (not (run a "climac"))
	 (not (run a "climax"))))
  t)

(defmultitest run-to-first-match.test-1
  (let ((a (regexp-automaton (string-regexp "[a-z]+"))))
    (and (= (run-to-first-match a "abc") 1)
	 (eq (run-to-first-match a "ABC") nil)
	 (eq (run-to-first-match a "000abc") nil)
	 (= (run-to-first-match a "a") 1)
	 (eq (run-to-first-match a "") nil)))
  t)

(defmultitest run-to-first-match.test-2
  (let ((a (regexp-automaton (string-regexp "(ab)+"))))
    (and (= (run-to-first-match a "abab") 2)
	 (= (run-to-first-match a "ababac") 2)))
  t)

(defmultitest run-to-first-unmatch.test-1
  (let ((a (regexp-automaton (string-regexp "[a-z]+"))))
    (and (= (run-to-first-unmatch a "abc") 3)
	 (eq (run-to-first-unmatch a "ABC") nil)
	 (eq (run-to-first-unmatch a "000abc") nil)
	 (= (run-to-first-unmatch a "a") 1)
	 (eq (run-to-first-unmatch a "") nil)
	 (= (run-to-first-unmatch a "abc9d") 3)))
  t)

(defmultitest run-to-first-unmatch.test-2
  (let ((a (regexp-automaton (string-regexp "(ab)+"))))
    (and (= (run-to-first-unmatch a "abab") 2)
	 (= (run-to-first-unmatch a "ababac") 2)))
  t)