;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2006-2007 by
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

(def-suite lisp-syntax-tests :description "The test suite for
tests related to the Lisp syntax module. The parser is not
explicitly tested. Instead, it is hoped that any defects will be
caught by other test cases, all of which depend on correct
parsing. Also, redisplay is not tested, because no-one has any
idea how to do it.")

(in-suite lisp-syntax-tests)

(defvar *run-self-compilation-test* nil
  "If true, running the Lisp syntax module test suite will
involve an extreme stress test wherein the Lisp parser will be
used to read in the Drei source code, recompile Drei based on the
parser result and re-run the test suite (except for this
self-compilation test, of course).")

(defmacro testing-lisp-syntax ((buffer-contents &rest options) &body body)
  (assert (evenp (length options)))
  (with-gensyms (buffer drei)
    `(with-buffer (,buffer :initial-contents ,buffer-contents :syntax 'lisp-syntax)
       ,@(loop for (option value) on options by #'cddr
            collecting `(eval-option (syntax ,buffer) ,option ,value))
       (let ((,drei (make-instance 'test-drei :buffer ,buffer)))
         (with-bound-drei-special-variables (,drei :minibuffer nil)
           (labels ((get-form ()
                      (first (drei-lisp-syntax::children
                              (slot-value *current-syntax*
                                          'drei-lisp-syntax::stack-top))))
                    (get-object (&rest args)
                      (apply #'form-to-object *current-syntax*
                             (get-form) args)))
             ,@body))))))

(defmacro testing-symbol ((sym-sym &rest args) &body body)
  `(let ((,sym-sym (get-object ,@args)))
     ,@body
     (unless (or (null (symbol-package sym))
                 (eq (symbol-package sym)
                     (find-package :clim))
                 (eq (symbol-package sym)
                     (find-package :common-lisp)))
       (unintern ,sym-sym (symbol-package sym)))))

(defmacro testing-lisp-syntax-symbol ((buffer-contents sym-sym &rest args)
                                      &body body)
  `(testing-lisp-syntax (,buffer-contents)
     (flet ((get-object (&rest args)
              (apply #'form-to-object *current-syntax*
                     (first (drei-lisp-syntax::children
                             (slot-value *current-syntax*
                                         'drei-lisp-syntax::stack-top)))
                     args)))
       (testing-symbol (,sym-sym ,@args)
         ,@body))))

(test lisp-syntax-test-base
  "Test the Base syntax attribute for Lisp syntax."
  (testing-lisp-syntax ("")
    (is (= *read-base* (drei-lisp-syntax::base *current-syntax*))))
  (testing-lisp-syntax ("" :base "2")
    (is (= 2 (drei-lisp-syntax::base *current-syntax*))))
  (testing-lisp-syntax ("" :base "36")
    (is (= 36 (drei-lisp-syntax::base *current-syntax*))))
  (testing-lisp-syntax ("" :base "1")   ; Should be ignored.
    (is (= *read-base* (drei-lisp-syntax::base *current-syntax*))))
  (testing-lisp-syntax ("" :base "37")  ; Should be ignored.
    (is (= *read-base* (drei-lisp-syntax::base *current-syntax*)))))

(test lisp-syntax-test-package
  "Test the Package syntax attribute for Lisp syntax."
  (testing-lisp-syntax ("")
    (is (eq nil (drei-lisp-syntax::option-specified-package *current-syntax*))))
  (testing-lisp-syntax ("" :package "COMMON-LISP")
    (is (eq (find-package :cl)
            (drei-lisp-syntax::option-specified-package *current-syntax*))))
  (testing-lisp-syntax ("" :package "CL")
    (is (eq (find-package :cl)
            (drei-lisp-syntax::option-specified-package *current-syntax*))))
  (testing-lisp-syntax ("" :package "common-lisp")
    (is (string= "common-lisp"
                 (drei-lisp-syntax::option-specified-package *current-syntax*)))))

(test lisp-syntax-test-attributes
  "Test that the syntax attributes of Lisp syntax are returned
properly."
  (testing-lisp-syntax ("")
    (is-true (assoc :package (current-attributes-for-syntax *current-syntax*)))
    (is-true (assoc :base (current-attributes-for-syntax *current-syntax*)))))

(test lisp-syntax-package-at-mark
  "Test that Lisp syntax' handling of (in-package) forms is
correct."
  (testing-lisp-syntax ("(in-package :cl-user)  ")
    (is (eq *package*
            (drei-lisp-syntax::package-at-mark *current-syntax* 10))))
  (testing-lisp-syntax ("(in-package :cl-user)  " :package "DREI-LISP-SYNTAX")
    (is (eq (find-package :drei-lisp-syntax)
            (drei-lisp-syntax::package-at-mark *current-syntax* 10))))
  (testing-lisp-syntax ("(in-package :cl-user)  ")
    (is (eq (find-package :cl-user)
            (drei-lisp-syntax::package-at-mark *current-syntax* 23))))
  (testing-lisp-syntax ("(in-package \"CL-USER\")  ")
    (is (eq (find-package :cl-user)
            (drei-lisp-syntax::package-at-mark *current-syntax* 23))))
  (testing-lisp-syntax ("(in-package \"cl-user\")  ")
    (is (eq *package*
            (drei-lisp-syntax::package-at-mark *current-syntax* 23))))
  (testing-lisp-syntax ("(in-package :cl-user)(in-package :clim)  ")
    (is (eq (find-package :clim)
            (drei-lisp-syntax::package-at-mark *current-syntax* 43))))
  (testing-lisp-syntax ("(in-package :cl-user)(in-package :iDoNotExist)  ")
    (is (eq (find-package :cl-user)
            (drei-lisp-syntax::package-at-mark *current-syntax* 43)))))

(test lisp-syntax-provided-package-name-at-mark
  "Test that Lisp syntax' handling of (in-package) forms is
correct, even counting packages that cannot be found."
  (testing-lisp-syntax ("(in-package :cl-user)  ")
    (is (string= "CLIM-USER"
                 (drei-lisp-syntax::provided-package-name-at-mark *current-syntax* 10))))
  (testing-lisp-syntax ("(in-package :cl-user)  " :package "DREI-LISP-SYNTAX")
    (is (string= "DREI-LISP-SYNTAX"
                 (drei-lisp-syntax::provided-package-name-at-mark *current-syntax* 10))))
  (testing-lisp-syntax ("(in-package :cl-user)  ")
    (is (string= "CL-USER"
                 (drei-lisp-syntax::provided-package-name-at-mark *current-syntax* 23))))
  (testing-lisp-syntax ("(in-package \"CL-USER\")  ")
    (is (string= "CL-USER"
                 (drei-lisp-syntax::provided-package-name-at-mark *current-syntax* 23))))
  (testing-lisp-syntax ("(in-package \"cl-user\")  ")
    (is (string= "cl-user"
                 (drei-lisp-syntax::provided-package-name-at-mark *current-syntax* 23))))
  (testing-lisp-syntax ("(in-package :cl-user)(in-package :clim)  ")
    (is (string= "CLIM"
                 (drei-lisp-syntax::provided-package-name-at-mark *current-syntax* 43))))
  (testing-lisp-syntax ("(in-package :cl-user)(in-package :iDoNotExist)  ")
    (is (string= "IDONOTEXIST"
                 (drei-lisp-syntax::provided-package-name-at-mark *current-syntax* 48)))))

(test lisp-syntax-need-to-update-package-list-p
  "Test that Lisp syntax can properly handle it when (in-package)
  forms change."
  (testing-lisp-syntax ("(in-package :cl-user)  ")
    (is (eq (find-package :cl-user)
            (drei-lisp-syntax::package-at-mark *current-syntax* 23)))
    (delete-buffer-range *current-buffer* 0 (size *current-buffer*))
    (insert-buffer-sequence *current-buffer* 0 "(in-package :cl-userr)  ")
    (update-syntax *current-buffer* *current-syntax*)
    (is (eq *package*
            (drei-lisp-syntax::package-at-mark *current-syntax* 24)))
    (insert-buffer-sequence *current-buffer* 24 "(in-package :drei-lisp-syntax)  ")
    (update-syntax *current-buffer* *current-syntax*)
    (is (eq (find-package :drei-lisp-syntax)
            (drei-lisp-syntax::package-at-mark *current-syntax* 54)))
    (delete-buffer-range *current-buffer* 0 23)
    (insert-buffer-sequence *current-buffer* 0 "(in-package :clim-user)")
    (update-syntax *current-buffer* *current-syntax*)
    (is (eq (find-package :clim-user)
            (drei-lisp-syntax::package-at-mark *current-syntax* 26)))))

(test form-to-object-1
  "Test that we can parse and recognize T in Lisp syntax."
  (testing-lisp-syntax ("T")
    (is (eq t (get-object))))
  (testing-lisp-syntax ("t")
    (is (eq t (get-object)))))

(test form-to-object-2
  "Test that casing is properly done for NIL."
  (testing-lisp-syntax ("nil")
    (is (eq nil (get-object))))
  (testing-lisp-syntax ("NIL")
    (is (eq nil (get-object))))
  (testing-lisp-syntax ("NIl")
    (is (eq nil (get-object))))
  (testing-lisp-syntax ("NIl")
    (is-false (eq nil (get-object :case :preserve)))))

(test form-to-object-3
  "Test case-conversion for tokens."
  (testing-lisp-syntax ("iDoNotExist")
    (testing-symbol (sym :case :upcase)
      (is-false (symbol-package sym))
      (is (string= "IDONOTEXIST"
                   (symbol-name sym))))
    (testing-symbol (sym :case :preserve)
      (is-false (symbol-package sym))
      (is (string= "iDoNotExist"
                   (symbol-name sym))))
    (testing-symbol (sym :case :downcase)
      (is-false (symbol-package sym))
      (is (string= "idonotexist"
                   (symbol-name sym))))
    (testing-symbol (sym :read t :case :upcase)
      (is-true (symbol-package sym))
      (is (string= "IDONOTEXIST"
                   (symbol-name sym))))
    (testing-symbol (sym :read t :case :preserve)
      (is-true (symbol-package sym))
      (is (string= "iDoNotExist"
                   (symbol-name sym))))
    (testing-symbol (sym :read t :case :downcase)
      (is-true (symbol-package sym))
      (is (string= "idonotexist"
                   (symbol-name sym))))
    (testing-symbol (sym :case :invert)
      (is-false (symbol-package sym))
      (is (string= "iDoNotExist"
                   (symbol-name sym)))))
  (testing-lisp-syntax-symbol ("IDONOTEXIST" sym :case :invert)
    (is-false (symbol-package sym))
    (is (string= "idonotexist"
                 (symbol-name sym))))
  (testing-lisp-syntax-symbol ("idonotexist" sym :case :invert)
    (is-false (symbol-package sym))
    (is (string= "IDONOTEXIST"
                 (symbol-name sym)))))

(test form-to-object-4
  "Test case-conversion for uninterned tokens."
  (testing-lisp-syntax ("#:iDoNotExist")
    (testing-symbol (sym :case :upcase)
      (is-false (symbol-package sym))
      (is (string= "IDONOTEXIST"
                   (symbol-name sym))))
    (testing-symbol (sym :case :preserve)
      (is-false (symbol-package sym))
      (is (string= "iDoNotExist"
                   (symbol-name sym))))
    (testing-symbol (sym :case :downcase)
      (is-false (symbol-package sym))
      (is (string= "idonotexist"
                   (symbol-name sym))))
    (testing-symbol (sym :case :invert)
      (is-false (symbol-package sym))
      (is (string= "iDoNotExist"
                   (symbol-name sym)))))
  (testing-lisp-syntax ("#:IDONOTEXIST")
    (let ((sym (get-object :case :invert)))
      (is-false (symbol-package sym))
      (is (string= "idonotexist"
                   (symbol-name sym)))))
  (testing-lisp-syntax ("#:idonotexist")
    (let ((sym (get-object :case :invert)))
      (is-false (symbol-package sym))
      (is (string= "IDONOTEXIST"
                   (symbol-name sym))))))

(test form-to-object-5
  "Test handling of escaped symbols."
  (testing-lisp-syntax-symbol ("|123|" sym :read t)
    (is (string= "123" (symbol-name sym))))
  (testing-lisp-syntax-symbol ("|LIST|" sym :read t :case :downcase)
    (is (string= "LIST" (symbol-name sym))))
  (testing-lisp-syntax-symbol ("|   |" sym :read t)
    (is (string= "   " (symbol-name sym))))
  (testing-lisp-syntax-symbol ("|foo|bar|abbabz|" sym :read t)
    (is (string= "fooBARabbabz" (symbol-name sym))))
  (testing-lisp-syntax-symbol ("||" sym :read t)
    (is (string= "" (symbol-name sym))))
  (testing-lisp-syntax-symbol ("||||" sym :read t)
    (is (string= "" (symbol-name sym)))))

(test form-to-object-6
  "Test keyword symbols."
  (testing-lisp-syntax-symbol (":foo" sym :read t)
    (is (string= "FOO" (symbol-name sym)))
    (is (eq (find-package :keyword)
            (symbol-package sym)))))

(test form-to-object-7
  "Test that numbers are recognized and handled properly by the
Lisp syntax."
  (testing-lisp-syntax ("123")
    (is (= 123 (get-object))))
  (testing-lisp-syntax ("-123")
    (is (= -123 (get-object))))
  (testing-lisp-syntax (".123")
    (is (= .123 (get-object))))
  (testing-lisp-syntax ("-.123")
    (is (= -.123 (get-object))))
  (testing-lisp-syntax ("1.234")
    (is (= 1.234 (get-object))))
  (testing-lisp-syntax ("-1.234")
    (is (= -1.234 (get-object))))
  (testing-lisp-syntax ("1e7")
    (is (= 1e7 (get-object))))
  (testing-lisp-syntax ("1E7")
    (is (= 1e7 (get-object))))
  (testing-lisp-syntax ("1.123E7")
    (is (= 1.123e7 (get-object))))
  (testing-lisp-syntax ("-1.123E7")
    (is (= -1.123e7 (get-object))))
  (testing-lisp-syntax (".123E7")
    (is (= .123e7 (get-object))))
  (testing-lisp-syntax ("-.123E7")
    (is (= -.123e7 (get-object))))
  (testing-lisp-syntax ("1.34e-7")
    (is (= 1.34e-7 (get-object)))))

(test form-to-object-8
  "Test that the standard reader macros for numbers are
recognized and handled."
  (testing-lisp-syntax ("#b0000")
    (is (= 0 (get-object))))
  (testing-lisp-syntax ("#b10")
    (is (= 2 (get-object))))
  (testing-lisp-syntax ("#b-10")
    (is (= -2 (get-object))))
  (testing-lisp-syntax ("#x00")
    (is (= 0 (get-object))))
  (testing-lisp-syntax ("#xFE")
    (is (= 254 (get-object))))
  (testing-lisp-syntax ("#x-FE")
    (is (= -254 (get-object))))
  (testing-lisp-syntax ("#o00")
    (is (= 0 (get-object))))
  (testing-lisp-syntax ("#o71")
    (is (= 57 (get-object))))
  (testing-lisp-syntax ("#o-71")
    (is (= -57 (get-object)))))

(test form-to-object-9
  "Test handling of the literal character reader macro."
  (testing-lisp-syntax ("#\\a")
    (is (char= (get-object) #\a)))
  (testing-lisp-syntax ("#\\Null")
    (is (char= (get-object) #\Null)))
  (testing-lisp-syntax ("#\\NULL")
    (is (char= (get-object) #\Null)))
  (testing-lisp-syntax ("#\\ ")
    (is (char= (get-object) #\Space))))

(test form-to-object-10
  "Test handling of list syntax."
  (testing-lisp-syntax ("(t t t)")
    (is (equal (get-object) '(t t t))))
  (testing-lisp-syntax ("()")
    (is (eq (get-object) nil)))
  (testing-lisp-syntax ("(#\\  t)")
    (is (equal (get-object) '(#\Space t))))
  (testing-lisp-syntax ("(NIL nil Nil)")
    (destructuring-bind (a b c) (get-object :case :preserve)
      (is (string= (symbol-name a) "NIL"))
      (is (string= (symbol-name b) "nil"))
      (is (string= (symbol-name c) "Nil")))))

(test form-to-object-11
  "Test handling of the vector reader-macro syntax."
  (testing-lisp-syntax ("#(t t t)")
    (is (equalp (get-object) #(t t t))))
  (testing-lisp-syntax ("#()")
    (is (equalp (get-object) #())))
  (testing-lisp-syntax ("#(#\\  t)")
    (is (equalp (get-object) #(#\Space t))))
  (testing-lisp-syntax ("#(NIL nil Nil)")
    (destructuring-bind (a b c) (loop for x across (get-object :case :preserve)
                                   collecting x)
      (is (string= (symbol-name a) "NIL"))
      (is (string= (symbol-name b) "nil"))
      (is (string= (symbol-name c) "Nil"))))
  (testing-lisp-syntax ("#(a b c c c c)")
    (is (equalp (get-object) #6(a b c c c c))))
  (testing-lisp-syntax ("#6(a b c c c c)")
    (is (equalp (get-object) #6(a b c c c c))))
  (testing-lisp-syntax ("#6(a b c)")
    (is (equalp (get-object) #6(a b c c c c))))
  (testing-lisp-syntax ("#6(a b c c)")
    (is (equalp (get-object) #6(a b c c c c)))))

(test form-to-object-12
  "Test handling of dotted-pair forms."
  (testing-lisp-syntax ("(t . t)")
    (is (equal (get-object) '(t . t))))
  (testing-lisp-syntax ("(t.t)")
    (is (string= (first (get-object)) "T.T")))
  (testing-lisp-syntax ("(t . nil)")
    (is (equal (get-object) '(t))))
  (testing-lisp-syntax ("(t t . t)")
    (is (equal (get-object) '(t t . t))))
  (testing-lisp-syntax ("(#\\ . t)")
    (is (equal (get-object) '(#\Space . t))))
  (testing-lisp-syntax ("(t t . 't)")
    (is (equal (get-object) '(t t quote t))))
  (testing-lisp-syntax ("(NIL nil . Nil)")
    (destructuring-bind (a b . c) (get-object :case :preserve)
      (is (string= (symbol-name a) "NIL"))
      (is (string= (symbol-name b) "nil"))
      (is (string= (symbol-name c) "Nil")))))

(test form-to-object-13
  "Test handling of incomplete list forms."
  (testing-lisp-syntax ("(t ")
    (finishes
      (get-object))
    (signals form-conversion-error
      (get-object :read t))
    (finishes
      (get-object :read t :no-error t))))

(test form-to-object-14
  "Test backquote syntax handling for lists."
  (testing-lisp-syntax ("`(list ,(+ 2 2))")
    (is (equal (eval (get-object))
               '(list 4))))
  (testing-lisp-syntax ("``(list ,,(+ 2 2))")
    (is (equal (eval (eval (get-object)))
               '(list 4))))
  (testing-lisp-syntax ("(let ((a '(1 2 3))) `(list ,@a))")
    (is (equal (eval (get-object :read t))
               '(list 1 2 3))))
  (testing-lisp-syntax ("(let ((a '(1 2 3))) 
                           `(let ((b 42))
                              `(list (,,@a) ,b))))")
    (is (equal (eval (eval (get-object :read t)))
               '(list (1 2 3) 42))))
  (testing-lisp-syntax ("(let ((a '(1 2 3))) 
                           `(list ,a `',(+ 2 2)))")
    (is (equal (second  (eval (get-object :read t)))
               '(1 2 3))))
  (testing-lisp-syntax ("(let ((a 'list)) `',a)")
    (is (equal (eval (eval (get-object :read t)))
               'list)))
  (testing-lisp-syntax ("(let ((a '(1 2 3))) `',`',a)")
    (is (equal (eval (get-object :read t))
               '''(1 2 3))))
  (testing-lisp-syntax ("(let ((a '(1 2 3))) ``(list ,@',a))")
    (is (equal (eval (eval (eval (get-object :read t))))
               '(1 2 3))))
  (testing-lisp-syntax ("(let ((a '(4 5 6))) `(list 1 2 3 ,.a))")
    (is (equal (eval (eval (get-object :read t)))
               '(1 2 3 4 5 6))))
  (testing-lisp-syntax ("(let ((a '('(4 5 6) '(7 8 9)))) 
```(list 1 2 3 ,.,@',a))")
    (is (equal (eval (eval (eval (eval (get-object :read t)))))
               '(1 2 3 4 5 6 7 8 9))))
  (testing-lisp-syntax ("`(car . cdr)")
    (is (equal (eval (get-object :read t))
               '(car . cdr)))))

(test form-to-object-15
  "Test backquote syntax handling for arrays."
  (testing-lisp-syntax ("`#(1 ,(+ 2 2) 6)")
    (is (equalp (eval (get-object :read t))
                #(1 4 6))))
  (testing-lisp-syntax ("(let ((a '(2 3 4 5))) `#(1 ,@a 6))")
    (is (equalp (eval (get-object :read t))
                #(1 2 3 4 5 6))))
  (testing-lisp-syntax ("`#(list ,(+ 2 2))")
    (is (equalp (eval (get-object))
                #(list 4))))
  (testing-lisp-syntax ("``(list #(,,(+ 2 2)))")
    (is (equalp (eval (eval (get-object)))
                '(list #(4)))))
  (testing-lisp-syntax ("(let ((a '(1 2 3))) 
                           `(let ((b 42))
                              `#(list #(,,@a) ,b))))")
    (is (equalp (eval (eval (get-object :read t)))
                #(list #(1 2 3) 42))))
  (testing-lisp-syntax ("(let ((a #(1 2 3))) 
                           `(list #(,a) `#',(+ 2 2)))")
    (is (equalp (second (eval (get-object :read t)))
                #(#(1 2 3)))))
  (testing-lisp-syntax ("(let ((a 'list)) `#(,a))")
    (is (equalp (eval (eval (get-object :read t)))
                #(list))))
  (testing-lisp-syntax ("(let ((a '(1 2 3))) `#(,`#(,a)))")
    (is (equalp (eval (get-object :read t))
                #(#((1 2 3))))))
  (testing-lisp-syntax ("(let ((a '(1 2 3))) ``#(,@',a))")
    (is (equalp (eval (eval (eval (get-object :read t))))
                #(1 2 3))))
  (testing-lisp-syntax ("(let ((a '(4 5 6))) `#(1 2 3 ,.a))")
    (is (equalp (eval (eval (get-object :read t)))
                #(1 2 3 4 5 6))))
  (testing-lisp-syntax ("(let ((a '('(4 5 6) '(7 8 9)))) 
```#(1 2 3 ,.,@',a))")
    (is (equalp (eval (eval (eval (eval (get-object :read t)))))
                #(1 2 3 4 5 6 7 8 9)))))

(test form-to-object-16
  "Test read-time conditional handling."
  (testing-lisp-syntax ("#+mcclim t")
    (is (eq (get-object) (or #+mcclim t))))
  (testing-lisp-syntax ("#-mcclim t")
    (is (eq (get-object) (or #-mcclim t))))
  (testing-lisp-syntax ("(#+mcclim t)")
    (is (equal (get-object) '(#+mcclim t))))
  (testing-lisp-syntax ("(#-mcclim t)")
    (is (equal (get-object) '(#-mcclim t)))))

(test form-to-object-17
  "Test the reader syntax for labels (including circular
references)."
  (testing-lisp-syntax ("(#1=list #1#)")
    (is (equal (get-object) '(list list))))
  (testing-lisp-syntax ("#1=(list . #1#)")
    (finishes
      (loop for x in (get-object)
         for y in '#1=(list . #1#)
         for i from 0 upto 100
         unless (eq y x)
         do (fail "~A is not eq to ~A" x y))))
  (testing-lisp-syntax ("(#1=list (#1# 1 2 3))")
    (let ((form (drei-lisp-syntax::form-before *current-syntax* 14)))
      (is (eq 'list (form-to-object *current-syntax* form)))))
  (testing-lisp-syntax ("(#1=list #1=cons)")
    (signals form-conversion-error
      (get-object))))

(test form-to-object-18
  "Test the reader syntax for multidimensional arrays."
  (testing-lisp-syntax ("#2A((0 1 5) (foo 2 (hot dog)))")
    (is (equalp (get-object) #2A((0 1 5) (foo 2 (hot dog))))))
  (testing-lisp-syntax ("#2A((0 1) (foo 2 (hot dog)))")
    (signals form-conversion-error (get-object)))
  (testing-lisp-syntax ("#1A((0 1 5) (foo 2 (hot dog)))")
    (is (equalp (get-object) #1A((0 1 5) (foo 2 (hot dog))))))
  (testing-lisp-syntax ("#0Anil")
    (is (equalp (get-object) #0Anil)))
  (testing-lisp-syntax ("#0A#2A((0 1 5) (foo 2 (hot dog)))")
    (is (equalp (get-object) #0A#2A((0 1 5) (foo 2 (hot dog)))))))

(test form-to-object-19
  "Test the handling of the quote reader macro."
  (testing-lisp-syntax ("'list")
    (is (eq 'quote (first (get-object))))
    (is (eq 'list (second (get-object)))))
  (testing-lisp-syntax ("''list")
    (is (eq 'quote (first (get-object))))
    (is (eq 'quote (caadr (get-object))))
    (is (eq 'list (cadadr (get-object)))))
  (testing-lisp-syntax ("'#:list")
    (is (eq 'quote (first (get-object))))
    (is (string= "LIST" (symbol-name (second (get-object)))))
    (is-false (symbol-package (second (get-object)))))
  (testing-lisp-syntax ("'#p\"foobar\"")
    (is (eq 'quote (first (get-object))))
    (is (equalp #p"foobar" (second (get-object)))))
  (testing-lisp-syntax ("'#.(+ 2 2)")
    (is (eq 'quote (first (get-object))))
    (is (= 4 (second (get-object :read t))))))

(defmacro testing-form-selectors ((buffer-contents &rest syntax-options)
                                  &body body)
  `(testing-lisp-syntax (,buffer-contents ,@syntax-options)
     (macrolet ((test-selector (selector-fn offset expected-result
                                            &optional (test 'eql))
                  `(is (,test ,expected-result
                         (form-to-object *current-syntax*
                                         (,selector-fn *current-syntax* ,offset)))))
                (test-selector-null (selector-fn offset)
                  `(is-false (,selector-fn *current-syntax* ,offset))))
       ,@body)))

(test form-before
  "Test the `form-before' form selector of Lisp syntax."
  (testing-form-selectors ("(list #|foo|# foo #|bar|# bar
 baz ; baz indeed
)  ")
    (test-selector-null drei-lisp-syntax::form-before 0)
    (test-selector-null drei-lisp-syntax::form-before 4)
    (test-selector drei-lisp-syntax::form-before 5 'list)))

(test form-after
  "Test the `form-after' form selector of Lisp syntax."
  (testing-form-selectors ("(list #|foo|# foo #|bar|# bar
 baz ; baz indeed
)  ")
    (test-selector-null drei-lisp-syntax::form-after (size *current-buffer*))
    (test-selector-null drei-lisp-syntax::form-after (- (size *current-buffer*) 4))
    (test-selector drei-lisp-syntax::form-after 7 'foo)))

(test form-around
  "Test the `form-around' form selector of Lisp syntax."
  (testing-form-selectors ("(list #|foo|# foo #|bar|# bar
 baz ; baz indeed
)  ")
    (test-selector-null drei-lisp-syntax::form-around (size *current-buffer*))
    (test-selector drei-lisp-syntax::form-around
                   (- (size *current-buffer*) 4)
                   '(list foo bar baz) equal)
    (test-selector drei-lisp-syntax::form-around 3 'list)))

(test expression-at-mark
  "Test the `expression-at-mark' function of Lisp syntax."
  (testing-form-selectors ("(list #|foo|# foo #|bar|# bar
 baz ; baz indeed
)  " :syntax "DREI-TESTS")
    (test-selector drei-lisp-syntax::expression-at-mark 15 'foo)
    (test-selector drei-lisp-syntax::expression-at-mark 10 'foo)
    (test-selector drei-lisp-syntax::expression-at-mark (1- (size *current-buffer*))
                   '(list foo bar baz) equal)))

(test definition-at-mark
  "Test the `definition-at-mark' function of Lisp syntax."
  (testing-form-selectors ("(defun foo (&rest x y z) (append x y z))")
    (let ((expected-result '(defun foo (&rest x y z) (append x y z))))
      (test-selector drei-lisp-syntax::definition-at-mark 0
                     expected-result equal)
      (test-selector drei-lisp-syntax::definition-at-mark 10
                     expected-result equal)
      (test-selector drei-lisp-syntax::definition-at-mark (1- (size *current-buffer*))
                     expected-result equal))))

(test symbol-at-mark
  "Test the `symbol-at-mark' function of Lisp syntax."
  (testing-form-selectors ("'(''list 'progn #p\"foobar\" '''''''''identity) ")
    (test-selector drei-lisp-syntax::symbol-at-mark 5 'list)
    (test-selector drei-lisp-syntax::symbol-at-mark 15 'progn)
    (test-selector-null drei-lisp-syntax::symbol-at-mark 25)))

(test this-form
  "Test the `this-form' function of Lisp syntax."
  (testing-form-selectors ("(   list 1 (identity 4 5 6) 2 3)")
    (test-selector drei-lisp-syntax::this-form 0
                   '(list 1 (identity 4 5 6) 2 3) equal)
    (test-selector drei-lisp-syntax::this-form 1
                   '(list 1 (identity 4 5 6) 2 3) equal)
    (test-selector drei-lisp-syntax::this-form 4 'list)
    (test-selector drei-lisp-syntax::this-form 7 'list)
    (test-selector drei-lisp-syntax::this-form 8 'list)))

(defmacro testing-lisp-predicate ((predicate) &body body)
  `(macrolet ((trues (&rest strings)
                `(progn ,@(mapcar #'(lambda (string)
                                      `(is-true
                                        ,(list ',predicate
                                               `(get-form-from-string ,string))))
                                  strings)))
              (falses (&rest strings)
                `(progn ,@(mapcar #'(lambda (string)
                                      `(is-false
                                        ,(list ',predicate
                                               `(get-form-from-string ,string))))
                                  strings))))
     (flet ((get-form-from-string (string)
              (testing-lisp-syntax (string)
                (get-form))))
       (progn ,@body))))

(test formp
  "Test the predicate for determining whether a syntax object is
a form in the Lisp sense."
  (testing-lisp-predicate (drei-lisp-syntax::formp)
    (trues "(1 2 3)")
    (falses "#| foo |#")))

(test form-list-p
  "Test whether a syntax object represents a list."
  (testing-lisp-predicate (drei-lisp-syntax::form-list-p)
    (trues "(1 2 3)" "(1 2 3" )
    (falses "42")))

(test form-incomplete-p
  "Test whether a syntax object represents an incomplete piece of
syntax."
  (testing-lisp-predicate (drei-lisp-syntax::form-incomplete-p)
    (trues "(1 2 3" "#p\"foo")
    (falses "T" "42")))

(test form-complete-p
  "Test whether a syntax object represents a complete piece of
syntax."
  (testing-lisp-predicate (drei-lisp-syntax::form-complete-p)
    (trues "T" "42")
    (falses "(1 2 3" "#p\"foo")))

(test form-token-p
  "Test whether a syntax object represents a token in the Lisp
  sense (CLHS 2.3)."
  (testing-lisp-predicate (drei-lisp-syntax::form-token-p)
    (falses "(1 2 3)" "#p\"foo\"")
    (trues "T" "42" "foobar" "|Foobar|" "#:foobar" "|Foobar|" ":foobar")))

(test form-string-p
  "Test whether a syntax object represents a string."
  (testing-lisp-predicate (drei-lisp-syntax::form-string-p)
    (trues "\"foobar\"" "\"foobar")
    (falses "foobar")))

(test form-quoted-p
  "Test whether a syntax object represents a quoted form."
  (testing-lisp-predicate (drei-lisp-syntax::form-quoted-p)
    (trues "'list" "'(foo bar baz)" "'#p\"foobar\""
           "'`'''#:hello" "`',t" "'(1 2 3")
    (falses "42" "53" "(1 2 3" "foobar" "(quote hello)")))

(test form-comma-p
  "Test whether a syntax object represents a comma (,) form."
  (testing-lisp-predicate (drei-lisp-syntax::form-comma-p)
    (trues ",foo")
    (falses "'foo" "`',foo" ",.foobar" ",@foobaz")))

(test form-comma-at-p
  "Test whether a syntax object represents a comma-at (,@) form."
  (testing-lisp-predicate (drei-lisp-syntax::form-comma-at-p)
    (trues ",@foobaz")
    (falses "'foo" "`',foo" ",.foobar" ",foo")))

(test form-comma-dot-p
  "Test whether a syntax object represents a comma-dot (,.) form."
  (testing-lisp-predicate (drei-lisp-syntax::form-comma-dot-p)
    (trues ",.foobar")
    (falses "'foo" "`',foo" ",@foobaz" ",foo")))

(test comment-p
  "Test whether a syntax object represents a comment."
  (testing-lisp-predicate (drei-lisp-syntax::comment-p)
    (trues ";foo" ";" "#|foobar|#" "#||#" "#||||#" "#|")
    (falses "#:|foo|#")))

(test form-at-top-level-p
  "Test the function that determines whether a form is a
top-level form."
  (testing-lisp-syntax ("(defun foo (&rest x y z) (append x y z))")
    (is-false (drei-lisp-syntax::form-at-top-level-p
               (drei-lisp-syntax::form-around *current-syntax* 10)))
    (is-true (drei-lisp-syntax::form-at-top-level-p
              (drei-lisp-syntax::form-around *current-syntax* 0)))
    (is-false (drei-lisp-syntax::form-at-top-level-p
               (drei-lisp-syntax::form-around *current-syntax* 30)))))

(test replace-symbol-at-mark
  "Test the function for replacing symbols at the position of a
mark."
  (testing-lisp-syntax ("(defun foo (&rest x y z) (append x y z))")
    (let ((mark (clone-mark *current-point*)))
      (setf (offset mark) 8)
      (performing-drei-operations (*current-window* :redisplay nil)
        (drei-lisp-syntax::replace-symbol-at-mark
         *current-syntax* mark
         "list"))
      (is (= 11 (offset mark)))
      (is (eq 'list (second (get-object))))
      (setf (offset mark) 0)
      (performing-drei-operations (*current-window* :redisplay nil)
        (drei-lisp-syntax::replace-symbol-at-mark
         *current-syntax* mark
         "quote"))
      (is (= 5 (offset mark)))
      (is (eq 'quote (get-object)))))
  ;; And now for a real-world test case (completion)...
  (testing-lisp-syntax ("(w-o-t-s (s \"foo\" :e-t 'character ")
    (let ((mark (clone-mark *current-point*)))
      (setf (offset mark) 8)
      (performing-drei-operations (*current-window* :redisplay nil)
        (drei-lisp-syntax::replace-symbol-at-mark
         *current-syntax* mark
         "with-output-to-string"))
      (is (= 22 (offset mark)))
      (buffer-is "(with-output-to-string (s \"foo\" :e-t 'character ")
      (setf (offset mark) 36)
      (performing-drei-operations (*current-window* :redisplay nil)
        (drei-lisp-syntax::replace-symbol-at-mark
         *current-syntax* mark
         ":element-type"))
      (buffer-is "(with-output-to-string (s \"foo\" :element-type 'character ")
      (is (= 45 (offset mark))))))

(motion-fun-one-test expression (51 0 (11 28 7)
                                    "(defun list (&rest elements)
(append elements nil))" :syntax lisp-syntax))

(motion-fun-one-test list (64 4 (22 41 11)
                             "foo (defun (barbaz) list (&rest elements)
(append elements nil))" :syntax lisp-syntax))

(motion-fun-one-test down (1 53 (15 16 13)
                             "(defun list () (&rest elements)
(append elements nil))" :syntax lisp-syntax))

(motion-fun-one-test up (nil nil (13 14 12)
                             "(defun list () (&rest elements)
(append elements nil))" :syntax lisp-syntax))

(motion-fun-one-test definition (51 52 (35 51 0)
                                    "(defun list (&rest elements)
(append elements nil)) (defun second (list) (cadr list))" 
:syntax lisp-syntax))

(test in-string-p
  "Test the `in-string-p' function of Lisp syntax."
  (testing-lisp-syntax (" \"foobar!\" ")
    (is-false (drei-lisp-syntax::in-string-p 0 *current-syntax*))
    (is-false (drei-lisp-syntax::in-string-p 1 *current-syntax*))
    (is-true (drei-lisp-syntax::in-string-p 2 *current-syntax*))
    (is-true (drei-lisp-syntax::in-string-p 6 *current-syntax*))
    (is-true (drei-lisp-syntax::in-string-p 9 *current-syntax*))
    (is-false (drei-lisp-syntax::in-string-p 10 *current-syntax*))))

(test in-comment-p
  "Test the `in-comment-p' function of Lisp syntax."
  (testing-lisp-syntax (" ; I'm a comment
;; I'm another comment
#| I'm a
- BLOCK -
comment |#")
    (is-false (drei-lisp-syntax::in-comment-p 0 *current-syntax*))
    (is-false (drei-lisp-syntax::in-comment-p 1 *current-syntax*))
    (is-true (drei-lisp-syntax::in-comment-p 2 *current-syntax*))
    (is-false (drei-lisp-syntax::in-comment-p 16 *current-syntax*))
    (is-false (drei-lisp-syntax::in-comment-p 17 *current-syntax*))
    (is-true (drei-lisp-syntax::in-comment-p 18 *current-syntax*))
    (is-false (drei-lisp-syntax::in-comment-p 40 *current-syntax*))
    (is-true (drei-lisp-syntax::in-comment-p 41 *current-syntax*))
    (is-true (drei-lisp-syntax::in-comment-p 50 *current-syntax*))
    (is-true (drei-lisp-syntax::in-comment-p 60 *current-syntax*))
    (is-false (drei-lisp-syntax::in-comment-p 69 *current-syntax*))))

;; For the arglist fetching tests, we need some dummy functions and
;; macros whose arglists we can be sure of. We define those here. We
;; also hope we don't clobber anything important.

(defun lisp-syntax-f1 ())
(defun lisp-syntax-f2 (l) (declare (ignore l)))
(defun lisp-syntax-f3 (a b c &optional (l 1)) (declare (ignore a b c l)))

(defmacro lisp-syntax-m1 ((var &optional string &key (element-type ''character))
                          &body forms-decls) ; with-output-to-string
  (declare (ignore var string element-type forms-decls)))

(defmacro swine-test (name &body body)
  `(test ,name
     (if (eq (drei-lisp-syntax::default-image) t)
         (skip "No useful image link found")
         (progn
           ,@body))))

(defmacro arglist-test (name &body body)
  `(swine-test ,name
     (flet ((arglist-of (symbol)
              (drei-lisp-syntax::arglist-for-form
               *current-syntax* symbol)))
       ,@body)))

(arglist-test arglist-for-form-1
  "Test that we can extract basic information about the arglists
of functions and macros."
  (testing-lisp-syntax ("")
    (is (equal '() (arglist-of 'lisp-syntax-f1)))
    (is (equal '(l) (arglist-of 'lisp-syntax-f2)))
    (is (equal '(a b c &optional (l 1)) (arglist-of 'lisp-syntax-f3)))
    (is (equal '((var &optional string &key (element-type ''character))
                 &body forms-decls)
               (arglist-of 'lisp-syntax-m1)))))

(arglist-test arglist-for-form-2
  "Test that we can extract basic information about the arglists
of lambda expressions."
  (testing-lisp-syntax ("")
    (is (equal '() (arglist-of '(lambda ()))))
    (is (equal '(l) (arglist-of '(lambda (l)))))
    (is (equal '(a b c &optional (l 1)) (arglist-of '(lambda (a b c &optional (l 1))))))))

;; Testing indentation consists of providing a string containing
;; properly indented code. When the test is run, all leading
;; whitespace will be removed from each line, and the entire buffer
;; will subsequently be reindented and compared with the original
;; string. Fast and easy.
(defmacro indentation-test (name &body body)
  `(swine-test ,name
     (macrolet ((test-indentation (string)
                  `(testing-lisp-syntax (,string)
                     (let ((start (clone-mark (point *current-window*)))
                           (end (clone-mark (point *current-window*))))
                       (beginning-of-buffer start)
                       (end-of-buffer end)
                       (do-buffer-region-lines (line start end)
                         (delete-indentation *current-syntax* line))
                       (indent-region *current-window* start end)
                       (buffer-is ,string)))))
       (macrolet ((test-indentations (&rest strings)
                    `(progn
                       ,@(loop for string in strings
                            collecting (macroexpand `(test-indentation ,string))))))
         ,@body))))

(indentation-test indent-form-1
  "Test indentation of relatively simple and complete list forms."
  (test-indentations
   "
(defun list (&rest elements)
  (append elements nil))"
   "
(list
 1
 2
 3)"
   "
(list 1
      2
      3)"
   "
(list ;foo!
 1
 2
 3)"
   "
(list 1 2
      3)"
   "
(list 1 2 3
      )"
   "
((list 1 2
       3))"))

(indentation-test indent-form-2
  "Test the indentation of simple vector forms."
  (test-indentations
   "
#(1
  2
  3)"
   "
#(1 2
  3)"
   "
#(1 2 3
  )"))

(indentation-test indent-form-3
  "Test the indentation of quoted forms."
  (test-indentations
   "
'()"
   "
'(
  
  
  a)"
   "
'(list 1
  2
  3)"
   "
'(list ;foo!
  1
  2
  3)"
   "
'(list 1 2
  3)"
   "
'(list 1 2 3
  )"
   "
'(f
  l)"))

(indentation-test indent-form-4
  "Test the indentation of backquoted forms."
  (test-indentations
   "
`()"
   "
`(
  
  
  ,a)"
   "
`(list 1
       2
       3)"
   "
`(list ;foo!
  1
  2
  3)"
   "
`(list 1 2
       3)"
   "
`(list 1 2 3
       )"
   "
`(f
  ,l)"
   ;; Okay, I'm bored, just slap an evil (pseudo) macro definition in
   ;; and test it...
   "
(defmacro testing-lisp-predicate ((predicate) &body body)
  `((trues (&rest strings)
           `(progn ,@(mapcar #'(lambda (string)
                                 `(f
                                   ,(list ',predicate
                                          `(get-form-from-string ,string))))
                             strings)))
    (falses (&rest strings)
            `(progn ,@(mapcar #'(lambda (string)
                                  `(l
                                    ,(list ',predicate
                                           `(get-form-from-string ,string))))
                              strings))))
  (progn ,@body))"))

(indentation-test indent-form-5
  "Test the indentation of string forms."
  (test-indentations
   "
\"





\" "
   "
\"foobar! 
Foo 
Foo\" "))

(indentation-test indent-form-progn
  "Test the indentation rules for the `progn' special form."
  (test-indentations
   "
(progn a b c)"
   "
(progn a
       b c)"
   "
(progn a
       b
       c)"
   "
(progn
  a
  b
  c)"
   "
(progn
  a b
  c)"
   "
(progn a
       b
       (list 1
             2
             3))"
   "
(
 progn)"))

(indentation-test indent-form-prog1
  "Test the indentation rules for the `prog1' special form."
  (test-indentations
   "
(prog1 a
  b
  c)"

   "
(prog1
    a
  b
  c)"
   "
(prog1
    a)"))

(indentation-test indent-form-prog2
  "Test the indentation rules for the `prog2' special form."
  (test-indentations
   "
(prog2
    a
    b
  c)"
   "
(prog2 a
    b
  c)"
   "
(prog2 a
    )"))

(indentation-test indent-form-let
  "Test the indentation rules for the `let' special form."
  (test-indentations
   "
(let ((a b)
      (c d))
  contents)"
   "
(let
    ((a b)
     (c d))
  contents2)"
   "
(let ((a b)
      (c d))
  contents
  contents2)"
   "
(let
    ((a b)
     (c d))
  contents
  contents2)"
   "
(let ((a (foo bar
              baz)))
  contents2)
"))

(indentation-test indent-form-let*
  "Test the indentation rules for the `let*' special form."
  (test-indentations
   "
(let* ((a b)
       (c d))
  contents)"
   "
(let*
    ((a b)
     (c d))
  contents2)"
   "
(let* ((a b)
       (c d))
  contents
  contents2)"
   "
(let*
    ((a b)
     (c d))
  contents
  contents2)"
   "
(let* ((a (foo bar
               baz)))
  contents2)
"))

(indentation-test indent-form-multiple-value-bind
  "Test the indentation rules for the `multiple-value-bind'
  special form."
  (test-indentations
   "(multiple-value-bind (a b)
    (foo bar)
  contents)"
   "
(multiple-value-bind (a b)
    (foo bar)
  contents
  contents2)"
   "
(multiple-value-bind
      (a b)
    (foo bar)
  contents)"
   "
(multiple-value-bind
      (a b)
    (foo bar)
  contents
  contents2)"
   "
(multiple-value-bind (a b) (foo bar)
  contents)"
   "
(multiple-value-bind (a b) (foo bar)
  contents
  contents2)"
   "
(multiple-value-bind (a b) (foo bar) contents
                                     contents2)
"
   "
(
 multiple-value-bind)
"))

(test lisp-syntax-comment-region
  "Test the implementation of the `comment-region' function for
Lisp syntax."
  (testing-lisp-syntax ("foo bar

baz
")
    (let ((begin (beginning-of-buffer (clone-mark *current-point*)))
          (end (end-of-buffer (clone-mark *current-point*))))
      (comment-region *current-syntax* begin end)
      (buffer-is ";;; foo bar
;;; 
;;; baz
")))
  (testing-lisp-syntax ("")
    (let ((begin (beginning-of-buffer (clone-mark *current-point*)))
          (end (end-of-buffer (clone-mark *current-point*))))
      (comment-region *current-syntax* begin end)
      (buffer-is ""))))

(defgeneric find-pathnames (module)
  (:documentation "Get a list of the pathnames of the files
making up an ASDF module/system/component.")
  (:method-combination nconc))

(defmethod find-pathnames nconc ((module asdf:module))
  (mapcan #'find-pathnames (asdf:module-components module)))

(defmethod find-pathnames nconc ((module asdf:source-file))
  (list (asdf:component-pathname module)))

;; Thank you Mr. Insane 3000!
(defun slurp-file (pathname)
  (with-open-file (strm pathname)
    (let ((string (make-string (file-length strm))))
      (read-sequence string strm)
      string)))

(test self-compilation-test
  ;; The big one. Prepare for pain and suffering. TODO: Recompile more
  ;; stuff. Once McCLIM has a test suite worthy of the name, recompile
  ;; that as well.
  (if *run-self-compilation-test*
      (let ((pathnames (find-pathnames (asdf:find-system :drei-mcclim))))
        ;; Just start from one end and go through.
        (format t "Re-evaluating Drei code using the Lisp syntax parser~%")
        (dolist (pathname pathnames)
          (testing-lisp-syntax ((slurp-file pathname))
            ; Rebind because the `*current-syntax*' variable will be
            ; clobbered during the test.
            (let ((syntax *current-syntax*))
              (mapcar #'(lambda (form)
                          (when (drei-lisp-syntax::formp form)
                            (eval (form-to-object syntax form :read t))))
                      (drei-lisp-syntax::children
                       (slot-value syntax 'drei-lisp-syntax::stack-top))))))
        ;; If we're really lucky, the Lisp system will now run Drei
        ;; interpreted, making this test close to a whole-night event.
        ;; Also, as fun as infinite recursion would be... disable this
        ;; test before running the suite.
        (let ((*run-self-compilation-test* nil))
          (format t "Re-running Drei test suite with newly evaluated Drei definitions~%")
          (run-tests))) 
      (skip "Sensibly skipping self-compilation test")))
