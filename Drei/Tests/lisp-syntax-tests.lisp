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

(def-suite lisp-syntax-tests :description "The test suite for
tests related to the Lisp syntax module.")

(in-suite lisp-syntax-tests)

(defvar *run-self-compilation-test* nil
  "If true, running the Lisp syntax module test suite will
involve an extreme stress test wherein the Lisp parser will be
used to read in the Drei source code, recompile Drei based on the
parser result and re-run the test suite (except for this
self-compilation test, of course).")

(defmacro testing-lisp-syntax ((buffer-contents) &body body)
  `(with-buffer (buffer :initial-contents ,buffer-contents
                        :syntax 'lisp-syntax)
     (flet ((get-object (&rest args)
              (apply #'form-to-object (syntax buffer)
                     (first (drei-lisp-syntax::children
                             (slot-value (syntax buffer)
                                         'drei-lisp-syntax::stack-top)))
                     args)))
       ,@body)))

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
  `(with-buffer (buffer :initial-contents ,buffer-contents
                        :syntax 'lisp-syntax)
     (flet ((get-object (&rest args)
              (apply #'form-to-object (syntax buffer)
                     (first (drei-lisp-syntax::children
                             (slot-value (syntax buffer)
                                         'drei-lisp-syntax::stack-top)))
                     args)))
       (testing-symbol (,sym-sym ,@args)
         ,@body))))

(test form-to-object-1
  (testing-lisp-syntax ("T")
    (is (eq (get-object) t)))
  (testing-lisp-syntax ("t")
    (is (eq (get-object) t))))

(test form-to-object-2
  (testing-lisp-syntax ("nil")
    (is (eq (get-object) nil)))
  (testing-lisp-syntax ("NIL")
    (is (eq (get-object) nil)))
  (testing-lisp-syntax ("NIl")
    (is (eq (get-object) nil)))
  (testing-lisp-syntax ("NIl")
    (is-false (eq (get-object :case :preserve) nil))))

(test form-to-object-3
  (testing-lisp-syntax ("iDoNotExist")
    (testing-symbol (sym :case :upcase)
      (is-false (symbol-package sym))
      (is (string= (symbol-name sym)
                   "IDONOTEXIST")))
    (testing-symbol (sym :case :preserve)
      (is-false (symbol-package sym))
      (is (string= (symbol-name sym)
                   "iDoNotExist")))
    (testing-symbol (sym :case :downcase)
      (is-false (symbol-package sym))
      (is (string= (symbol-name sym)
                   "idonotexist")))
    (testing-symbol (sym :read t :case :upcase)
      (is-true (symbol-package sym))
      (is (string= (symbol-name sym)
                   "IDONOTEXIST")))
    (testing-symbol (sym :read t :case :preserve)
      (is-true (symbol-package sym))
      (is (string= (symbol-name sym)
                   "iDoNotExist")))
    (testing-symbol (sym :read t :case :downcase)
      (is-true (symbol-package sym))
      (is (string= (symbol-name sym)
                   "idonotexist")))
    (testing-symbol (sym :case :invert)
      (is-false (symbol-package sym))
      (is (string= (symbol-name sym)
                   "iDoNotExist"))))
  (testing-lisp-syntax-symbol ("IDONOTEXIST" sym :case :invert)
    (is-false (symbol-package sym))
    (is (string= (symbol-name sym)
                 "idonotexist")))
  (testing-lisp-syntax-symbol ("idonotexist" sym :case :invert)
    (is-false (symbol-package sym))
    (is (string= (symbol-name sym)
                 "IDONOTEXIST"))))

(test form-to-object-4
  (testing-lisp-syntax ("#:iDoNotExist")
    (testing-symbol (sym :case :upcase)
      (is-false (symbol-package sym))
      (is (string= (symbol-name sym)
                   "IDONOTEXIST")))
    (testing-symbol (sym :case :preserve)
      (is-false (symbol-package sym))
      (is (string= (symbol-name sym)
                   "iDoNotExist")))
    (testing-symbol (sym :case :downcase)
      (is-false (symbol-package sym))
      (is (string= (symbol-name sym)
                   "idonotexist")))
    (testing-symbol (sym :case :invert)
      (is-false (symbol-package sym))
      (is (string= (symbol-name sym)
                   "iDoNotExist"))))
  (testing-lisp-syntax ("#:IDONOTEXIST")
    (let ((sym (get-object :case :invert)))
      (is-false (symbol-package sym))
      (is (string= (symbol-name sym)
                   "idonotexist"))))
  (testing-lisp-syntax ("#:idonotexist")
    (let ((sym (get-object :case :invert)))
      (is-false (symbol-package sym))
      (is (string= (symbol-name sym)
                   "IDONOTEXIST")))))

(test form-to-object-5
  (testing-lisp-syntax-symbol ("|123|" sym :read t)
    (is (string= (symbol-name sym) "123")))
  (testing-lisp-syntax-symbol ("|LIST|" sym :read t :case :downcase)
    (is (string= (symbol-name sym) "LIST")))
  (testing-lisp-syntax-symbol ("|   |" sym :read t)
    (is (string= (symbol-name sym) "   ")))
  (testing-lisp-syntax-symbol ("|foo|bar|abbabz|" sym :read t)
    (is (string= (symbol-name sym)
                 "fooBARabbabz")))
  (testing-lisp-syntax-symbol ("||" sym :read t)
    (is (string= (symbol-name sym) "")))
  (testing-lisp-syntax-symbol ("||||" sym :read t)
    (is (string= (symbol-name sym) ""))))

(test form-to-object-6
  (testing-lisp-syntax-symbol (":foo" sym :read t)
    (is (string= (symbol-name sym) "FOO"))
    (is (eq (symbol-package sym)
            (find-package :keyword)))))

(test form-to-object-7
  (testing-lisp-syntax ("123")
    (is (= (get-object) 123)))
  (testing-lisp-syntax ("-123")
    (is (= (get-object) -123)))
  (testing-lisp-syntax (".123")
    (is (= (get-object) .123)))
  (testing-lisp-syntax ("-.123")
    (is (= (get-object) -.123)))
  (testing-lisp-syntax ("1.234")
    (is (= (get-object) 1.234)))
  (testing-lisp-syntax ("-1.234")
    (is (= (get-object) -1.234)))
  (testing-lisp-syntax ("1e7")
    (is (= (get-object) 1e7)))
  (testing-lisp-syntax ("1E7")
    (is (= (get-object) 1e7)))
  (testing-lisp-syntax ("1.123E7")
    (is (= (get-object) 1.123e7)))
  (testing-lisp-syntax ("-1.123E7")
    (is (= (get-object) -1.123e7)))
  (testing-lisp-syntax (".123E7")
    (is (= (get-object) .123e7)))
  (testing-lisp-syntax ("-.123E7")
    (is (= (get-object) -.123e7)))
  (testing-lisp-syntax ("1.34e-7")
    (is (= (get-object) 1.34e-7))))

(test form-to-object-8
  (testing-lisp-syntax ("#b0000")
    (is (= (get-object) 0)))
  (testing-lisp-syntax ("#b10")
    (is (= (get-object) 2)))
  (testing-lisp-syntax ("#b-10")
    (is (= (get-object) -2)))
  (testing-lisp-syntax ("#x00")
    (is (= (get-object) 0)))
  (testing-lisp-syntax ("#xFE")
    (is (= (get-object) 254)))
  (testing-lisp-syntax ("#x-FE")
    (is (= (get-object) -254)))
  (testing-lisp-syntax ("#o00")
    (is (= (get-object) 0)))
  (testing-lisp-syntax ("#o71")
    (is (= (get-object) 57)))
  (testing-lisp-syntax ("#o-71")
    (is (= (get-object) -57))))

(test form-to-object-9
  (testing-lisp-syntax ("#\\a")
    (is (char= (get-object) #\a)))
  (testing-lisp-syntax ("#\\Null")
    (is (char= (get-object) #\Null)))
  (testing-lisp-syntax ("#\\NULL")
    (is (char= (get-object) #\Null)))
  (testing-lisp-syntax ("#\\ ")
    (is (char= (get-object) #\Space))))

(test form-to-object-10
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
  (testing-lisp-syntax ("(t ")
    (finishes
      (get-object))
    (signals form-conversion-error
      (get-object :read t))
    (finishes
      (get-object :read t :no-error t))))

(test form-to-object-14
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
  (testing-lisp-syntax ("#+mcclim t")
    (is (eq (get-object) (or #+mcclim t))))
  (testing-lisp-syntax ("#-mcclim t")
    (is (eq (get-object) (or #-mcclim t))))
  (testing-lisp-syntax ("(#+mcclim t)")
    (is (equal (get-object) '(#+mcclim t))))
  (testing-lisp-syntax ("(#-mcclim t)")
    (is (equal (get-object) '(#-mcclim t)))))

(test form-to-object-17
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
    (let ((form (drei-lisp-syntax::form-before (syntax buffer) 14)))
      (is (eq (form-to-object (syntax buffer) form) 'list))))
  (testing-lisp-syntax ("(#1=list #1=cons)")
    (signals form-conversion-error
      (get-object))))

(test form-to-object-18
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
  ;; The big one. Prepare for pain and suffering.
  (if *run-self-compilation-test*
      (let ((pathnames (find-pathnames (asdf:find-system :drei-mcclim))))
        ;; Just start from one end and go through.
        (format t "Re-evaluating Drei code using the Lisp syntax parser~%")
        (dolist (pathname pathnames)
          (testing-lisp-syntax ((slurp-file pathname))
            (let ((syntax (syntax buffer)))
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
