;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Unit tests for the standard presentations types.

(cl:in-package #:clim-tests)

(def-suite* :mcclim.presentations.standard-presentations
  :in :mcclim.presentations)

(defun make-presentation-type (presentation-type-name parameters)
  (if (typep parameters '(cons list t))
      (apply #'make-presentation-type-specifier
             `(,presentation-type-name ,@(first parameters))
             (rest parameters))
      (make-presentation-type-specifier
       `(,presentation-type-name ,@parameters))))

(defun %typep-cases (presentation-type-name cases)
  (map nil (lambda (case)
             (destructuring-bind (parameters object expected) case
               (let* ((presentation-type (make-presentation-type
                                          presentation-type-name parameters))
                      (result            (presentation-typep
                                          object presentation-type)))
                 (is (eql expected result)
                     "For presentation type ~S and value ~S, expected ~
                      ~S but got ~S"
                     presentation-type object expected result))))
       cases))

(defmacro typep-cases ((presentation-type-name) &body clauses)
  `(%typep-cases ',presentation-type-name (list ,@clauses)))

(defun %present-cases (presentation-type-name cases)
  (map nil (lambda (case)
             (destructuring-bind (parameters object expected-string) case
               (let* ((presentation-type (make-presentation-type
                                          presentation-type-name parameters))
                      (string            (present-to-string object presentation-type
                                                            :acceptably t)))
                 (is (string= expected-string string)
                     "For presentation type ~S and object ~S, expected ~
                      string ~S but got ~S"
                     presentation-type object expected-string string))))
       cases))

(defmacro present-cases ((presentation-type-name) &body clauses)
  `(%present-cases ',presentation-type-name (list ,@clauses)))

(defun %accept-cases (presentation-type-name cases)
  (map nil (lambda (case)
             (destructuring-bind
                 (parameters input expected-value
                  &optional (expected-position (1+ (length input))) ; activation gestures
                            (expected-type     (make-presentation-type
                                                presentation-type-name parameters)))
                 case
               (let ((presentation-type (make-presentation-type
                                         presentation-type-name parameters)))
                 (flet ((do-it ()
                          (accept-from-string presentation-type input)))
                   (case expected-value
                     (parse-error
                      (signals parse-error (do-it)))
                     (t
                      (multiple-value-bind (value type position) (do-it)
                        (is (equalp expected-value value)
                            "For presentation type ~S and input ~S, ~
                             expected value ~S but got ~S"
                            presentation-type input expected-value value)
                        (is (equalp expected-type type)
                            "For presentation type ~S and input ~S, ~
                             expected type ~S but got ~S"
                            presentation-type input expected-type type)
                        (is (eql expected-position position)
                            "For presentation type ~S and input ~S, ~
                             expected final position ~D but got ~D"
                            presentation-type input expected-position position))))))))
       cases))

(defmacro accept-cases ((presentation-type-name) &body clauses)
  `(%accept-cases ',presentation-type-name (list ,@clauses)))

(defmacro define-presentation-type-tests
    ((presentation-type-name) &key typep present accept)
  (flet ((test-name (method)
           (alexandria:symbolicate
            '#:standard-presentations. presentation-type-name '#:. method))
         (test-documentation (method)
           (format nil "Smoke test for the `~(~A~)' method for type ~
                        `~(~A~)'."
                   method presentation-type-name)))
    `(progn
       ,@(when typep
           `((test ,(test-name 'typep)
               ,(test-documentation 'typep)
               (,(if (typep typep '(cons (eql fails))) 'fails 'progn)
                (typep-cases (,presentation-type-name)
                  ,@(if (typep typep '(cons (eql fails))) (second typep) typep))))))
       ,@(when present
           `((test ,(test-name 'present)
               ,(test-documentation 'present)
               (,(if (typep present '(cons (eql fails))) 'fails 'progn)
                (let ((*read-default-float-format* 'single-float))
                  (present-cases (,presentation-type-name)
                    ,@(if (typep present '(cons (eql fails))) (second present) present)))))))
       ,@(when accept
           `((test ,(test-name 'accept)
               ,(test-documentation 'accept)
               (,(if (typep accept '(cons (eql fails))) 'fails 'progn)
                (accept-cases (,presentation-type-name)
                  ,@(if (typep accept '(cons (eql fails))) (second accept) accept)))))))))

(define-presentation-type-tests (null)
  :typep
  ('(() 1 nil))
  :present
  ('(() nil "None"))
  :accept
  (;; Invalid
   '(() "1"    parse-error)
   ;; Valid
   ;; '(() ""     nil)
   '(() "None" nil)))

(define-presentation-type-tests (boolean)
  :typep
  ('(() 1   nil)
   '(() nil t)
   '(() t   t))
  :present
  ('(() nil "No")
   '(() t   "Yes"))
  :accept
  (fails
    (;; Invalid
     '(() "foo" parse-error)
     ;; Valid
     '(() "yes" t)
     '(() "Yes" t)
     '(() "no"  nil)
     '(() "No"  nil))))

(define-presentation-type-tests (symbol)
  :typep
  ('(() 1    nil)
   '(() nil  t)
   '(() :foo t))
  :present
  ('(() :foo ":FOO")
   '(() mod  "MOD"))
  :accept
  (;; Invalid
   '(() "5"    parse-error)
   ;; Valid
   '(() ":foo" :foo)
   '(() ":FOO" :foo)
   '(() "mod"  mod)
   '(() "MOD"  mod)))

(define-presentation-type-tests (keyword)
  :typep
  ('(() 1    nil)
   '(() nil  nil)
   '(() :foo t))
  :present
  ('(() :foo ":FOO"))
  :accept
  (;; Invalid
   '(() "mod"  parse-error)
   ;; Valid
   '(() ":foo" :foo)
   '(() ":FOO" :foo)))

(define-presentation-type-tests (number)
  :typep
  ('(() nil     nil)
   '(() 1       t)
   '(() 1/2     t)
   '(() 1.3     t)
   '(() #C(1 2) t))
  :present
  ('(() 1       "1")
   '(() 1/2     "1/2")
   '(() 1.5f0   "1.5")
   '(() #C(1 2) "#C(1 2)"))
  :accept
  (;; Invalid
   '(() ""        parse-error)
   '(() ":foo"    parse-error)
   ;; Valid
   '(() "1"       1)
   '(() "1/2"     1/2)
   '(() "1.5f0"   1.5f0)
   '(() "#C(1 2)" #C(1 2))))

(define-presentation-type-tests (complex)
  :typep
  ('(()        nil         nil)
   '(()        1           nil)
   '((integer) #C(1/2 2/3) nil)
   '(()        #C(1/2 2/3) t)
   '((integer) #C(1 2)     t))
  :present
  ('(() 1       "1 0")
   '(() #C(1 2) "1 2"))
  :accept
  (fails
    (;; Invalid
     '(()        ":foo"      parse-error)
     '((integer) "1.2f0 .5"  parse-error)
      ;; Valid
     '(()        "1 0"       1)
     '(()        "1 2"       #C(1 2))
     '((integer) "1 0"       1)
     '((integer) "1 2"       #C(1 2))
     '((real)    "1/2 0.5f0" #C(.5f0 .5f0))
     '((real)    "1/2 3/4"   #C(1/2 3/4)))))

(define-presentation-type-tests (real)
  :typep
  ('(() nil     nil)
   '(() 1       t)
   '(() 1/2     t)
   '(() 1.3     t)
   '(() #C(1 2) nil))
  :present
  ('(()                     15 "15")
   '((() :base 16)          15 "F")
   '((() :base 16 :radix t) 15 "#xF"))
  :accept
  (fails
    (;; Invalid
     '(()            "f"   parse-error)
     ;; Valid
     '(()            "15"  15)
     '(()            "#xf" 15)
     '((() :base 16) "f"   15)
     '(()            "1/2" 1/2))))

(define-presentation-type-tests (integer)
  :typep
  ('(() nil     nil)
   '(() 1       t)
   '(() 1/2     nil)
   '(() 1.3     nil)
   '(() #C(1 2) nil))
  :present
  ('(()                     15 "15")
   '((() :base 16)          15 "F")
   '((() :base 16 :radix t) 15 "#xF"))
  :accept
  (fails
    (;; Invalid
     '(()            "1/2" parse-error)
     '(()            "f"   parse-error)
     ;; Valid
     '(()            "15"  15)
     '(()            "#xf" 15)
     '((() :base 16) "f"   15))))

(define-presentation-type-tests (character)
  :typep
  ('(() nil nil)
   '(() 1   nil)
   '(() #\a t))
  :present
  ('(() #\a "a"))
  #+not-implemented :accept
  #+not-implemented (;; Invalid
                     '(() "no-such-character" parse-error)
                     ;; Valid
                     '(() "a"                 #\a)))

(define-presentation-type-tests (string)
  :typep
  ('(() nil nil)
   '(() ""  t))
  :present
  ('(()  ""    "\"\"")
   '(()  "foo" "\"foo\"")
   '((3) "foo" "\"foo\""))
  :accept
  (;; Invalid
   '((5) "foo" parse-error)
   ;; Valid
   '(()  "foo" "foo")))

(define-presentation-type-tests (pathname)
  :typep
  ('(() nil     nil)
   '(() ""      nil)
   '(() #P"foo" t))
  :present
  ('(() "foo"    "foo")
   '(() #P"foo"  "foo")
   '(() "foo/"   "foo/")
   '(() #P"foo/" "foo/"))
  :accept
  (;; Invalid
   '(() ""               parse-error)
   ;; Valid
   '(() "does-not-exist" "does-not-exist" 15 string)
   (let* ((pathname   #.(or *compile-file-pathname*
                            *load-pathname*))
          (namestring (namestring pathname)))
     `(() ,namestring ,pathname))))

(define-presentation-type-tests (completion)
  :typep
  ('((((1 2 3)))                   :foo nil)
   '((((1 2 3)))                   4    nil)
   '((((1 2 3)))                   1    t)
   '((((-1 -2 -3) :value-key abs)) 1 t  ))
  :present
  ('((((1 2 3)))                               1    "1")
   '((((1 2 3)))                               2    "2")
   '((((:foo :bar)))                           :foo "Foo")
   '((((:foo :bar)) :name-key string-downcase) :foo "foo"))
  :accept
  (fails
    (;; Invalid
     '((((1 2 3)))                                            "4"   parse-error)
     ;; Valid
     '((((1 2 3)))                                            "1"   1)
     '((((1 2 3)))                                            "2"   2)
     '((((-1 -2 -3) :value-key abs))                          "-2"  2)
     '((((("a" . 1) ("b" . 2)) :value-key cdr) :name-key car) "a"   1)
     '((((:foo :bar)))                                        "Foo" :foo)
     '((((:foo :bar)))                                        "foo" :foo))))

(define-presentation-type-tests (subset-completion)
  :typep
  ('((((1 2 3)))                                            1     nil)
   '((((1 2 3)))                                            4     nil)
   '((((1 2 3)))                                            ()    t)
   '((((1 2 3)))                                            (1 2) t)
   '((((-1 -2 -3) :value-key abs))                          (4)   nil)
   '((((-1 -2 -3) :value-key abs))                          (1)   t)
   '((((("a" . 1) ("b" . 2)) :value-key cdr) :name-key car) (3)   nil)
   '((((("a" . 1) ("b" . 2)) :value-key cdr) :name-key car) (1)   t))
  :present
  ('((((1 2 3)))                               ()     "")
   '((((1 2 3)))                               (1 3)  "1,3")
   '((((:foo :bar)))                           (:foo) "Foo")
   '((((:foo :bar)) :name-key string-downcase) (:foo) "foo"))
  #+not-implemented :accept
  #+not-implemented (;; Invalid
                     '((((1 2 3)))                                            "4"   parse-error)
                     ;; Valid
                     '((((1 2 3)))                                            "1"   (1))
                     '((((1 2 3)))                                            "1,3" (1 3))
                     '((((-1 -2 -3) :value-key abs))                          "-2"  (2))
                     '((((("a" . 1) ("b" . 2)) :value-key cdr) :name-key car) "a"   (1))
                     '((((:foo :bar)))                                        "Foo" (:foo))
                     '((((:foo :bar)))                                        "foo" (:foo))))

(define-presentation-type-tests (sequence)
  :typep
  ('((number) 1     nil)
   '((number) (:a)  nil)
   '((number) ()    t)
   '((number) (1)   t)
   '((number) (1 2) t))
  :present
  (fails
    ('((number)  ()       "")
     '((number)  #()      "")
     '((number)  (1)      "1")
     '((number)  #(1)     "1")
     '((boolean) (nil t)  "No,Yes")
     '((boolean) #(nil t) "No,Yes")))
  :accept
  (;; Invalid
   '((boolean)        "1"       parse-error)
   '((number)         "Yes"     parse-error)
   '((number)         "1,Yes"   parse-error)
   ;; Valid
   '((number)         "1"       (1))
   '((number)         "1,2"     (1 2))
   '((t)              "1, :foo" (1 :foo))))

(define-presentation-type-tests (sequence-enumerated)
  :typep
  (fails
    ('(()              1      nil)
     '((number)        (:a)   nil)
     '((number number) (1)    nil)
     '((number)        (1 2)  nil)
     '(()              ()     t)
     '((number)        (1)    t)
     '((number symbol) (1 :a) t)))
  :present
  ('(()               ()     "")
   '(()               #()    "")
   '((number)         (1)    "1")
   '((number)         #(1)   "1")
   '((number boolean) (1 t)  "1,Yes")
   '((number boolean) #(1 t) "1,Yes")
   '((number number)  (1 2)  "1,2")
   '((number number)  #(1 2) "1,2"))
  :accept
  (;; Invalid
   '((boolean)        "1"     parse-error)
   '((number number)  "1"     parse-error)
   '((number)         "1, 2"  (1)         2)
   ;; Valid
   '((number)         "1"     (1))
   '((number boolean) "1,yes" (1 t))
   '((number number)  "1, 2"  (1 2))))

(define-presentation-type-tests (or)
  :typep
  ('(()              :foo nil)
   '((number)        :foo nil)
   '((number symbol) ""   nil)
   '((number)        1    t)
   '((number symbol) 1    t)
   '((number symbol) :foo t))
  :present
  ('((number)          1    "1")
   '((number integer)  1    "1")
   '((boolean symbol)  t    "Yes")
   '((boolean symbol)  :foo ":FOO")
   '((symbol boolean)  t    "T")
   '((symbol boolean)  :foo ":FOO"))
  :accept
  (;; Invalid
   '(()               "1"     parse-error)
   '((number)         "T"     parse-error)
   ;; Valid
   '((number)         "1"     1      2 number)
   '((integer number) "1"     1      2 integer)
   '((integer number) "1.2f0" 1.2f0  6 number)
   '((boolean symbol) "Yes"   t      4 boolean)
   '((boolean symbol) ":FOO"  :foo   5 symbol)
   '((symbol boolean) "Yes"   yes    4 symbol)
   '((symbol boolean) ":FOO"  :foo   5 symbol)))

(define-presentation-type-tests (and)
  :typep
  ('((number)        :foo nil)
   '((number real)   ""   nil)
   '(()              :foo t)
   '((number)        1    t)
   '((number real)   1    t)
   '((number real)   1.0  t))
  :present
  ('((number)          1 "1")
   '((number integer)  1 "1")
   '((boolean symbol)  t "Yes")
   '((symbol boolean)  t "T"))
  :accept
  (;; Invalid
   '(()               "1"     parse-error)
   '((number)         "T"     parse-error)
   '((number integer) "1.2"   parse-error)
   ;; Valid
   '((number)         "1"    1)
   '((number integer) "1"    1)
   '((boolean symbol) "Yes"  t)
   '((symbol boolean) "T"    t)))
