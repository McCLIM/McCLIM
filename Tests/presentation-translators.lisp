(cl:in-package #:clim-tests)

(def-suite* :mcclim.presentation-translators
  :in :mcclim)

(test presentation-translators.smoke
  (define-command-table pt.smoke-ct)
  (let ((tr (define-presentation-translator pt-smoke-tr
                (integer string pt.smoke-ct)
                (object)
              (format nil "~a" object)))
        (all (find-presentation-translators 'integer 'string 'pt.smoke-ct)))
    (is (member tr all))))

(test presentation-translators.meta
  (define-command-table pt.smoke-ct2)
  (let ((tr (define-presentation-translator pt-smoke-tr2
                ((or real string) string pt.smoke-ct2)
                (object)
              (etypecase object
                (real (format nil "~a" object))
                (string object))))
        (all-1 (find-presentation-translators 'real 'string 'pt.smoke-ct2))
        (all-2 (find-presentation-translators 'string 'string 'pt.smoke-ct2))
        (all-3 (find-presentation-translators '(or string real) 'string 'pt.smoke-ct2))
        (all-4 (find-presentation-translators '(or string integer) 'string 'pt.smoke-ct2))
        (all-5 (find-presentation-translators '(and string (member "a" "b")) 'string 'pt.smoke-ct2))
        (all-6 (find-presentation-translators '(member "a" "b") 'string 'pt.smoke-ct2))
        (all-7 (find-presentation-translators '(member "dan" 3) 'string 'pt.smoke-ct2))
        (non-1 (find-presentation-translators 'number 'string 'pt.smoke-ct2))
        (non-2 (find-presentation-translators '(or string number) 'string 'pt.smoke-ct2))
        (non-3 (find-presentation-translators '(member "dan" :foo) 'string 'pt.smoke-ct2)))
    (is (member tr all-1))
    (is (member tr all-2))
    (is (member tr all-3))
    (is (member tr all-4))
    (is (member tr all-5))
    (is (member tr all-6))
    (is (member tr all-7))
    (is (not (member tr non-1)))
    (is (not (member tr non-2)))
    (is (not (member tr non-3)))))
