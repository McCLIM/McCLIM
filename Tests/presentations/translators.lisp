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
  (let ((translator (define-presentation-translator pt-smoke-tr2
                ((or real string) string pt.smoke-ct2)
                (object)
              (etypecase object
                (real (format nil "~a" object))
                (string object)))))
    (labels ((find-translators (from to)
               (find-presentation-translators from to 'pt.smoke-ct2))
             (is-applicable (from to)
               (is (member translator (find-translators from to))
                   "~@<Expected ~A to be applicable when translating from ~
                    ~S to ~S~@:>"
                   translator from to)
               ;; Run the same query again, so the cache is used.
               (is (member translator (find-translators from to))
                   "~@<Expected ~A to be applicable when translating WITH ~
                    CACHE from ~S to ~S~@:>"
                   translator from to))
             (is-not-applicable (from to)
               (is (not (member translator (find-translators from to)))
                   "~@<Expected ~A to not be applicable when translating ~
                    from ~S to ~S~@:>"
                   translator from to)
               ;; Run the same query again, so the cache is used.
               (is (not (member translator (find-translators from to)))
                   "~@<Expected ~A to not be applicable when translating ~
                    WITH CACHE from ~S to ~S~@:>"
                   translator from to)))
      (is-applicable     'real                                'string)
      (is-applicable     'string                              'string)
      (is-applicable     '(or string real)                    'string)
      (is-applicable     '(or string integer)                 'string)
      (is-applicable     '(and string (completion ("a" "b"))) 'string)
      (is-applicable     '(completion ("a" "b"))              'string)
      (fails (is-applicable     '(completion ("dan" 3))              'string))
      (is-not-applicable 'number                              'string)
      (is-not-applicable '(or string number)                  'string)
      (is-not-applicable '(completion ("dan" :foo))           'string)
      ;; Make sure meta type as "to" type do not result in invalid caching.
      (is-applicable     'real                                '(or real string))
      (fails (is-not-applicable 'real                                '(or real number))))))
