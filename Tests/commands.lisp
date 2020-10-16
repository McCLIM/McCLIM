(cl:in-package #:clim-tests)

(def-suite* :mcclim.commands
  :in :mcclim)

;;; Utilities

(defmacro with-command-table ((var name &rest args) &body body)
  (alexandria:once-only (name)
    `(let ((,var (make-command-table ,name ,@args)))
       (declare (ignorable ,var))
       (unwind-protect
            (progn ,@body)
         (remhash ,name climi::*command-tables*)))))

(defmacro with-command-tables (tables &body body)
  (destructuring-bind (first-table &rest more-tables) tables
    (if more-tables
        `(with-command-table ,first-table
           (with-command-tables ,more-tables
             ,@body))
        `(with-command-table ,first-table
           ,@body))))


;;; command tables

(test commands.map-over-command-table-keystrokes.no-menu
  (with-command-table (ct nil)
    (let ((count 0))
      (add-command-to-command-table '(com-test-command) ct :keystroke '(#\t))
      (map-over-command-table-keystrokes
       (lambda (menu-name gesture item)
         (incf count)
         (is (equal nil menu-name))
         (is (equal '(:keyboard #\t 0) gesture))
         (is (equal (lookup-keystroke-command-item gesture ct)
                    (command-menu-item-value item))))
       ct)
      (is (= 1 count)))))

(test commands.map-over-command-table-keystrokes.menu
  (let ((count 0)
        (ct (make-command-table nil)))
    (add-command-to-command-table
     '(com-test-command) ct :keystroke '(#\u) :menu "Test")
    (map-over-command-table-keystrokes
     (lambda (menu-name gesture item)
       (incf count)
       (is (equal "Test" menu-name))
       (is (equal '(:keyboard #\u 0) gesture))
       (is (equal (lookup-keystroke-command-item gesture ct)
                  (command-menu-item-value item))))
     ct)
    (is (= 1 count))))

(test commands.add-and-remove-command
  (let ((ct (make-command-table nil)))
    (add-command-to-command-table 'com-test-command ct)
    (remove-command-from-command-table 'com-test-command ct)
    (signals command-not-present
      (remove-command-from-command-table 'com-test-command ct))))


;;; command table menu items and keystrokes definition

;;; The :MENU option should behave the same as if we had added menu items
;;; manually.
(test commands.command-table.menu-is-menu
  (let ((menu-item `(("Foo" :command (com-foo) :keystroke :abort))))
    (with-command-tables ((ct1 nil :inherit-from nil :menu menu-item)
                          (ct2 nil :inherit-from nil))
      (apply #'add-menu-item-to-command-table ct2 (car menu-item))
      (is (not (null (lookup-keystroke-item :abort ct1 :test #'eql))))
      (is (not (null (lookup-keystroke-item :abort ct2 :test #'eql)))))))

;;; When adding menu items with keystrokes, the keystroke items should be
;;; present as if they were added with add-keystrokes-to-command-table.
(test commands.command-table.menu-is-keystrokes
  (with-command-tables ((ct1 nil :inherit-from nil)
                        (ct2 nil :inherit-from nil))
    (add-menu-item-to-command-table ct1 "Foo" :command '(com-foo)
                                              :keystroke :abort)
    (add-keystroke-to-command-table ct2 :abort :command '(com-foo))
    (is (not (null (lookup-keystroke-item :abort ct1 :test #'eql))))
    (is (not (null (lookup-keystroke-item :abort ct2 :test #'eql)))))
  ;; vvv is a variant of COMMANDS.COMMAND-TABLE.MENU-IS-MENU.
  (let ((menu-item `(("Foo" :command (com-foo) :keystroke :abort))))
    (with-command-tables ((ct1 nil :inherit-from nil :menu menu-item)
                          (ct2 nil :inherit-from nil))
      (add-keystroke-to-command-table ct2 :abort :command '(com-foo))
      (is (not (null (lookup-keystroke-item :abort ct1 :test #'eql))))
      (is (not (null (lookup-keystroke-item :abort ct2 :test #'eql)))))))

;;; When adding menu items with keystrokes and when the name is NIL, the
;;; operator should behave exactly as if we had added only keystrokes.
(test commands.command-table.menu-is-keystrokes*
  (flet ((check-keystroke-not-menu (ct)
           (map-over-command-table-menu-items
            (lambda (&rest args)
              (declare (ignore args))
              (fail "Menu item found (it shouldn't be)."))
            ct)
           (block nil
             (map-over-command-table-keystrokes
              (lambda (&rest args)
                (declare (ignore args))
                (return (pass)))
              ct)
             (fail "Keystroke not found (it should be)."))))
    (with-command-table (ct nil :inherit-from nil)
      (add-menu-item-to-command-table ct nil :command '(com-foo)
                                             :keystroke :abort)
      (check-keystroke-not-menu ct))
    (with-command-table (ct nil :inherit-from nil)
      (add-keystroke-to-command-table ct :abort :command '(com-foo))
      (check-keystroke-not-menu ct))))


;;; command table inheritance

(test commands.command-table-inheritance.smoke
  ;; This test verifies that all inherited command tables are traversed.
  (with-command-tables
      ((ct nil :inherit-from ())
       (ct.1 nil :inherit-from (list ct))
       (ct.2 nil :inherit-from (list ct))
       (ct.2.1 nil :inherit-from (list ct.2))
       (ct.2.2 nil :inherit-from (list ct.2))
       (ct.2.* nil :inherit-from (list ct.2.1 ct.2.2))
       (ct.x.x nil :inherit-from (list ct ct.1))
       (ct.x.y nil :inherit-from (list ct ct)))
    (let ((acc nil))
      (do-command-table-inheritance (ct ct.2.*)
        ;; This macro may traverse the same command table multiple times,
        ;; because the ancestor may be linked in a few places in the inheritance
        ;; tree with different :inherit-menu values.
        (pushnew ct acc))
      (is (alexandria:set-equal (list ct ct.2 ct.2.1 ct.2.2 ct.2.*) acc)))))


;;; command table errors (see 27.2)

(test commands.command-table-errors
  (is (subtypep 'command-table-error 'error))
  (is (subtypep 'command-table-not-found 'command-table-error))
  (is (subtypep 'command-table-already-exists 'command-table-error))
  (is (subtypep 'command-not-present 'command-table-error))
  (is (subtypep 'command-not-accessible 'command-table-error))
  (is (subtypep 'command-already-present 'command-table-error))
  (let ((condition (make-condition 'command-table-error
                                   :format-control "~A"
                                   :format-arguments '(!))))
    (is-true (find #\! (format nil "~A" condition)))))

;;; not actually required to DTRT here, but we use this form (without
;;; control and arguments) internally, so make sure that we don't
;;; error out recursively when in the debugger with one of these.

(test commands.command-no-present.print
  (let ((condition (make-condition 'command-not-present)))
    (is-true (stringp (format nil "~A" condition)))))

;;; Presentation translators

(test commands.find-presentation-translator.smoke
  (with-command-table (table 'test)
    ;; Not present - should signal
    (signals command-not-present
      (find-presentation-translator 'dummy-translator 'test))
    ;; Not present, but with ERRORP being NIL - should not signal
    (is (eq nil (find-presentation-translator
                 'dummy-translator 'test :errorp nil)))
    ;; Present - should be found.
    (let ((translator (define-presentation-translator dummy-translator
                          (integer string test)
                          (object)
                        (princ-to-string object))))
      (is (eq translator
              (find-presentation-translator 'dummy-translator 'test))))))

(test commands.add-presentation-translator-to-command-table.smoke
  (with-command-table (table1 'test1)
    (let ((translator (define-presentation-translator dummy-translator
                          (integer string test1)
                          (object)
                        (princ-to-string object))))
      (with-command-table (table2 'test2)
        ;; Not present
        (add-presentation-translator-to-command-table table2 translator)
        (is-true (find-presentation-translator 'dummy-translator 'test2))
        ;; Already present - should signal.
        (signals command-already-present
          (add-presentation-translator-to-command-table table2 translator))
        ;; Already present, but with ERRORP being NIL - should not signal.
        (finishes (add-presentation-translator-to-command-table
                   table2 translator :errorp nil))))))

(test commands.remove-presentation-translator-from-command-table.smoke
  (with-command-table (table 'test)
    ;; Not present - should signal.
    (signals command-not-present
      (remove-presentation-translator-from-command-table
       'test 'dummy-translator))
    ;; Not present, but with ERRORP being NIL - should do nothing.
    (remove-presentation-translator-from-command-table
     'test 'dummy-translator :errorp nil)
    ;; Present - should be removed.
    (define-presentation-translator dummy-translator
        (integer string test)
        (object)
      (princ-to-string object))
    (is-true (find-presentation-translator
              'dummy-translator 'test :errorp nil))
    (remove-presentation-translator-from-command-table 'test 'dummy-translator)
    (is-false (find-presentation-translator
               'dummy-translator 'test :errorp nil))))
