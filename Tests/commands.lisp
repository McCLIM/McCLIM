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

(defmacro with-gestures ((&rest gestures) &body body)
  (loop for (name . spec) in gestures
        collect `(define-gesture-name ,name :keyboard ,spec) into defines
        collect `(delete-gesture-name ,name) into deletes
        finally (return `(progn
                           ,@defines
                           (unwind-protect (progn ,@body)
                             ,@deletes)))))


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
  (with-command-table (ct nil :inherit-from nil)
    (with-gestures ((:x #\x) (:y #\y))
      (add-command-to-command-table 'com-test1 ct)
      (add-command-to-command-table 'com-test2 ct :menu "test2")
      (add-command-to-command-table 'com-test3 ct :keystroke :x)
      (add-command-to-command-table 'com-test4 ct :menu "test4" :keystroke :y)
      ;;
      (is (find-menu-item "test2" ct :errorp nil))
      (is (find-menu-item "test4" ct :errorp nil))
      (is (find-keystroke-item :x ct :test #'eql :errorp nil))
      (is (find-keystroke-item :y ct :test #'eql :errorp nil))
      ;;
      (remove-command-from-command-table 'com-test1 ct)
      (remove-command-from-command-table 'com-test2 ct)
      (remove-command-from-command-table 'com-test3 ct)
      (remove-command-from-command-table 'com-test4 ct)
      (signals command-not-present
        (remove-command-from-command-table 'com-test5 ct))
      ;;
      (is (null (find-menu-item "test2" ct :errorp nil)))
      (is (null (find-menu-item "test4" ct :errorp nil)))
      (is (null (find-keystroke-item :x ct :test #'eql :errorp nil)))
      (is (null (find-keystroke-item :y ct :test #'eql :errorp nil)))
      (mapc (lambda (com)
              (signals command-not-present
                (remove-command-from-command-table com ct)))
            '(com-test1 com-test2 com-test3 com-test4)))))


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

(defmacro with-keystroke-item-testers (nil &body body)
  `(labels ((test-find (gesture table expected-item expected-table)
              (multiple-value-bind (item found-table)
                  (find-keystroke-item gesture table :test #'eql :errorp nil)
                (is (equal (list expected-item expected-table)
                           (list (and item (command-menu-item-value item)) found-table)))
                ;; When find-keystroke finds the item, lookup-keystroke
                ;; shoudl always return the same result.
                (when (and item found-table)
                  (test-look gesture table expected-item expected-table))))
            (test-look (gesture table expected-item expected-table)
              (multiple-value-bind (item found-table)
                  (lookup-keystroke-item gesture table :test #'eql)
                (is (equal (list expected-item expected-table)
                           (list (and item (command-menu-item-value item)) found-table))))))
     ,@body))

(test commands.command-table.keystroke.1
  (dolist (inherit-menu '(t :keystrokes))
    (with-gestures ((:w #\w) (:x #\x) (:y #\y) (:z #\z))
      (with-command-tables ((root-menu nil :menu `(("X" :command (com-x) :keystroke :x)))
                            (sub1-menu nil :menu `(("Y" :command (com-y) :keystroke :y)
                                                   ("W" :command (com-w) :keystroke :w)))
                            (root nil :menu `(("Menu" :menu ,root-menu)
                                              ("CMD1" :command (cmd-1) :keystroke :y)
                                              ("CMD2" :command (cmd-2) :keystroke :z)))
                            (sub1 nil :menu `(("Menu" :menu ,sub1-menu)
                                              ("CMD3" :command (cmd-3) :keystroke :z))
                                      :inherit-from (list root)
                                      :inherit-menu inherit-menu))
        (with-keystroke-item-testers ()
          ;; Find keystroke doesn't look in submenus while lookup does.
          (test-find :x sub1 nil nil)
          (test-look :x sub1 '(com-x) root-menu)
          (test-find :w sub1 nil nil)
          (test-look :w sub1 '(com-w) sub1-menu)
          ;; Entries are inherited
          (test-find :y sub1 '(cmd-1) root)
          ;; And may be shadowed
          (test-find :z root '(cmd-2) root)
          (test-find :z sub1 '(cmd-3) sub1))))))

;;; This test is similar to the previous one, however it examines
;;; :inherit-menu values that do not inherit keystrokes.
(test commands.command-table.keystroke.2
  (dolist (inherit-menu '(nil :menu))
    (with-gestures ((:w #\w) (:x #\x) (:y #\y) (:z #\z))
      (with-command-tables ((root-menu nil :menu `(("X" :command (com-x) :keystroke :x)))
                            (sub1-menu nil :menu `(("Y" :command (com-y) :keystroke :y)
                                                   ("W" :command (com-w) :keystroke :w)))
                            (root nil :menu `(("Menu" :menu ,root-menu)
                                              ("CMD1" :command (cmd-1) :keystroke :y)
                                              ("CMD2" :command (cmd-2) :keystroke :z)))
                            (sub1 nil :menu `(("Menu" :menu ,sub1-menu)
                                              ("CMD3" :command (cmd-3) :keystroke :z))
                                      :inherit-from (list root)
                                      :inherit-menu inherit-menu))
        (with-keystroke-item-testers ()
          ;; Find keystroke doesn't look in submenus while lookup does. In
          ;; this case lookup does not look into inherited submenus.
          (test-find :x sub1 nil nil)
          (test-look :x sub1 nil nil)
          (test-find :w sub1 nil nil)
          (test-look :w sub1 '(com-w) sub1-menu)
          ;; Inheritance is inhibited.
          (test-find :y sub1 nil nil))))))

;;; This tests verifies whether we correctly descend into the menu command
;;; table ancestors (or not) depending on :inherit-menu value. We add some
;;; conflicts in root command table to check whether inherited keystrokes take
;;; precedence over keystrokes that could be found in the menu.
(test commands.command-table.keystroke.3
  (with-gestures ((:w #\w) (:x #\x) (:y #\y) (:z #\z))
    (with-command-tables ((subs-menu nil :menu `(("X" :command (com-x) :keystroke :x)))
                          (sub1-menu nil :menu `(("Y" :command (com-y) :keystroke :y)
                                                 ("W" :command (com-w) :keystroke :w))
                                         :inherit-from (list subs-menu)
                                         :inherit-menu t)
                          (sub2-menu nil :menu `(("Y" :command (com-y) :keystroke :y)
                                                 ("W" :command (com-w) :keystroke :w))
                                         :inherit-from (list subs-menu)
                                         :inherit-menu nil)
                          (root nil :menu `(("CMD1" :command (cmd-1) :keystroke :y)
                                            ("CMD2" :command (cmd-2) :keystroke :z)))
                          (sub1 nil :menu `(("Menu" :menu ,sub1-menu))
                                    :inherit-from (list root)
                                    :inherit-menu t)
                          (sub2 nil :menu `(("Menu" :menu ,sub2-menu))
                                    :inherit-from (list root)
                                    :inherit-menu t)
                          (sub3 nil :menu `(("Menu" :menu ,sub2-menu))
                                    :inherit-from (list root)
                                    :inherit-menu nil))
      (with-keystroke-item-testers ()
        ;; Find keystroke doesn't look in submenus while lookup does. In
        ;; this case lookup does not look into inherited submenus.
        (test-find :x sub1 nil nil)
        (test-look :x sub1 '(com-x) subs-menu)
        (test-look :x sub2 nil nil)
        ;; Check for conflict
        (test-find :y sub1 '(cmd-1) root)
        (test-find :y sub2 '(cmd-1) root)
        (test-find :y sub3 nil nil)
        (test-look :y sub3 '(com-y) sub2-menu)))))

;;; This tests verifies that we don't follow sub-menus that have a keystroke
;;; accelerator. This is to enable emacs-like chains C-x C-t to traverse
;;; command tables. This lookup behavior is consistent with CLIM-TOS.
(test commands.command-table.keystroke.4
  (with-gestures ((:w #\w) (:x #\x) (:y #\y) (:z #\z))
    (with-command-tables ((men1 nil :menu `(("Y" :command (com-y) :keystroke :y)))
                          (men2 nil :menu `(("Z" :command (com-z) :keystroke :z)))
                          (root nil :menu `(("menu1" :menu ,men1  :keystroke :w)
                                            ("menu2" :menu ,men2))))
      (with-keystroke-item-testers ()
        (test-find :w root men1 root)
        (test-look :y root nil nil)
        (test-look :z root '(com-z) men2)))))

;;; This test checks, whether with-command-table-keystrokes correctly
;;; traverses all menus and inherited command-tables.
(test commands.command-table.keystroke.5
  (with-gestures ((:w #\w) (:x #\x) (:y #\y) (:z #\z))
    (with-command-tables ((men1 nil :menu `(("W" :command (com-w) :keystroke :w)))
                          (men2 nil :menu `(("X" :command (com-y) :keystroke :x)))
                          (root nil :menu `(("Y" :menu ,men1  :keystroke :y)
                                            ("M" :menu ,men2)))
                          (sub1 nil :inherit-from (list root) :inherit-menu t
                                    :menu `(("Z" :command '(com-z) :keystroke :z))))
      (with-command-table-keystrokes (keystrokes sub1)
        (is (member :z keystrokes) "Keystrokes are not collected.")
        (is (member :y keystrokes) "Keystrokes are not inherited.")
        (is (not (member :w keystrokes)) "Keystroked-menu is traversed.")
        (is (member :x keystrokes) "Keystrokes in sub-menus are not collected.")))))


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
