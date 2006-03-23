(defpackage :clim-tests
  (:use :clim-lisp :clim))

(in-package :clim-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; command tables
(define-command-table no-menu-test-table)

(add-command-to-command-table '(com-test-command) 'no-menu-test-table
                              :keystroke '(#\t))

(let ((count 0))
  (map-over-command-table-keystrokes
   (lambda (menu-name gesture item)
     (incf count)
     (assert 
      (and (equal menu-name nil)
           (equal gesture '(:keyboard #\t 0))
           (equal (command-menu-item-value item)
                  (lookup-keystroke-command-item gesture 
                                                 'no-menu-test-table)))))
   'no-menu-test-table)
  (assert (= count 1)))

(define-command-table menu-test-table)

(add-command-to-command-table '(com-test-command) 'menu-test-table
                              :keystroke '(#\u)
                              :menu "Test")

(let ((count 0))
  (map-over-command-table-keystrokes
   (lambda (menu-name gesture item)
     (incf count)
     (assert 
      (and (equal menu-name "Test")
           (equal gesture '(:keyboard #\u 0))
           (equal (command-menu-item-value item)
                  (lookup-keystroke-command-item gesture 'menu-test-table)))))
   'menu-test-table)
  (assert (= count 1)))

(define-command-table removal-test-table)
(add-command-to-command-table 'com-test-command 'removal-test-table)
(remove-command-from-command-table 'com-test-command 'removal-test-table)
(assert (handler-case
            (remove-command-from-command-table 'com-test-command
                                               'removal-test-table)
          (command-not-present () t)
          (:no-error (x) (declare (ignore x)) nil)))
