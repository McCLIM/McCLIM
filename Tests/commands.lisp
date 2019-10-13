(cl:in-package #:clim-tests)

(def-suite* :mcclim.commands
  :in :mcclim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; command tables
(define-command-table no-menu-test-table)

(add-command-to-command-table '(com-test-command) 'no-menu-test-table
                              :keystroke '(#\t))

(test commands.map-over-command-table-keystrokes.no-menu
  (let ((count 0))
    (map-over-command-table-keystrokes
     (lambda (menu-name gesture item)
       (incf count)
       (is (equal nil menu-name))
       (is (equal '(:keyboard #\t 0) gesture))
       (is (equal (lookup-keystroke-command-item
                   gesture 'no-menu-test-table)
                  (command-menu-item-value item))))
     'no-menu-test-table)
    (is (= 1 count))))

(define-command-table menu-test-table)

(add-command-to-command-table '(com-test-command) 'menu-test-table
                              :keystroke '(#\u)
                              :menu "Test")

(test commands.map-over-command-table-keystrokes.menu
  (let ((count 0))
    (map-over-command-table-keystrokes
     (lambda (menu-name gesture item)
       (incf count)
       (is (equal "Test" menu-name))
       (is (equal '(:keyboard #\u 0) gesture))
       (is (equal (lookup-keystroke-command-item
                   gesture 'menu-test-table)
                  (command-menu-item-value item))))
     'menu-test-table)
    (is (= 1 count))))

;; (define-command-table removal-test-table)
;; (add-command-to-command-table 'com-test-command 'removal-test-table)
;; (remove-command-from-command-table 'com-test-command 'removal-test-table)
;; (signals command-not-present
;;   (remove-command-from-command-table 'com-test-command
;;                                      'removal-test-table))

;;; command table errors (see 27.2)

;; (is (subtypep 'command-table-error 'error))
;; (is (subtypep 'command-table-not-found 'command-table-error))
;; (is (subtypep 'command-table-already-exists 'command-table-error))
;; (is (subtypep 'command-not-present 'command-table-error))
;; (is (subtypep 'command-not-accessible 'command-table-error))
;; (is (subtypep 'command-already-present 'command-table-error))

;; (let ((condition (make-condition 'command-table-error
;;                                  :format-control "~A"
;;                                  :format-arguments '(!))))
;;   (is-true (find #\! (format nil "~A" condition))))

;;; not actually required to DTRT here, but we use this form (without
;;; control and arguments) internally, so make sure that we don't
;;; error out recursively when in the debugger with one of these.

(test commands.command-no-present.print
  (let ((condition (make-condition 'command-not-present)))
    (is-true (stringp (format nil "~A" condition)))))
