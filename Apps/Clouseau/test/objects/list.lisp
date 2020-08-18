;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019-2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Smoke test for inspecting lists.
;;;

(cl:in-package #:clouseau.test)

(def-suite* :clouseau.objects.list
  :in :clouseau)

(test list.object-state-class.smoke
  "Test `object-state-class' for list-related objects."

  (object-state-class-cases
   '(()                      inspected-object)

   ;; Small lists
   '((1 . 2)                 clouseau:inspected-improper-list)
   '((1 2)                   clouseau:inspected-proper-list)
   '((1 2 . 3)               clouseau:inspected-improper-list)
   '((1 2 3)                 clouseau:inspected-proper-list)

   ;; Plist
   '((:foo 1)                clouseau:inspected-plist)
   '((:foo 1 :bar 2)         clouseau:inspected-plist)

   ;; Alist
   '(((:foo . 1))            clouseau:inspected-proper-list)
   '(((:foo . 1) (:bar . 2)) clouseau:inspected-alist)))
