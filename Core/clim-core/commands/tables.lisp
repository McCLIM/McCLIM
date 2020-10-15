;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2002 by Tim Moore (moore@bricoworks.com)
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Command tables.
;;;

(in-package #:clim-internals)

(defclass standard-command-table (command-table)
  ((name :initarg :name :reader command-table-name)
   (inherit-from :initarg :inherit-from
                 :initform '()
                 :reader command-table-inherit-from
                 :type list)
   (commands :accessor commands
             :initarg :commands
             :initform (make-hash-table :test #'eq))
   (command-line-names :accessor command-line-names
                       :initform (make-hash-table :test #'equal))
   (presentation-translators :reader presentation-translators
                             :initform (make-instance 'translator-table))
   (inherit-menu :reader inherit-menu
                 :initform nil
                 ;; We interpret :menu to mean "inherit menu items
                 ;; without keystrokes" and :keystrokes to mean
                 ;; "inherit menu items with keystrokes".
                 :type (member nil t :menu :keystrokes)
                 :initarg :inherit-menu)
   (menu :initarg :menu :initform '())
   (keystroke-accelerators :initform nil)
   (keystroke-items :initform nil)))

(defmethod print-object ((table standard-command-table) stream)
  (print-unreadable-object (table stream :identity t :type t)
    (format stream "~S" (command-table-name table))))

;;; We store command-table designators, but this function should
;;; return command table objects.
(defmethod command-table-inherit-from :around
    ((command-table standard-command-table))
  (mapcar #'find-command-table (call-next-method)))

(defmethod (setf command-table-inherit-from)
    (inherit (table standard-command-table))
  (invalidate-translator-caches)
  (setf (slot-value table 'inherit-from) inherit))

(defparameter *command-tables* (make-hash-table :test #'eq))

(define-condition command-table-error (simple-error)
  ((command-table-name :reader error-command-table-name
                       :initform nil
                       :initarg :command-table-name))
  (:report (lambda (object stream)
             (format stream
                     "Command table name: ~s~%"
                     (error-command-table-name object))
             (apply #'format stream
                    (simple-condition-format-control object)
                    (simple-condition-format-arguments object))))
  (:default-initargs :format-control "" :format-arguments nil))

(define-condition command-table-not-found (command-table-error) ()
  (:default-initargs :format-control "Command table not found."))

(define-condition command-table-already-exists (command-table-error) ()
  (:default-initargs :format-control "Command table already exists."))

(define-condition command-not-present (command-table-error) ()
  (:default-initargs :format-control "Command not present."))

(define-condition command-not-accessible (command-table-error) ()
  (:default-initargs :format-control "Command not accessible."))

(define-condition command-already-present (command-table-error) ()
  (:default-initargs :format-control "Command already accessible."))

(defun command-table-designator-as-name (designator)
  "Return the name of `designator' if it is a command table,
`designator' otherwise."
  (if (typep designator 'standard-command-table)
      (command-table-name designator)
      designator))

(defun find-command-table (name &key (errorp t))
  (cond ((command-table-p name) name)
        ((gethash name *command-tables*))
        (errorp (error 'command-table-not-found :command-table-name name))
        (t nil)))

(setf (gethash 'global-command-table *command-tables*)
      (make-instance 'standard-command-table
                     :name 'global-command-table
                     :inherit-from nil
                     :menu nil))

;;; adjusted to allow anonymous command-tables for menu-bars
(defun make-command-table (name &key inherit-from menu inherit-menu (errorp t))
  (if (and name errorp (gethash name *command-tables*))
      (error 'command-table-already-exists :command-table-name name)
      (let ((result (make-instance 'standard-command-table
                                   :name name
                                   :inherit-from inherit-from
                                   :inherit-menu inherit-menu
                                   :menu (menu-items-from-list menu))))
        (when name
          (setf (gethash name *command-tables*) result))
        result)))

(make-command-table 'user-command-table)

(defmacro define-command-table (name &key (inherit-from '(global-command-table))
                                          (menu nil menu-supplied-p)
                                          inherit-menu)
  `(if-let ((old-table (gethash ',name *command-tables* nil)))
     (with-slots (inherit-from menu) old-table
       (setq inherit-from ',inherit-from)
       ,(when menu-supplied-p
          `(setq menu (menu-items-from-list ',menu)))
       old-table)
     (make-command-table ',name
                         :inherit-from ',inherit-from
                         :inherit-menu ,inherit-menu
                         :menu ',menu
                         :errorp nil)))

(defun apply-with-command-table-inheritance (fun command-table)
  (funcall fun command-table)
  (map nil
       (lambda (inherited-command-table)
         (apply-with-command-table-inheritance
          fun (find-command-table inherited-command-table)))
       (command-table-inherit-from command-table)))

;;; do-command-table-inheritance has been shipped off to utils.lisp.

(defun map-over-command-table-names (function command-table &key (inherited t))
  (let ((command-table (find-command-table command-table)))
    (flet ((map-func (table)
             (maphash function (slot-value table 'command-line-names))))
      (if inherited
          (apply-with-command-table-inheritance #'map-func command-table)
          (map-func command-table)))))

(defun command-present-in-command-table-p (command-name command-table)
  (let ((table (find-command-table command-table)))
    (if (gethash command-name (slot-value table 'commands))
        table
        nil)))

(defun command-accessible-in-command-table-p (command-name command-table)
  (or (command-present-in-command-table-p command-name command-table)
      (some #'(lambda (table)
                (command-accessible-in-command-table-p
                 command-name
                 (find-command-table table)))
            (command-table-inherit-from (find-command-table command-table)))))

(defun find-command-from-command-line-name (name command-table &key (errorp t))
  (apply-with-command-table-inheritance
   #'(lambda (table)
       (let ((value (gethash name (command-line-names table))))
         (when value
           (return-from find-command-from-command-line-name
             (values value table)))))
   (find-command-table command-table))
  (when errorp
    (error 'command-not-accessible :command-table-name command-table)))

(defun command-line-name-for-command (command-name command-table
                                      &key (errorp t))
  (do-command-table-inheritance (table command-table)
    (let* ((command-item (gethash command-name (slot-value table 'commands)))
           (command-line-name (and command-item
                                   (command-line-name command-item))))
      (when (stringp command-line-name)
        (return-from command-line-name-for-command command-line-name))))
  (cond ((eq errorp :create)
         (command-name-from-symbol command-name))
        (errorp
         (error 'command-not-accessible :command-table-name
                (command-table-designator-as-name command-table)))
        (t nil)))

(defvar *numeric-argument-marker* '%numeric-argument-marker%)

(defun substitute-numeric-argument-marker (command numeric-arg)
  (substitute numeric-arg *numeric-argument-marker* command))

;;; Note that command table inheritance is the opposite of Common Lisp
;;; subclassing / subtyping: the inheriting table defines a superset
;;; of the commands of its ancestor, so therefore it's command
;;; presentation type is a supertype of its ancestor's!
(defun command-table-inherits-from-p (command-table super-table)
  (let ((command-table (find-command-table command-table))
        (super-table (find-command-table super-table)))
    (do-command-table-inheritance (table command-table)
      (when (eq table super-table)
        (return-from command-table-inherits-from-p (values t t))))
    (values nil t)))

(defun inherit-keystrokes (command-table)
  "Return true if `command-table' (which must be a command table
designator) inherits keystrokes."
  (let ((inherit-menu (inherit-menu (find-command-table command-table))))
    (or (eq inherit-menu t)
        (eq inherit-menu :keystrokes))))

(defun inherit-menu-items (command-table)
  "Return true if `command-table' (which must be a command table
designator) inherits menu items."
  (let ((inherit-menu (inherit-menu (find-command-table command-table))))
    (or (inherit-keystrokes command-table)
        (eq inherit-menu :menu))))

(defun %add-menu-item (command-table item after)
  (with-slots (menu)
      command-table
    (when (null menu)
      (setf after :start))
    (case after
      (:start (push item menu))
      ((:end nil) (setf menu (nconc menu (list item))))
      (:sort (setf menu (sort (cons item menu)
                              #'string-lessp
                              :key #'command-menu-item-name)))
      (t (push item
               (cdr (member after menu
                            :key #'command-menu-item-name
                            :test #'string-equal))))))
  (when-let ((keystroke (slot-value item 'keystroke)))
    (%add-keystroke-item command-table keystroke item nil)))

;; At this point we should still see the gesture name as supplied by the
;; programmer in 'gesture'
(defun %add-keystroke-item (command-table gesture item errorp)
  (with-slots (keystroke-accelerators keystroke-items)
      command-table
    (let* ((gesture (if (and (symbolp gesture) ; symbolic gesture name?
                             (gethash gesture *gesture-names*))
                        gesture
                        (multiple-value-list (realize-gesture-spec :keyboard gesture))))
           (in-table (position gesture keystroke-accelerators :test #'equal)))
      (when (and in-table errorp)
        (error 'command-already-present :command-table-name
               (command-table-designator-as-name command-table)))
      (if in-table
          (setf (nth in-table keystroke-items) item)
          (progn
            (push gesture keystroke-accelerators)
            (push item keystroke-items))))))

(defun partial-command-from-name (command-name command-table)
  (let ((parser (gethash command-name *command-parser-table*)))
    (if (null parser)
        (error 'command-not-present :command-table-name
               (command-table-designator-as-name command-table))
        (cons command-name
              (mapcar #'(lambda (foo)
                          (declare (ignore foo))
                          *unsupplied-argument-marker*)
                      (required-args parser))))))


;;; Command table item accessors.

(defun map-over-command-table-commands
    (function command-table &key (inherited t))
  (let ((command-table (find-command-table command-table)))
    (flet ((map-func (table)
             (maphash #'(lambda (key val)
                          (declare (ignore val))
                          (funcall function key))
                      (slot-value table 'commands))))
      (if inherited
          (apply-with-command-table-inheritance #'map-func command-table)
          (map-func command-table)))))

(defun add-command-to-command-table (command-name
                                     command-table
                                     &key name menu keystroke (errorp t)
                                     (menu-command (and menu `(,command-name))))
  (let ((table (find-command-table command-table))
        (name (cond ((stringp name)
                     name)
                    (name
                     (command-name-from-symbol command-name))
                    (t nil))))
    (multiple-value-bind (menu-name menu-options)
        (cond ((null menu)
               nil)
              ((stringp menu)
               menu)
              ((eq menu t)
               (if (stringp name)
                   name
                   (command-name-from-symbol command-name)))
              ((consp menu)
               (values (car menu) (cdr menu))))
      (when keystroke
        (add-keystroke-to-command-table table keystroke
                                        :command command-name :errorp nil))
      (let* ((item (if menu
                       (apply #'make-menu-item
                              menu-name :command menu-command
                              :command-name command-name
                              :command-line-name name
                              `(,@(and keystroke `(:keystroke ,keystroke))
                                ,@menu-options))
                       (make-instance 'command-item
                                      :command-name command-name
                                      :command-line-name name)))
             (after (getf menu-options :after)))
        (when (and errorp (gethash command-name (commands table)))
          (error 'command-already-present :command-table-name command-table))
        (remove-command-from-command-table command-name table :errorp nil)
        (setf (gethash command-name (commands table)) item)
        (when name
          (setf (gethash name (command-line-names table)) command-name))
        (when menu
          (%add-menu-item table item after))))))

(defun remove-command-from-command-table (command-name
                                          command-table
                                          &key (errorp t))
  (let* ((table (find-command-table command-table))
         (commands (commands table))
         (item (gethash command-name commands)))
    (if (null item)
        (when errorp
          (error 'command-not-present :command-table-name (command-table-name command-table)))
        (progn
          (when (typep item '%menu-item)
            ;; Remove the keystroke and/or the menu entry.
            (setf (slot-value table 'menu)
                  (delete command-name
                          (slot-value table 'menu)
                          :key #'command-item-name)))
          (when (command-item-name item)
            (remhash (command-item-name item) (command-line-names table)))
          (remhash command-name commands)))))

(defun map-over-command-table-menu-items
    (function command-table &key (inherited t))
  "Applies function to all of the items in `command-table's
menu. `Command-table' must be a command table or the name of a
command table. `Function' must be a function of three arguments,
the menu name, the keystroke accelerator gesture (which will be
NIL if there is none), and the command menu item; it has dynamic
extent. The command menu items are mapped over in the order
specified by `add-menu-item-to-command-table'. `Command-table' is
a command table designator. Any inherited menu items will be
mapped over after `command-table's own menu items.

`Map-over-command-table-menu-items' does not descend into
sub-menus. If the programmer requires this behavior, he should
examine the type of the command menu item to see if it is
`:menu'."
  (let ((table-object (find-command-table command-table)))
    (flet ((map-table-entries (table)
             (mapc #'(lambda (item)
                       (with-slots (menu-name keystroke) item
                         (funcall function
                                  menu-name
                                  keystroke
                                  item)))
                   (slot-value table 'menu))))
      (map-table-entries table-object)
      (when (and inherited (inherit-menu-items table-object))
        (dolist (table (command-table-inherit-from table-object))
          (map-over-command-table-menu-items function table))))
    (values)))

(defun find-menu-item (menu-name command-table &key (errorp t))
  (let* ((table (find-command-table command-table))
         (mem (member menu-name (slot-value table 'menu)
               :key #'command-menu-item-name :test #'string-equal)))
    (if mem
        (values (car mem) command-table)
        (or (find-if #'(lambda (table)
                         (find-menu-item menu-name table :errorp nil))
                     (command-table-inherit-from table))
            (when errorp
              (error 'command-not-accessible :command-table-name
                     (command-table-designator-as-name table)))))))

(defun add-menu-item-to-command-table (command-table
                                       string type value
                                       &rest args
                                       &key documentation (after :end)
                                         keystroke text-style (errorp t))
  "Adds menu item to the command table."
  (declare (ignore documentation keystroke text-style))
  (let* ((table (find-command-table command-table))
         (old-item (find-menu-item string table :errorp nil)))
    (cond ((and errorp old-item)
           (error 'command-already-present :command-table-name
                  (command-table-designator-as-name table)))
          (old-item
           (remove-menu-item-from-command-table command-table string))
          (t nil))
    (let ((item (apply #'make-menu-item string type value args)))
      (%add-menu-item table item after))))

(defun remove-menu-item-from-command-table (command-table string
                                            &key (errorp t))
  "Removes item from the `command-table'."
  (let ((table (find-command-table command-table))
        (item (find-menu-item string command-table :errorp nil)))
    (with-slots (menu) table
      (if (and errorp (not item))
          (error 'command-not-present :command-table-name
                 (command-table-designator-as-name table))
          (setf menu (delete string menu
                             :key #'command-menu-item-name
                             :test #'string-equal))))))

(defun map-over-command-table-keystrokes
    (function command-table &key (inherited t))
  (declare (ignore inherited))
  (let ((command-table (find-command-table command-table)))
    (with-slots (keystroke-accelerators keystroke-items)
        command-table
      (loop for gesture in keystroke-accelerators
            for item in keystroke-items
            do (funcall function
                        (command-menu-item-name item)
                        gesture
                        item)))))

(defun find-keystroke-item (gesture command-table
                            &key (test #'event-matches-gesture-name-p)
                            (errorp t))
  (let ((command-table (find-command-table command-table)))
    (loop for keystroke in (slot-value command-table 'keystroke-accelerators)
          for item in (slot-value command-table 'keystroke-items)
          if (funcall test gesture keystroke)
          do (return-from find-keystroke-item (values item command-table)))
    (if errorp
        (error 'command-not-present :command-table-name
               (command-table-designator-as-name command-table))
        nil)))

;; FIXME: According to the spec, we need to remove the menu item if already
;; present. Also, you could argue we don't signal 'command-already-present
;; in quite the right circumstance (see above).
(defun add-keystroke-to-command-table (command-table gesture type value
                                       &key documentation (errorp t))
  (let ((command-table (find-command-table command-table)))
    (%add-keystroke-item command-table
                         gesture
                         (make-menu-item nil type value
                                         :keystroke gesture
                                         :documentation documentation)
                         errorp)))

(defun remove-keystroke-from-command-table (command-table gesture
                                            &key (errorp t))
  (let ((command-table (find-command-table command-table)))
    (with-slots (keystroke-accelerators keystroke-items)
        command-table
      (let ((in-table (position gesture keystroke-accelerators :test #'equal)))
        (if in-table
            (if (zerop in-table)
                (setq keystroke-accelerators (cdr keystroke-accelerators)
                      keystroke-items (cdr keystroke-items))
                (let ((accel-tail (nthcdr (1- in-table)
                                          keystroke-accelerators))
                      (items-tail (nthcdr (1- in-table) keystroke-items)))
                  (setf (cdr accel-tail) (cddr accel-tail))
                  (setf (cdr items-tail) (cddr items-tail))))
            (when errorp
              (error 'command-not-present :command-table-name
                     (command-table-designator-as-name command-table)))))))
  nil)

(defun lookup-keystroke-item (gesture command-table
                              &key (test #'event-matches-gesture-name-p))
  (let ((command-table (find-command-table command-table)))
    (multiple-value-bind (item table)
        (find-keystroke-item gesture command-table :test test :errorp nil)
      (when table
        (return-from lookup-keystroke-item (values item table)))
      (map-over-command-table-menu-items
       #'(lambda (name keystroke item)
           (declare (ignore name keystroke))
           (when (eq (command-menu-item-type item) :menu)
             (multiple-value-bind (sub-item sub-command-table)
                 (lookup-keystroke-item gesture
                                        (command-menu-item-value item)
                                        :test test)
               (when sub-command-table
                 (return-from lookup-keystroke-item
                   (values sub-item sub-command-table))))))
       command-table))))

;;; XXX The spec says that GESTURE may be a gesture name, but also that the
;;; default test is event-matches-gesture-name-p.  Uh...

(defun lookup-keystroke-command-item (gesture command-table
                                      &key test (numeric-arg 1))
  (when-let* ((item (lookup-keystroke-item
                     gesture command-table
                     :test (or test
                               #'(lambda (gesture gesture-name)
                                   (or (equal gesture gesture-name)
                                       (event-matches-gesture-name-p
                                        gesture
                                        gesture-name))))))
              (command (case (command-menu-item-type item)
                         (:command
                          (command-menu-item-value item))
                         (:function
                          (funcall (command-menu-item-value item)
                                   gesture numeric-arg))
                         ;; XXX What about the :menu case?
                         (otherwise nil))))
    ;; Return a literal command, or create a partial command from a
    ;; command-name.
    (return-from lookup-keystroke-command-item
      (substitute-numeric-argument-marker
       (if (symbolp command)
           (partial-command-from-name command command-table)
           command)
       numeric-arg)))
  gesture)

(defun map-over-command-table-translators
    (function command-table &key (inherited t))
  (flet ((map-func (table)
           (alexandria:maphash-values
            (lambda (translator)
              (funcall function translator))
            (slot-value (presentation-translators table) 'translators))))
    (let ((command-table (find-command-table command-table)))
      (if inherited
          (apply-with-command-table-inheritance #'map-func command-table)
          (map-func command-table)))))

(defun find-presentation-translator
    (translator-name command-table &key (errorp t))
  (let* ((table (find-command-table command-table))
         (translators (presentation-translators table))
         (translator (gethash translator-name
                              (slot-value translators 'translators))))
    (when (and errorp (null translator))
      (error 'command-not-present :command-table-name command-table))
    translator))

(defun add-presentation-translator-to-command-table
    (command-table translator &key (errorp t))
  (let ((translators (presentation-translators
                      (find-command-table command-table))))
    (when (and errorp
               (nth-value
                1 (gethash (name translator)
                           (slot-value translators 'translators))))
      (error 'command-already-present :command-table-name command-table))
    (add-translator translators translator)))

(defun remove-presentation-translator-from-command-table
    (command-table translator-name &key (errorp t))
  (let* ((translators (presentation-translators
                       (find-command-table command-table)))
         (translator (gethash translator-name
                              (slot-value translators 'translators))))

    (cond ((not (null translator))
           (remove-translator translators translator))
          (errorp
           (error 'command-not-present :command-table-name command-table)))))
