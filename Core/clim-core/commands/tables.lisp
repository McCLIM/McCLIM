;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com)
;;;  (c) copyright 2000 by Robert Strandh <strandh@labri.u-bordeaux.fr>
;;;  (c) copyright 2002 by Tim Moore <moore@bricoworks.com)
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Command tables.
;;;

(in-package #:clim-internals)

(defclass standard-command-table (command-table)
  ((name
    :initarg :name
    :reader command-table-name
    :type symbol)
   (inherit-from
    :initarg :inherit-from
    :initform '()
    :reader command-table-inherit-from
    :type list)
   (commands
    :initarg :commands
    :initform (make-hash-table :test #'eq)
    :accessor commands
    :type hash-table)
   (command-line-names
    :initform (make-hash-table :test #'equal)
    :accessor command-line-names
    :type hash-table)
   (presentation-translators
    :initform (make-instance 'translator-table)
    :reader presentation-translators
    :type translator-table)
   (inherit-menu
    :initarg :inherit-menu
    :initform nil
    :reader inherit-menu
    ;; We interpret :menu to mean "inherit menu items without
    ;; keystrokes" and :keystrokes to mean "inherit menu items
    ;; with keystrokes".
    :type (member nil t :menu :keystrokes))
   (menu
    :initarg :menu
    :initform '()
    :type list)))

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
     (with-slots (inherit-from inherit-menu menu) old-table
       (setq inherit-from ',inherit-from
             inherit-menu ',inherit-menu)
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
  (let ((ht (slot-value (find-command-table command-table) 'commands)))
    (nth-value 1 (gethash command-name ht))))

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
    (or (eq inherit-menu t)
        (eq inherit-menu :menu))))

(defun %add-menu-item (command-table item after)
  (with-slots (menu) command-table
    (when (null menu)
      (setf after :start))
    (case after
      (:start
       (push item menu))
      ((:end nil)
       (setf menu (nconc menu (list item))))
      (:sort
       (flet ((menu-name-lessp (x y)
                (cond ((null x) t)
                      ((null y) nil)
                      (t (string-lessp x y)))))
         (setf menu (sort (cons item menu) #'menu-name-lessp
                          :key #'command-menu-item-name))))
      (t
       (check-type after string)
       (push item
             (cdr (member after menu
                          :key #'command-menu-item-name
                          :test #'equal)))))))

(defun partial-command-from-name
    (command-name command-table &optional (errorp t))
  (if-let ((parser (gethash command-name *command-parser-table*)))
    (cons command-name
          (mapcar #'(lambda (foo)
                      (declare (ignore foo))
                      *unsupplied-argument-marker*)
                  (required-args parser)))
    (when errorp
      (error 'command-not-present :command-table-name
             (command-table-designator-as-name command-table)))))


;;; Command table item accessors.

(defun map-over-command-table-commands
    (function command-table &key (inherited t))
  (let ((command-table (find-command-table command-table)))
    (flet ((map-func (table)
             (alexandria:maphash-keys function (slot-value table 'commands))))
      (if inherited
          (apply-with-command-table-inheritance #'map-func command-table)
          (map-func command-table)))))

(defun add-command-to-command-table
    (command-name command-table &key name menu keystroke (errorp t))
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
      (let ((item (if (or menu keystroke)
                      (apply #'make-menu-item
                             menu-name :command command-name
                                       :command-name command-name
                                       :command-line-name name
                                       `(,@(when keystroke `(:keystroke ,keystroke))
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
        (when (or menu keystroke)
          (%add-menu-item table item after))))))

(defun remove-command-from-command-table
    (command-name command-table &key (errorp t))
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

;;; This internal function is like map-over-command-menu-items, but it maps
;;; over each element in the command table and (if inherited is t) its
;;; ancestors. It does not descend into sub-menus. The mapped function accepts
;;; two arguments: the item and the command table the item belongs to.
(defun map-over-command-table-menu (function table &key (inherited t))
  (setf table (find-command-table table))
  (mapc #'(lambda (item)
            (funcall function item table))
        (slot-value table 'menu))
  (when inherited
    (dolist (sub-table (command-table-inherit-from table))
      (map-over-command-table-menu function sub-table))))

;;; This internal function is like find-menu-item, but it allows looking for
;;; entries by a command name, a menu name or a keystroke. Moreover it allows
;;; specifying whether it should look in inherited menus.
(defun find-menu-item*
    (item command-table &key (inherited t) (errorp t) key test)
  (map-over-command-table-menu
   #'(lambda (menu-item table)
       (when (funcall test item (funcall key menu-item))
         (return-from find-menu-item*
           (values menu-item table))))
   command-table :inherited inherited)
  (if errorp
      (error 'command-not-present
             :command-table-name (command-table-designator-as-name command-table))
      (values nil nil)))

(defun map-over-command-table-menu-items (function table &key (inherited t))
  "Applies function to all of the items in `table's menu. `table' must be a
command table or the name of a command table. `Function' must be a function of
three arguments, the menu name (which will be NIL if there is none), the
keystroke accelerator gesture (which will be NIL if there is none), and the
command menu item; it has dynamic extent. The command menu items are mapped
over in the order specified by `add-menu-item-to-command-table'. `table' is a
command table designator. Any inherited menu items will be mapped over after
`command-table's own menu items.

`Map-over-command-table-menu-items' does not descend into sub-menus. If the
programmer requires this behavior, they should examine the type of the command
menu item to see if it is `:menu'."
  (flet ((fun (item table)
           (declare (ignore table))
           (with-slots (menu-name keystroke type) item
             (when (or menu-name (eq type :divider))
               (funcall function menu-name keystroke item)))))
    (map-over-command-table-menu
     #'fun table :inherited (and inherited (inherit-menu-items table))))
  (values))

(defun find-menu-item (menu-name command-table &key (errorp t))
  (check-type menu-name string)
  (find-menu-item* menu-name command-table
                   :inherited (inherit-menu-items command-table)
                   :errorp errorp
                   :key #'command-menu-item-name
                   :test #'equalp))

(defun add-menu-item-to-command-table (command-table
                                       string type value
                                       &rest args
                                       &key documentation (after :end)
                                         keystroke text-style (errorp t))
  "Adds menu item to the command table."
  (declare (ignore documentation keystroke text-style))
  (let* ((table (find-command-table command-table))
         (old-item (and string (find-menu-item string table :errorp nil))))
    (cond ((and errorp old-item)
           (error 'command-already-present :command-table-name
                  (command-table-designator-as-name table)))
          (old-item
           (remove-menu-item-from-command-table command-table string))
          (t nil))
    (let ((item (apply #'make-menu-item string type value args)))
      (%add-menu-item table item after))))

(defun remove-menu-item-from-command-table (table string
                                            &key (errorp t))
  "Removes item from the `command-table'."
  (setf table (find-command-table table))
  (with-slots (menu) table
    (if-let ((item (find-menu-item string table :errorp nil)))
      ;; When a keystroke is still present leave the item be.
      (if (null (command-menu-item-keystroke item))
          (setf menu (delete string menu
                             :key #'command-menu-item-name
                             :test #'equal))
          (setf (command-menu-item-name item) nil))
      (when errorp
        (error 'command-not-present :command-table-name
               (command-table-designator-as-name table))))))

(defun map-over-command-table-keystrokes (function table &key (inherited t))
  (flet ((fun (item table)
           (declare (ignore table))
           (with-slots (menu-name keystroke type) item
             (when keystroke
               (funcall function menu-name keystroke item)))))
    (map-over-command-table-menu
     #'fun table :inherited (and inherited (inherit-keystrokes table))))
  (values))

;;; Unlike map-over-command-table-keystrokes this function descends into
;;; sub-menus to look for keystrokes. The function accepts (item table).
(defun map-over-command-table-keystrokes* (function table &key (inherited t))
  (flet ((map-items (item table)
           (when-let ((keystroke (command-menu-item-keystroke item)))
             (funcall function item table)))
         (map-menus (item table)
           (declare (ignore table))
           (when (and (null (command-menu-item-keystroke item))
                      (eq (command-menu-item-type item) :menu))
             (let ((table (command-menu-item-value item)))
               (map-over-command-table-keystrokes* function table
                                                   :inherited t)))))
    (let ((inherit (and inherited (inherit-keystrokes table))))
      (map-over-command-table-menu #'map-items table :inherited inherit)
      (map-over-command-table-menu #'map-menus table :inherited inherit))
    (values)))

(defun find-keystroke-item
    (gesture command-table &key (test #'event-matches-gesture-name-p) (errorp t))
  (find-menu-item* gesture command-table
                   :inherited (inherit-keystrokes command-table)
                   :errorp errorp
                   :key #'command-menu-item-keystroke
                   :test (lambda (keystroke menu-item)
                           (and menu-item (funcall test keystroke menu-item)))))

(defun lookup-keystroke-item (gesture table
                              &key (test #'event-matches-gesture-name-p))
  (map-over-command-table-keystrokes*
   (lambda (item table)
     (when-let ((keystroke (command-menu-item-keystroke item)))
       (when (funcall test gesture keystroke)
         (return-from lookup-keystroke-item (values item table)))))
   table :inherited t))

(defun add-keystroke-to-command-table (command-table gesture type value
                                       &key documentation (errorp t))
  (add-menu-item-to-command-table command-table nil type value
                                  :documentation documentation
                                  :errorp errorp
                                  :keystroke gesture))

(defun remove-keystroke-from-command-table (command-table gesture
                                            &key (errorp t))
  (let ((command-table (find-command-table command-table)))
    (with-slots (menu) command-table
      (if-let ((item (find gesture menu :key #'command-menu-item-keystroke
                                        :test #'equal)))
        (if (null (command-menu-item-name item))
            (setf menu (delete item menu))
            (setf (command-menu-item-keystroke item) nil))
        (when errorp
          (error 'command-not-present :command-table-name
                 (command-table-designator-as-name command-table)))))))

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
    (when (command-enabled (command-name command) *application-frame*)
      (return-from lookup-keystroke-command-item
        (substitute-numeric-argument-marker
         (if (symbolp command)
             (partial-command-from-name command command-table)
             command)
         numeric-arg))))
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
