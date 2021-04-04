;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;  (c) copyright 2000 by Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2014 by Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2004 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2019, 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The DEFINE-APPLICATION-FRAME macro and supporting code.
;;;

(in-package #:clim-internals)

(defun coerce-pane-name (pane name)
  (setf (slot-value pane 'name) name)
  pane)

(defun generate-pane-creation-form (name form)
  (destructuring-bind (pane &rest options) form
    (cond
      ;; Single form which is a function call
      ((and (null options) (listp pane))
       `(coerce-pane-name ,pane ',name))
      ;; Standard panes denoted by a keyword
      ((eq pane :application)
       `(make-clim-application-pane :name ',name ,@options :scroll-bars :both))
      ((eq pane :interactor)
       `(make-clim-interactor-pane :name ',name ,@options :scroll-bars :vertical))
      ((eq pane :pointer-documentation)
       `(make-clim-pointer-documentation-pane :name ',name ,@options))
      ((eq pane :command-menu)
       `(make-clim-command-menu-pane :name ',name ,@options))
      ;; Non-standard pane designator passed to the `make-pane'
      (t
       `(make-pane ',pane :name ',name ,@options)))))

;;; FIXME The menu-bar code in the following function is incorrect.  it
;;; needs to be moved to somewhere after the backend, since it depends
;;; on the backend chosen.
;;;
;;; This hack slaps a menu-bar into the start of the application-frame,
;;; in such a way that it is hard to find.
(defun generate-generate-panes-form (class-name menu-bar panes layouts
                                     pointer-documentation)
  (when pointer-documentation
    (setf panes (append panes
                        '((%pointer-documentation%
                           pointer-documentation-pane)))))
  `(defmethod generate-panes ((fm standard-frame-manager) (frame ,class-name))
     (with-look-and-feel-realization (fm frame)
       (unless (frame-panes-for-layout frame)
         (setf (frame-panes-for-layout frame)
               (list
                ,@(loop for (name . form) in panes
                        collect `(cons ',name ,(generate-pane-creation-form
                                                name form))))))
       (let ,(loop for (name . form) in panes
                   collect `(,name (alexandria:assoc-value
                                    (frame-panes-for-layout frame)
                                    ',name :test #'eq)))
         ;; [BTS] added this, but is not sure that this is correct for
         ;; adding a menu-bar transparently, should also only be done
         ;; where the exterior window system does not support menus
         (setf (frame-menu-bar-pane frame) nil)
         (setf (frame-panes frame)
               (ecase (frame-current-layout frame)
                 ,@(if (or menu-bar pointer-documentation)
                       (mapcar (lambda (layout)
                                 `(,(first layout)
                                   (vertically ()
                                     ,@(cond
                                         ((eq menu-bar t)
                                          `((setf (frame-menu-bar-pane frame)
                                                  (make-menu-bar (frame-command-table frame) frame 'hmenu-pane))))
                                         ((consp menu-bar)
                                          `((setf (frame-menu-bar-pane frame)
                                                  (make-menu-bar
                                                   (make-command-table nil :menu ',menu-bar)
                                                   frame 'hmenu-pane))))
                                         (menu-bar
                                          `((setf (frame-menu-bar-pane frame)
                                                  (make-menu-bar ',menu-bar frame 'hmenu-pane))))
                                         (t nil))
                                     ,@(rest layout)
                                     ,@(when pointer-documentation
                                         '(%pointer-documentation%)))))
                               layouts)
                       layouts)))))
     ;; Update frame-current-panes and the special pane slots.
     (update-frame-pane-lists frame)))

(defun geometry-specification-p (thing)
  (and (alexandria:proper-list-p thing)
       (evenp (length thing))
       (loop for (key value) on thing by #'cddr
             always (member key '(:left :top :right :bottom :width :height)))))

(defun parse-define-application-frame-options (options)
  ;; Note that options are always of the form (KEY . ARGUMENTS).
  ;; Depending on KEY, ARGUMENTS is expected to have one of two
  ;; shapes:
  ;; 1) an arbitrary list, denoted by * in the INFOS table. In this
  ;;    case, the "value" of the option is just ARGUMENTS.
  ;; 2) a singleton list (that is the option is of the form (KEY
  ;;    VALUE)), denoted by 1 in the INFOS table. In this case the
  ;;    "value" of the option is (first arguments). :TYPE in the INFOS
  ;;    table is the expected type of this value, not of ARGUMENTS.
  (let ((infos '(;; CLIM
                 (:pane                  * :conflicts (:panes :layouts))
                 (:panes                 * :conflicts (:pane))
                 (:layouts               * :conflicts (:pane))
                 (:command-table         1 :type (cons symbol list))
                 (:command-definer       1 :type symbol)
                 (:menu-bar              1 :type (or symbol list))
                 (:disabled-commands     *)
                 (:top-level             1 :type (cons (or symbol cons) list))
                 (:icon                  1)
                 (:geometry              * :type (satisfies geometry-specification-p))
                 (:resize-frame          1)
                 ;; McCLIM extensions
                 (:pointer-documentation 1)
                 ;; Default initargs
                 (:pretty-name           1)
                 ;; Common Lisp
                 (:default-initargs      *)))
        (all-values '()))
    (labels ((definedp (key)
               (not (eq (getf all-values key 'undefined) 'undefined)))
             (maybe-check-type (key arguments expected-type expected-argument-count)
               (flet ((check-value (value)
                        (when (and expected-type (not (typep value expected-type)))
                          (error "~@<The argument ~S for option ~S is ~
                                  not of type ~A.~@:>"
                                 value key expected-type))
                        value))
                 (ecase expected-argument-count
                   (1
                    (unless (typep arguments '(cons t null))
                      (error "~@<The option ~S takes a single ~
                              argument (not ~@[~{~S~^ ~} which are ~]~
                              ~R).~@:>"
                             key arguments (length arguments)))
                    (check-value (first arguments)))
                   (*
                    (check-value arguments)))))
             (parse-option (key arguments)
               (when-let ((info (find key infos :key #'first)))
                 (destructuring-bind (name value-count &key conflicts type) info
                   (declare (ignore name))
                   (cond ((when-let ((other (find-if #'definedp conflicts)))
                            (error "~@<The options ~S and ~S are mutually ~
                                    exclusive.~@:>"
                                   key other)))
                         ((definedp key)
                          (error "~@<The option ~S cannot be supplied ~
                                  multiple times.~@:>"
                                 key))
                         ;; Canonicalize :pane, :panes and :layouts to
                         ;; just :panes and :layouts.
                         ((eq key :pane)
                          (setf (getf all-values :pane)
                                t
                                (getf all-values :panes)
                                `((single-pane ,@arguments))
                                (getf all-values :layouts)
                                `((:default single-pane))))
                         ((eq key :default-initargs)
                          (destructuring-bind
                              (&key ((:pretty-name user-pretty-name) nil pretty-name-p)
                               &allow-other-keys)
                              arguments
                            (when pretty-name-p
                              (parse-option :pretty-name (list user-pretty-name))))
                          (setf (getf all-values :user-default-initargs)
                                (alexandria:remove-from-plist arguments :pretty-name)))
                         (t
                          (setf (getf all-values key)
                                (maybe-check-type key arguments type value-count)))))
                 t)))
      (loop :for option :in options
            :for (key . values) = option
            :do (with-current-source-form (option)
                  (when (not (parse-option key values))
                    (push option (getf all-values :other-options)))))
      (alexandria:remove-from-plist all-values :pane))))

(defmacro define-application-frame (name superclasses slots &rest options)
  (when (null superclasses)
    (setq superclasses '(standard-application-frame)))
  (destructuring-bind (&key panes
                            layouts
                            (command-table (list name))
                            (command-definer t)
                            (menu-bar t)
                            disabled-commands
                            (top-level '(default-frame-top-level))
                            (icon nil icon-supplied-p)
                            geometry
                            resize-frame
                            ;; McCLIM extensions
                            pointer-documentation
                            ;; Default initargs
                            (pretty-name (string-capitalize name))
                            ;; Common Lisp
                            user-default-initargs
                            other-options
                            ;; Helpers
                            (current-layout (first (first layouts)))
                            (frame-arg (gensym "FRAME-ARG")))
      (parse-define-application-frame-options options)
    (when (eq command-definer t)
      (setf command-definer
            (alexandria:symbolicate '#:define- name '#:-command)))
    `(progn
       (defclass ,name ,superclasses
         ,slots
         (:default-initargs
          :name              ',name
          :pretty-name       ,pretty-name
          ,@(when icon-supplied-p `(:icon ,icon))
          :command-table     (find-command-table ',(first command-table))
          :disabled-commands ',disabled-commands
          :menu-bar          ',menu-bar
          :current-layout    ',current-layout
          :layouts           ',layouts
          :resize-frame      ',resize-frame
          :top-level-lambda  (lambda (,frame-arg)
                               (,(car top-level) ,frame-arg
                                ,@(cdr top-level)))
          ,@geometry
          ,@user-default-initargs)
         ,@other-options)

       ,@(when (or panes layouts)
           `(,(generate-generate-panes-form
               name menu-bar panes layouts pointer-documentation)))

       ,@(when command-table
           `((define-command-table ,@command-table)))

       ,@(when command-definer
           `((defmacro ,command-definer (name-and-options arguments &rest body)
               (destructuring-bind (name &rest options)
                   (alexandria:ensure-list name-and-options)
                 `(define-command (,name :command-table ,',(first command-table)
                                         ,@options)
                      ,arguments ,@body))))))))
