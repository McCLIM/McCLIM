;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;  (c) copyright 2000 by Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2014 by Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2004 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
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
    (cond ((and (null options) (listp pane)) ; Single form which is a function call
           `(coerce-pane-name ,pane ',name))
          ((eq pane :application) ; Standard pane denoted by a keyword (i.e `:application')
           `(make-clim-application-pane :name ',name ,@options))
          ((eq pane :interactor)
           `(make-clim-interactor-pane :name ',name ,@options))
          ((eq pane :pointer-documentation)
           `(make-clim-pointer-documentation-pane :name ',name ,@options))
          ((eq pane :command-menu)
           `(make-clim-command-menu-pane :name ',name ,@options))
          (t ; Non-standard pane designator fed to the `make-pane'
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
  `(defmethod generate-panes ((fm frame-manager) (frame ,class-name))
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
         (setf (frame-panes frame)
               (ecase (frame-current-layout frame)
                 ,@(if (or menu-bar pointer-documentation)
                       (mapcar (lambda (layout)
                                 `(,(first layout)
                                   (vertically ()
                                     ,@(cond
                                         ((eq menu-bar t)
                                          `((setf (frame-menu-bar-pane frame)
                                                  (make-menu-bar ',class-name))))
                                         ((consp menu-bar)
                                          `((make-menu-bar
                                             (make-command-table
                                              nil :menu ',menu-bar))))
                                         (menu-bar
                                          `((make-menu-bar ',menu-bar)))
                                         (t nil))
                                     ,@(rest layout)
                                     ,@(when pointer-documentation
                                         '(%pointer-documentation%)))))
                               layouts)
                       layouts)))))
     ;; Update frame-current-panes and the special pane slots.
     (update-frame-pane-lists frame)))

(defun parse-define-application-frame-options (options)
  (let ((infos '(;; CLIM
                 (:pane                  * :conflicts (:panes :layouts))
                 (:panes                 * :conflicts (:pane))
                 (:layouts               * :conflicts (:pane))
                 (:command-table         1)
                 (:command-definer       1)
                 (:menu-bar              ensure-1)
                 (:disabled-commands     *)
                 (:top-level             1)
                 ;; :icon is the CLIM specification but we don't support it
                 (:geometry              *)
                 ;; :resize-frame is mentioned in a spec annotation but we don't support it
                 ;; McCLIM extensions
                 (:pointer-documentation 1)
                 ;; Default initargs
                 (:pretty-name           1)
                 ;; Common Lisp
                 (:default-initargs      *)))
        (all-values '()))
    (labels ((definedp (key)
               (not (eq (getf all-values key 'undefined) 'undefined)))
             (parse-option (key values)
               (when-let ((info (find key infos :key #'first)))
                 (destructuring-bind (name value-count &key conflicts) info
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
                                `((single-pane ,@values))
                                (getf all-values :layouts)
                                `((:default single-pane))))
                         ((eq key :default-initargs)
                          (destructuring-bind
                              (&key ((:pretty-name user-pretty-name) nil pretty-name-p)
                               &allow-other-keys)
                              values
                            (when pretty-name-p
                              (parse-option :pretty-name (list user-pretty-name))))
                          (setf (getf all-values :user-default-initargs)
                                (alexandria:remove-from-plist values :pretty-name)))
                         (t
                          (setf (getf all-values key)
                                (ecase value-count
                                  (1        (first values))
                                  (ensure-1 (alexandria:ensure-car values))
                                  (*        values))))))
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
                            geometry
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
          :command-table     (find-command-table ',(first command-table))
          :disabled-commands ',disabled-commands
          :menu-bar          ',menu-bar
          :current-layout    ',current-layout
          :layouts           ',layouts
          :top-level         (list ',(car top-level) ,@(cdr top-level))
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
