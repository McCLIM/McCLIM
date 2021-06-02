;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;  (c) copyright 2000 by Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000,2014 by Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2004 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2019,2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;  (c) copyright 2016-2021 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The DEFINE-APPLICATION-FRAME macro and supporting code.
;;;

(in-package #:clim-internals)

(defun coerce-pane-name (pane name)
  (setf (slot-value pane 'name) name)
  pane)

(defun try-reinitialize-pane (pane type &rest initargs)
  (when-let* ((pane-class (find-concrete-pane-class *pane-realizer* type))
              (inner-pane (find-pane-of-type pane pane-class)))
    (unless (eq inner-pane pane)
      (when-let ((parent (sheet-parent inner-pane)))
        (sheet-disown-child parent inner-pane)))
    (if (member type '(:application :interactor :pointer-documentation :command-menu))
        (multiple-value-bind (stream-options wrapper-options wrapper-space)
            (separate-stream-pane-initargs
             (if (intersection initargs '(:scroll-bar :scroll-bars))
                 initargs
                 (list* :scroll-bars (ecase type
                                       (:interactor :vertical)
                                       (:application t)
                                       (:pointer-documentation nil)
                                       (:command-menu t))
                        initargs)))
          (apply #'reinitialize-instance inner-pane stream-options)
          (apply #'wrap-stream-pane inner-pane wrapper-space wrapper-options))
        (apply #'reinitialize-instance inner-pane initargs))))

(defun generate-make-pane (name type options)
  (cond
    ;; Single form which is a function call
    ((and (null options) (listp type))
     `(coerce-pane-name ,type ',name))
    ;; Standard panes denoted by a keyword
    ((eq type :application)
     `(make-clim-application-pane :name ',name ,@options))
    ((eq type :interactor)
     `(make-clim-interactor-pane :name ',name ,@options))
    ((eq type :pointer-documentation)
     `(make-clim-pointer-documentation-pane :name ',name ,@options))
    ((eq type :command-menu)
     `(make-clim-command-menu-pane :name ',name ,@options))
    ;; Non-standard pane designator passed to the `make-pane'
    (t
     `(make-pane ',type :name ',name ,@options))))

(defun generate-ensure-pane (spec reinitialize-panes)
  (with-current-source-form (spec)
    (destructuring-bind (name type &rest options) spec
      (unless (symbolp name)
        (error "~@<~S is not a valid pane name. It must be a symbol.~@:>" name))
      (if (and reinitialize-panes (symbolp type))
          (with-gensyms (pane)
            `(let ((,pane (assoc-value ,reinitialize-panes ',name :test #'eq)))
               (or (and ,pane (try-reinitialize-pane ,pane ',type ,@options))
                   ,(generate-make-pane name type options))))
          (generate-make-pane name type options)))))

(defun generate-panes-for-layout-form (panes reinitialize)
  `(list
    ,@(loop for spec in panes
            for (name . form) = spec
            collect `(cons ',name ,(generate-ensure-pane spec reinitialize)))))

(defun generate-panes-constructor (panes)
  `(lambda (fm frame)
     (or (frame-panes-for-layout frame)
         (setf (frame-panes-for-layout frame)
               (with-look-and-feel-realization (fm frame)
                 ,(generate-panes-for-layout-form panes nil))))))

(defun generate-layout-constructor (panes layouts)
  `(lambda (fm frame)
     (disown-frame-panes fm frame)
     (let ((named-panes (frame-panes-for-layout frame)))
       (let ,(loop for (name . form) in panes
                   collect `(,name (assoc-value named-panes ',name :test #'eq)))
         (setf (frame-panes frame)
               (ecase (frame-current-layout frame)
                 ,@layouts))))
     (adopt-frame-panes fm frame (frame-current-layout frame))
     ;; Update frame-current-panes and the special pane slots.
     (update-frame-pane-lists frame)))

(defun generate-reinitialize-instance (class-name panes layouts)
  (with-gensyms (old-panes)
    `(defmethod reinitialize-instance :after ((frame ,class-name) &rest args)
       (declare (ignore args))
       (setf (frame-panes-constructor frame)
             ,(generate-panes-constructor panes)
             (frame-layout-constructor frame)
             ,(generate-layout-constructor panes layouts))
       (let ((all-layouts ',layouts))
         (setf (slot-value frame 'layouts) all-layouts)
         (unless (member (frame-current-layout frame) all-layouts
                         :key #'car :test #'eq)
           (setf (slot-value frame 'current-layout) (car (first all-layouts)))))
       (when-let* ((fm (frame-manager frame))
                   (,old-panes (frame-panes-for-layout frame)))
         (with-bounding-rectangle* (:width width :height height)
             (frame-top-level-sheet frame)
           (changing-space-requirements ()
             (with-look-and-feel-realization (fm frame)
               (setf (frame-panes-for-layout frame)
                     ,(generate-panes-for-layout-form panes old-panes)))
             (funcall (frame-layout-constructor frame) fm frame)
             (layout-frame frame width height)))
         ;; This is to ensure that we start a new iteration of the top-level
         ;; loop with new bindings etc.
         (signal 'frame-layout-changed :frame frame)))))

(defun geometry-specification-p (thing)
  (and (alexandria:proper-list-p thing)
       (evenp (length thing))
       (loop for (key value) on thing by #'cddr
             always (member key '(:left :top :right :bottom :width :height)))))

(defun panes-or-layouts-specification-p (thing)
  (and (alexandria:proper-list-p thing)
       (every (alexandria:of-type '(cons symbol)) thing)))

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
                 (:panes                 * :type (satisfies panes-or-layouts-specification-p)
                                           :conflicts (:pane))
                 (:layouts               * :type (satisfies panes-or-layouts-specification-p)
                                           :conflicts (:pane))
                 (:command-table         1 :type list)
                 (:command-definer       1 :type symbol)
                 (:menu-bar              1 :type (or symbol list))
                 (:disabled-commands     *)
                 (:top-level             1 :type (cons (or symbol cons) list))
                 (:icon                  1)
                 (:geometry              * :type (satisfies geometry-specification-p))
                 (:resize-frame          1)
                 ;; McCLIM extensions
                 (:current-layout        1 :type symbol)
                 (:pointer-documentation 1)
                 (:reinitialize-frames   * :type (or symbol list))
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
             (check-names (key description value)
               (let* ((unique   (remove-duplicates value :key #'first :from-end t))
                      (repeated (set-difference value unique :test #'eq)))
                 (when-let ((first-repeated (first repeated)))
                   (with-current-source-form (first-repeated)
                     (error "~@<The ~A name ~S occurs ~
                             multiple times in the ~S option.~@:>"
                            description (first first-repeated) key)))
                 value))
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
                         ;; Canonicalize `:pane', `:panes' and `:layouts' to
                         ;; just `:panes' and `:layouts'.
                         ((eq key :pane)
                          (setf (getf all-values :pane)
                                t
                                (getf all-values :panes)
                                `((single-pane ,@arguments))
                                (getf all-values :layouts)
                                `((:default single-pane))))
                         ;; Detect duplicate names in `:panes' and `:layouts'.
                         ((member key '(:panes :layouts))
                          (setf (getf all-values key)
                                (check-names key (if (eq key :panes) "pane" "layout")
                                             (maybe-check-type
                                              key arguments type value-count))))
                         ;; Extract `:pretty-name' from `:default-initargs'.
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
                            (command-table (list name) command-table-p)
                            (command-definer t)
                            (menu-bar t menu-bar-p)
                            disabled-commands
                            (top-level '(default-frame-top-level))
                            (icon nil icon-supplied-p)
                            geometry
                            resize-frame
                            ;; McCLIM extensions
                            (pointer-documentation nil pdoc-bar-p)
                            reinitialize-frames
                            ;; Default initargs
                            (pretty-name (string-capitalize name) pretty-name-p)
                            ;; Common Lisp
                            user-default-initargs
                            other-options
                            ;; Helpers
                            (current-layout nil current-layout-p)
                            (frame-arg (gensym "FRAME-ARG")))
      (parse-define-application-frame-options options)
    (when (eq command-definer t)
      (setf command-definer
            (alexandria:symbolicate '#:define- name '#:-command)))
    (unless (or panes layouts)
      (setf panes `((single-pane :interactor))))
    (unless layouts
      (setf layouts `((:default (vertically () ,@(mapcar #'car panes))))))
    (unless current-layout
      (setf current-layout (first (first layouts))))
    `(progn
       (defclass ,name ,superclasses
         ,slots
         (:default-initargs
          :name ',name
          :pretty-name ,pretty-name
          ,@(when icon-supplied-p `(:icon ,icon))
          :command-table (find-command-table ',(first command-table))
          :disabled-commands ',disabled-commands
          :menu-bar ',menu-bar
          :pointer-documentation ',pointer-documentation
          :current-layout ',current-layout
          :layouts ',layouts
          :resize-frame ',resize-frame
          :top-level-lambda (lambda (,frame-arg)
                              (,(car top-level) ,frame-arg
                               ,@(cdr top-level)))
          :panes-constructor ,(generate-panes-constructor panes)
          :layout-constructor ,(generate-layout-constructor panes layouts)
          ,@geometry
          ,@user-default-initargs)
         ,@other-options)

       ,(generate-reinitialize-instance name panes layouts)

       ,@(if (car reinitialize-frames)
             `((defmethod update-instance-for-redefined-class :after
                   ((*application-frame* ,name) as ds pl &rest initargs)
                 (declare (ignore as ds pl initargs))
                 (reinitialize-instance
                  *application-frame*
                  ,@(and (rest reinitialize-frames) reinitialize-frames)
                  ,@(and current-layout-p `(:current-layout ',current-layout))
                  ,@(and pretty-name-p    `(:pretty-name ,pretty-name))
                  ,@(and icon-supplied-p  `(:icon ,icon))
                  ,@(and command-table-p  `(:command-table
                                            (find-command-table
                                             ',(first command-table))))
                  ,@(and menu-bar-p       `(:menu-bar ',menu-bar))
                  ,@(and pdoc-bar-p       `(:pointer-documentation
                                            ,pointer-documentation))))
               (make-instances-obsolete (find-class ',name))
               (map-over-ports (lambda (p)
                                 (map-over-frames #'frame-name :port p))))
             `((defmethod update-instance-for-redefined-class :after
                   ((instance ,name) as ds pl &rest initargs)
                 (declare (ignore instance as ds pl initargs)))))

       ,@(when command-table
           `((define-command-table ,@command-table)))

       ,@(when command-definer
           `((defmacro ,command-definer (name-and-options arguments &rest body)
               (destructuring-bind (name &rest options)
                   (alexandria:ensure-list name-and-options)
                 `(define-command (,name :command-table ,',(first command-table)
                                         ,@options)
                      ,arguments ,@body))))))))
