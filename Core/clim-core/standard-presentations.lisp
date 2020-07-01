;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2001-2002 by Tim Moore <moore@bricoworks.com>
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Standard presentations types.
;;;

(in-package #:clim-internals)

;;; The presentation type for T is the built-in type T. The correspondence is
;;; established by hand in presentations.lisp.
#+ (or) (define-presentation-type t ())
(define-presentation-type nil ())

;;; AUTO-ACTIVATE is described in the Franz user guide; it controls whether an
;;; accepting an expression returns immediately after typing the closing
;;; delimiter -- a la Genera et Mac Lisp -- or if an activation gesture is
;;; required.
;;; PRESERVE-WHITESPACE controls whether the accept method uses read or
;;; READ-PRESERVING-WHITESPACE. This is used in our redefinitions of read and
;;; READ-PRESERVING-WHITESPACE that accept forms.

;;; All the EXPRESSION and FORM reading stuff is in the file
;;; builtin-commands.lisp

(define-presentation-type expression ()
  :options (auto-activate (preserve-whitespace t) (subform-read nil))
  :inherit-from t)

(define-presentation-type form ()
  :options (auto-activate (preserve-whitespace t) (subform-read nil))
  :inherit-from `((expression) :auto-activate ,auto-activate
                  :preserve-whitespace ,preserve-whitespace
                  :subform-read ,subform-read ))

;;; The presentation types

(define-presentation-type null ()
  :inherit-from t)

(define-presentation-method presentation-typep (object (type null))
  (eq object nil))

(define-presentation-method present (object (type null)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore object acceptably for-context-type))
  (write-string "None" stream))

(define-presentation-method accept ((type null) stream (view textual-view)
                                    &key)
  (values (completing-from-suggestions (stream)
            (suggest "None" nil)
            (suggest "" nil))))

(define-presentation-type boolean ()
  :inherit-from t)

(define-presentation-method presentation-typep (object (type boolean))
  (or (eq object t) (eq object nil)))

(define-presentation-method present (object (type boolean) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (if object
      (write-string "Yes" stream)
      (write-string "No" stream)))

(define-presentation-method accept ((type boolean) stream (view textual-view)
                                    &key)
  (accept-using-completion 'boolean
                           stream
                           #'(lambda (input-string mode)
                               (complete-from-possibilities
                                input-string
                                '(("yes" t) ("no" nil))
                                nil
                                :action mode))))

(define-presentation-type symbol ()
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type symbol))
  (symbolp object))

(define-presentation-method present (object (type symbol) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (if acceptably
      (prin1 object stream)
      (princ object stream)))

(define-presentation-method accept ((type symbol) stream (view textual-view)
                                                  &key (default-type type)
                                                  default)
  (let ((read-result (accept-using-read stream type)))
    (if (and (null read-result) default)
        (values default default-type)
        (values read-result type))))

(define-presentation-type keyword () :inherit-from 'symbol)

(define-presentation-method presentation-typep (object (type keyword))
  (keywordp object))

(define-presentation-method present (object (type keyword) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (prin1 object stream))

(define-presentation-type blank-area ()
  :inherit-from t)

;;; Do other slots of this have to be bound in order for this to be
;;; useful?  Guess we'll see.
;;;
;;; KLUDGE: blank-area presentation-typep predicate depends on the
;;; *null-presentation* which is defined to be of type blank-area. The
;;; presentation initialize-instance :after method may call
;;; presentation-typep to ensure, that the presentation object matches
;;; the presentation type. That's why we fix the presentation-type of
;;; the *null-presentation* after creating the object.
(defvar *null-presentation*
  (let ((instance (make-instance 'standard-presentation
                                 :object nil
                                 :type t
                                 :view +textual-view+)))
    (setf (presentation-type instance) 'blank-area)
    instance))

(define-presentation-method presentation-typep (object (type blank-area))
  (eq object (presentation-object *null-presentation*)))

(define-presentation-method highlight-presentation ((type blank-area)
                                                    record
                                                    stream
                                                    state)
  (declare (ignore record stream state))
  nil)



(define-presentation-type number ()
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type number))
  (numberp object))

(define-presentation-type complex (&optional (part-type 'real))
  :inherit-from 'number)

(define-presentation-method presentation-typep (object (type complex))
  (and (complexp object)
       (typep (realpart object) part-type)
       (typep (imagpart object) part-type)))

(define-presentation-method presentation-subtypep ((type complex)
                                                   maybe-supertype)
  (with-presentation-type-parameters (complex type)
    (let ((component-type part-type)) ;i.e., the parameter named "type"
      (with-presentation-type-parameters (complex maybe-supertype)
        (let ((super-component-type part-type))
          (presentation-subtypep component-type super-component-type))))))

(define-presentation-method present (object (type complex) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (present (realpart object) (presentation-type-of (realpart object))
           :stream stream :view view :sensitive nil)
  (write-char #\Space stream)
  (present (imagpart object) (presentation-type-of (imagpart object))
           :stream stream :view view :sensitive nil))

(define-presentation-type real (&optional low high) :options ((base 10) radix)
                          :inherit-from 'number)

(define-presentation-method presentation-typep (object (type real))
  (and (realp object)
       (or (eq low '*)
           (<= low object))
       (or (eq high '*)
           (<= object high))))

(define-presentation-method present (object (type real) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
        (*print-radix* radix))
    (princ object stream)))

(define-presentation-method accept ((type real) stream (view textual-view) &key
                                                (default-type type)
                                                default)
  (let ((*read-base* base)
         (read-result (accept-using-read stream type)))
    (if (and (null read-result) default)
        (values default default-type)
        (values read-result type))))

;;; Define a method that will do the comparision for all real types.  It's
;;; already determined that that the numeric class of type is a subtype of
;;;supertype.

(defun number-subtypep (low high super-low super-high)
  (if (eq low '*)
      (unless (eq super-low '*)
        (return-from number-subtypep nil))
      (unless (or (eq super-low '*) (>= low super-low))
        (return-from number-subtypep nil)))
  (if (eq high '*)
      (unless (eq super-high '*)
        (return-from number-subtypep nil))
      (unless (or (eq super-high '*) (<= high super-high))
        (return-from number-subtypep nil)))
  t)

(define-presentation-type rational (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((real ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type rational))
  (and (rationalp object)
       (or (eq low '*)
           (<= low object))
       (or (eq high '*)
           (<= object high))))

(define-presentation-method present (object (type rational) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
        (*print-radix* radix))
    (princ object stream)))

(define-presentation-type integer (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((rational ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type integer))
  (and (integerp object)
       (or (eq low '*)
           (<= low object))
       (or (eq high '*)
           (<= object high))))

(define-presentation-method present (object (type integer) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
        (*print-radix* radix))
    (princ object stream)))

(define-presentation-type ratio (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((rational ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type ratio))
  (and (not (integerp object))
       (rationalp object)
       (or (eq low '*)
           (<= low object))
       (or (eq high '*)
           (<= object high))))

(define-presentation-method present (object (type ratio) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
        (*print-radix* radix))
    (princ object stream)))

(define-presentation-type float (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((real ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type float))
  (and (floatp object)
       (or (eq low '*)
           (<= low object))
       (or (eq high '*)
           (<= object high))))

(define-presentation-method present (object (type float) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
        (*print-radix* radix))
    (princ object stream)))

(macrolet ((frob (num-type)
             `(define-presentation-method presentation-subtypep ((type
                                                                  ,num-type)
                                                                 maybe-supertype)
                (with-presentation-type-parameters (,num-type maybe-supertype)
                  (let ((super-low low)
                        (super-high high))
                    (with-presentation-type-parameters (,num-type type)
                      (values (number-subtypep low high super-low super-high)
                              t)))))))
  (frob real)
  (frob rational)
  (frob ratio)
  (frob integer)
  (frob float))

(define-presentation-type character ()
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type character))
  (characterp object))

(define-presentation-method present (object (type character) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ object stream))

(define-presentation-type string (&optional length)
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type string))
  (and (stringp object)
       (or (eq length '*) (eql (length object) length))))

(define-presentation-method presentation-subtypep ((type string)
                                                   maybe-supertype)
  (with-presentation-type-parameters (string maybe-supertype)
    (let ((super-length length))
      (with-presentation-type-parameters (string type)
        (values (or (eq super-length '*)
                    (eql length super-length))
                t)))))

(define-presentation-method present (object (type string) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (if acceptably
      (prin1 object stream)
      (princ object stream)))

(define-presentation-method accept ((type string) stream (view textual-view)
                                    &key (default nil defaultp)
                                    (default-type type))
  (let ((result (read-token stream)))
    (cond ((numberp length)
           (if (eql length (length result))
               (values result type)
               (input-not-of-required-type result type)))
          ((and (zerop (length result)) defaultp)
           (values default default-type))
          (t (values result type)))))

(define-presentation-type pathname ()
  :options ((default-version :newest) default-type (merge-default t))
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type pathname))
  (pathnamep object))

(define-presentation-method present ((object pathname) (type pathname)
                                     stream (view textual-view) &key)
  ;; XXX: We can only visually represent the pathname if it has a name
  ;; - making it wild is a compromise. If the pathname is completely
  ;; blank, we leave it as-is, though.

  ;; The above comment was meant to indicate that if the pathname had
  ;; neither a name NOR a directory, then it couldn't be visually
  ;; represented.  Some discussion has ensued on the possbility of
  ;; emitting something like "A pathname of type <foo>"
  ;; [2007/01/08:rpg]
  (let ((pathname (if (equal object #.(make-pathname))
                      object
                      (merge-pathnames object (make-pathname :name :wild)))))
    (declare (ignore pathname))
    (princ object stream)))

(define-presentation-method present ((object string) (type pathname)
                                     stream (view textual-view)
                                     &rest args &key)
  (apply-presentation-generic-function
   present (pathname object) type stream view args))

(defun filename-completer-get-directory (string)
  (flet
      ((deal-with-home (pathname his-directory)
         ;; SBCL (and maybe others) treat "~/xxx" specially, returning a pathname
         ;; whose directory is (:ABSOLUTE :HOME xxx)
         ;; But if you call Directory on that pathname the returned list
         ;; are all complete pathnames without the :Home part!.
         ;; So this replaces the :HOME with what it actually means
         (let* ((home-env-variable (get-environment-variable "HOME"))
                (home (loop for pos = 1 then (1+ next-pos)
                         for next-pos = (position #\/ home-env-variable :start pos)
                         collect (subseq home-env-variable pos next-pos)
                         until (null next-pos)))
                (new-directory (cons
                                (first his-directory)
                                (append home (rest (rest his-directory))))))
           (make-pathname :host (pathname-host pathname)
                          :device (pathname-device pathname)
                          :name (pathname-name pathname)
                          :version (pathname-version pathname)
                          :type (pathname-type pathname)
                          :directory new-directory))))
    (let* ((raw-pathname (pathname string))
           (raw-directory (pathname-directory raw-pathname))
           (original-pathname (if (and (listp raw-directory)
                                       (eql (first raw-directory) :absolute)
                                       (eql (second raw-directory) :Home))
                                  (deal-with-home raw-pathname raw-directory)
                                  raw-pathname))
           ;; Complete logical pathnames as well as regular pathnames
           ;; strategy is to keep track of both original string provided and translated string
           ;; but to return pathname built from original components except for the name.
           (logical-pathname-p (typep original-pathname 'logical-pathname))
           (actual-pathname (if logical-pathname-p
                                (translate-logical-pathname original-pathname)
                                original-pathname))
           ;; merge in *default-pathname-defaults*
           (merged-pathname (merge-pathnames actual-pathname))
           (search-pathname  (make-pathname :host (pathname-host merged-pathname)
                                            :device (pathname-device merged-pathname)
                                            :directory (pathname-directory merged-pathname)
                                            :version :unspecific
                                            :type :wild
                             :name :wild)))
      (values search-pathname (pathname-type actual-pathname) original-pathname))))

(defun filename-completer-get-candidates (search-pathname pathname-type original-pathname)
  (let ((orginal-is-logical-pathname (typep original-pathname 'logical-pathname))
        (completions (directory search-pathname #+sbcl :resolve-symlinks #+sbcl nil)))
    ;; Now prune out all completions that don't start with the string
    (when (null pathname-type)
      (flet ((legitimate-logical-pathname (name)
               (let ((word (string name)))
                 (loop for i below (length word)
                       for ch = (schar word i)
                       always (and (standard-char-p ch)
                                   (or (alphanumericp ch) (char= ch #\-)))))))
        (let ((new-completions nil))
          (loop for pathname in completions
                for pathname-name = (pathname-name pathname)
                for pathname-type = (pathname-type pathname)
                for pathname-directory = (pathname-directory pathname)
                for pathname-host = (pathname-host original-pathname)
                for pathname-device = (pathname-device original-pathname)
                do (cond
                     ;; meaning this is actually a directory
                     ((and (null pathname-name)
                           (null pathname-type))
                      (when (and (loop for word in  (butlast pathname-directory)
                                       always (legitimate-logical-pathname word))
                                 (legitimate-logical-pathname (first (last pathname-directory))))
                        (pushnew (if orginal-is-logical-pathname
                                     (make-pathname :host pathname-host
                                                    :device pathname-device
                                                    :directory (first (last pathname-directory))
                                                    :name nil
                                                    :type nil)
                                     (make-pathname :host pathname-host
                                                    :device pathname-device
                                                    :directory (butlast pathname-directory)
                                                    :name (first (last pathname-directory))
                                                    :type nil))
                                 new-completions)))
                     (t
                      (when (or (not orginal-is-logical-pathname)
                                (and (legitimate-logical-pathname pathname-name)
                                     (legitimate-logical-pathname pathname-type)))
                        (pushnew (make-pathname :host pathname-host
                                                :device pathname-device
                                                :directory (pathname-directory original-pathname)
                                                :name pathname-name
                                                :type pathname-type)
                                 new-completions)))))
          (nreverse new-completions))))))


(defun filename-completer (string action)
  (multiple-value-bind (search-pathname pathname-type original-pathname)
      (filename-completer-get-directory string)
    (let ((candidates (filename-completer-get-candidates search-pathname pathname-type original-pathname)))
        (complete-from-possibilities (namestring original-pathname) candidates '(#\Space)
                                     :action action
                                     :name-key #'namestring
                                     :value-key #'identity))))

(define-presentation-method accept ((type pathname) stream (view textual-view)
                                    &key (default *default-pathname-defaults* defaultp)
                                    ((:default-type accept-default-type) type))
  (multiple-value-bind (pathname success string)
      (complete-input stream
                      #'filename-completer
                      :allow-any-input t)
    (cond ((and pathname success)
           (values (if merge-default
                       (progn
                         (unless (or (pathname-type pathname)
                                     (null default-type))
                           (setf pathname (make-pathname :defaults pathname
                                                         :type default-type)))
                         (merge-pathnames pathname default default-version))
                       pathname)
                   type))
          ((and (zerop (length string))
                defaultp)
           (values default accept-default-type))
          (t (values string 'string)))))

(defmethod presentation-replace-input :around
    ((stream input-editing-stream)
     (object pathname) (type (eql 'pathname))
     view &rest args &key &allow-other-keys)
  ;; This is fully valid and compliant, but it still smells slightly
  ;; like a hack.
  (let ((name (pathname-name object))
        (directory (when (pathname-directory object)
                     (directory-namestring object)))
        (type (pathname-type object))
        (string "")
        (old-insp (stream-insertion-pointer stream)))
    (setf string (or directory string))
    (setf string (concatenate 'string string
                              (cond ((and name type)
                                     (file-namestring object))
                                    (name name)
                                    (type (subseq
                                           (namestring
                                            (make-pathname
                                             :name " "
                                             :type type))
                                           1)))))
    (apply #'replace-input stream string args)
    (when directory
      (setf (stream-insertion-pointer stream)
            (+ old-insp (if directory (length directory) 0)))
      ;; If we moved the insertion pointer, this might be a good idea.
      (redraw-input-buffer stream old-insp))))

(defgeneric default-completion-name-key (item))

(defmethod default-completion-name-key ((item string))
  item)

(defmethod default-completion-name-key ((item null))
  "NIL")

(defmethod default-completion-name-key ((item cons))
  (string (car item)))

(defmethod default-completion-name-key ((item symbol))
  (string-capitalize (symbol-name item)))

(defmethod default-completion-name-key (item)
  (princ-to-string item))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; This function is copied from CLIM Franz code
  (defun highlight-completion-choice (continuation object stream)
    (with-text-face (stream :bold)
      (funcall continuation object stream)))

  (defconstant +completion-options+
    '((name-key 'default-completion-name-key)
      documentation-key
      (partial-completers '(#\Space))
      (printer #'write-token)
      (highlighter #'highlight-completion-choice))))

(define-presentation-type completion (sequence
                                      &key (test 'eql) (value-key 'identity))
  :options #.+completion-options+
  :inherit-from t)

(define-presentation-method presentation-typep (object (type completion))
  (map nil #'(lambda (obj)
               (when (funcall test object (funcall value-key obj))
                 (return-from presentation-typep t)))
       sequence)
  nil)

;;; Useful for subtype comparisons for several of the "member" style types

(defun sequence-subset-p (seq1 test1 value-key1 seq2 test2 value-key2)
  (let ((test-fun (if (eq test1 test2)
                      test1
                      ;; The object has to pass both type's equality test
                      #'(lambda (obj1 obj2)
                          (and (funcall test1 obj1 obj2)
                               (funcall test2 obj1 obj2))))))
    (map nil #'(lambda (type-obj)
                 (unless (find (funcall value-key1 type-obj)
                               seq2
                               :test test-fun :key value-key2)
                   (return-from sequence-subset-p nil)))
         seq1)
    t))

(define-presentation-method presentation-subtypep ((type completion)
                                                   maybe-supertype)
  (with-presentation-type-parameters (completion maybe-supertype)
    (let ((super-sequence sequence)
          (super-test test)
          (super-value-key value-key))
      (with-presentation-type-parameters (completion type)
        (values (sequence-subset-p sequence test value-key
                                   super-sequence super-test super-value-key)
                t)))))

(define-presentation-method present (object (type completion) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((obj-pos (position object sequence :test test :key value-key)))
    (if obj-pos
        (write-string (funcall name-key (elt sequence obj-pos)) stream)
        ;; Should define a condition type here.
        (error "~S is not of presentation type ~S" object type))))

(define-presentation-method accept ((type completion)
                                    stream
                                    (view textual-view)
                                    &key)
  (accept-using-completion (make-presentation-type-specifier
                            `(completion ,@parameters)
                            options)
                           stream
                           #'(lambda (input-string mode)
                               (complete-from-possibilities
                                input-string
                                sequence
                                partial-completers
                                :action mode
                                :name-key name-key
                                :value-key value-key))
                           :partial-completers partial-completers))

(define-presentation-type-abbreviation member (&rest elements)
  (make-presentation-type-specifier `(completion ,elements)
                                    :name-key name-key
                                    :documentation-key documentation-key
                                    :partial-completers partial-completers
                                    :printer printer
                                    :highlighter highlighter)
  :options #.+completion-options+)

(define-presentation-type-abbreviation member-sequence (sequence
                                                        &key (test 'eql testp))
  (make-presentation-type-specifier
   `(completion ,sequence ,@(and testp `(:test ,test)))
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers
   :printer printer
   :highlighter highlighter)
  :options #.+completion-options+)

(defun member-alist-value-key (element)
  (cond ((atom element)
         element)
        ((atom (cdr element))
         (cdr element))
        ((null (cddr element))
         (cadr element))
        (t (getf (cdr element) :value))))

(defun member-alist-doc-key (element)
  (if (and (consp element) (consp (cdr element)) (consp (cddr element)))
      (getf (cdr element) :documentation)))

(define-presentation-type-abbreviation member-alist (alist
                                                     &key (test 'eql testp))
  (make-presentation-type-specifier
   `(completion ,alist ,@(and testp `(:test ,test))
                :value-key member-alist-value-key)
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers
   :printer printer
   :highlighter highlighter)
  :options ((name-key 'default-completion-name-key)
            (documentation-key 'member-alist-doc-key)
            (partial-completers '(#\Space))
            (printer #'write-token)
            (highlighter #'highlight-completion-choice)))

(define-presentation-type subset-completion (sequence
                                             &key (test 'eql)
                                             (value-key 'identity))
  :options ((name-key 'default-completion-name-key)
            documentation-key
            (partial-completers '(#\Space))
            (separator #\,)
            (echo-space t)
            (printer #'write-token)
            (highlighter #'highlight-completion-choice))
  :inherit-from t)

(define-presentation-method presentation-typep (object
                                                (type subset-completion))
  (map nil #'(lambda (obj)
               (unless (find obj sequence :test test :key value-key)
                 (return-from presentation-typep nil)))
       object)
  t)

(define-presentation-method presentation-subtypep ((type subset-completion)
                                                   maybe-supertype)
  (with-presentation-type-parameters (subset-completion maybe-supertype)
    (let ((super-sequence sequence)
          (super-test test)
          (super-value-key value-key))
      (with-presentation-type-parameters (subset-completion type)
        (values (sequence-subset-p sequence test value-key
                                   super-sequence super-test super-value-key)
                t)))))

(define-presentation-method present ((object list) (type subset-completion)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for tail on object
        for (obj) = tail
        do (progn
             (present obj (presentation-type-of object)
                        :stream stream :view view
                        :acceptably acceptably
                        :sensitive nil)
             (when (cdr tail)
               (if acceptably
                   (princ separator stream)
                   (terpri stream))))))

(define-presentation-method present ((object vector) (type subset-completion)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for i from 0 below (length object)
        for obj = (aref object i)
        do (progn
             (present obj (presentation-type-of object)
                        :stream stream :view view
                        :acceptably acceptably
                        :sensitive nil)
             (when (< i (1- (length object)))
               (if acceptably
                   (princ separator stream)
                   (terpri stream))))))

;;; XXX is it a typo in the spec that subset, subset-sequence and subset-alist
;;; have the same options as completion, and not subset-completion?

(define-presentation-type-abbreviation subset (&rest elements)
  (make-presentation-type-specifier `(subset-completion ,elements)
                                    :name-key name-key
                                    :documentation-key documentation-key
                                    :partial-completers partial-completers
                                    :printer printer
                                    :highlighter highlighter)
  :options #.+completion-options+)

(define-presentation-type-abbreviation subset-sequence (sequence
                                                        &key (test 'eql testp))
  (make-presentation-type-specifier
   `(subset-completion ,sequence ,@(and testp `(:test ,test)))
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers
   :printer printer
   :highlighter highlighter)
  :options #.+completion-options+)

(define-presentation-type-abbreviation subset-alist (alist
                                                     &key (test 'eql testp))
  (make-presentation-type-specifier
   `(subset-completion ,alist ,@(and testp `(:test ,test))
                       :value-key member-alist-value-key)
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers
   :printer printer
   :highlighter highlighter)
  :options ((name-key 'default-completion-name-key)
            (documentation-key 'member-alist-doc-key)
            (partial-completers '(#\Space))
            (printer #'write-token)
            (highlighter #'highlight-completion-choice)))

(define-presentation-type sequence (element-type)
  :options ((separator #\,) (echo-space t))
  :inherit-from 't
  :parameters-are-types t)

(define-presentation-method presentation-type-specifier-p ((type sequence))
  (and (listp type)
       (consp (rest type))
       (presentation-type-specifier-p (second type))))

(define-presentation-method presentation-typep (object (type sequence))
  ;; XXX TYPE here is the sequence element type, not the whole type specifier
  (unless (or (listp object) (vectorp object))
    (return-from presentation-typep nil))
  (let ((real-type (expand-presentation-type-abbreviation element-type)))
    (map nil #'(lambda (obj)
                 (unless (presentation-typep obj real-type)
                   (return-from presentation-typep nil)))
         object)
    t))

(define-presentation-method presentation-subtypep ((type sequence)
                                                   maybe-supertype)
  (with-presentation-type-parameters (sequence type)
    ;; now TYPE is bound to the parameter TYPE
    (let ((real-type (expand-presentation-type-abbreviation element-type)))
      (with-presentation-type-parameters (sequence maybe-supertype)
        (let ((real-super-type (expand-presentation-type-abbreviation element-type)))
          (presentation-subtypep real-type real-super-type))))))

(define-presentation-method present ((object list) (type sequence)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for tail on object
        for (obj) = tail
        do (progn
             (present obj type          ; i.e., the type parameter
                        :stream stream :view view
                        :acceptably acceptably
                        :sensitive nil)
             (when (cdr tail)
               (write-char separator stream)))))

(define-presentation-method present ((object vector) (type sequence)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for i from 0 below (length object)
        for obj = (aref object i)
        do (progn
             (present obj type          ; i.e., the type parameter
                        :stream stream :view view
                        :acceptably acceptably
                        :sensitive nil)
             (when (< i (1- (length object)))
               (write-char separator stream)))))


(define-presentation-method accept ((type sequence)
                                    stream
                                    (view textual-view)
                                    &key)
  (loop
     with separators = (list separator)
     for element = (accept element-type ; i.e., the type parameter
                           :stream stream
                           :view view
                           :prompt nil
                           :additional-delimiter-gestures separators)
     collect element
     do (progn
          (when (not (eql (peek-char nil stream nil nil) separator))
            (loop-finish))
          (read-char stream)
          (when echo-space
            ;; Make the space a noise string
            (input-editor-format stream " ")))))


(define-presentation-type sequence-enumerated (&rest types)
  :options ((separator #\,) (echo-space t))
  :inherit-from 't
  :parameters-are-types t)

(define-presentation-method presentation-typep (object
                                                (type sequence-enumerated))
  (unless (or (listp object) (vectorp object))
    (return-from presentation-typep nil))
  (map nil #'(lambda (obj type)
               (let ((real-type (expand-presentation-type-abbreviation type)))
                 (unless (presentation-typep obj real-type)
                   (return-from presentation-typep nil))))
       object
       types)
  t)

(define-presentation-method presentation-subtypep ((type sequence-enumerated)
                                                   maybe-supertype)
  (with-presentation-type-parameters (sequence-enumerated maybe-supertype)
    (let ((supertypes types))
      (with-presentation-type-parameters (sequence-enumerated type)
        (unless (eql (length supertypes) (length types))
          (return-from presentation-subtypep (values nil t)))
        (map nil
             #'(lambda (element-type element-supertype)
                 (let ((real-type (expand-presentation-type-abbreviation
                                   element-type))
                       (real-supertype (expand-presentation-type-abbreviation
                                        element-supertype)))
                   (multiple-value-bind (subtypep determined)
                       (presentation-subtypep real-type real-supertype)
                     (cond ((not determined)
                            (return-from presentation-subtypep
                              (values nil nil)))
                           ((not subtypep)
                            (return-from presentation-subtypep
                              (values nil t)))))))
             types
             supertypes)
        (values t t)))))

(define-presentation-method present ((object list) (type sequence-enumerated)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for tail on object
        for (obj) = tail
        for obj-type in types
        do (progn
             (present obj obj-type
                        :stream stream :view view
                        :acceptably acceptably
                        :sensitive nil)
             (when (cdr tail)
               (if acceptably
                   (princ separator stream)
                   (terpri stream))))))

(define-presentation-method present ((object vector) (type sequence-enumerated)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for i from 0 below (length object)
        for obj = (aref object i)
        for obj-type in types
        do (progn
             (present obj obj-type
                        :stream stream :view view
                        :acceptably acceptably
                        :sensitive nil)
             (when (< i (1- (length object)))
               (if acceptably
                   (princ separator stream)
                   (terpri stream))))))

(define-presentation-method accept ((type sequence-enumerated)
                                    stream
                                    (view textual-view)
                                    &key)
  (loop
     with element = nil and element-type = nil
       and separators = (list separator)
     for type-tail on types
     for (this-type) = type-tail
     do (setf (values element element-type)
              (accept this-type
                      :stream stream
                      :view view
                      :prompt t
                      :display-default nil
                      :additional-delimiter-gestures separators))
     collect element into sequence-val
     do (progn
          (when (not (eql (peek-char nil stream nil nil) separator))
            (loop-finish))
          (read-char stream)
          (when echo-space
            ;; Make the space a noise string
            (input-editor-format stream " ")))
     finally (if (cdr type-tail)
                 (simple-parse-error "Input ~S too short for ~S."
                                     sequence-val
                                     types)
                 (return sequence-val))))

(define-presentation-type or (&rest types)
  :inherit-from t
  :parameters-are-types t)

(define-presentation-method presentation-typep (object (type or))
  (loop for type in types
        for real-type = (expand-presentation-type-abbreviation type)
        do (when (presentation-typep object real-type)
             (return-from presentation-typep t)))
  nil)

(define-presentation-method present (object (type or)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (loop for or-type in types
        for expanded-type = (expand-presentation-type-abbreviation or-type)
        do (when (presentation-typep object expanded-type)
             (present object expanded-type
                      :stream stream :view view
                      :acceptably acceptably
                      :for-context-type for-context-type)
             (loop-finish))))

(define-presentation-method accept ((type or)
                                    (stream input-editing-stream)
                                    (view textual-view)
                                    &key)
  (with-input-context (type)
      (object type-var)
      (let ((str (read-token stream)))
	(loop for or-type in types
	   do
	     (handler-case
		 (progn
		   (return (accept-from-string or-type
					       str
					       :view view)))
	       (parse-error ()))
	   finally (simple-parse-error "Input type is not one of ~S" types)))
    (t
     (presentation-replace-input stream object type-var view :rescan nil)
     (return-from accept (values object type-var)))))

;;; What does and inherit from?  Maybe we'll punt on that for the moment.
;;; Unless it inherits from its arguments...

(define-presentation-type and (&rest types)
  :parameters-are-types t)

(define-presentation-method presentation-typep (object (type and))
  (loop for type in types
        for real-type = (expand-presentation-type-abbreviation type)
        do (with-presentation-type-decoded (name parameters)
             real-type
             (cond ((eq name 'satisfies)
                    (unless (funcall (car parameters) object)
                      (return-from presentation-typep nil)))
                   ((eq name 'not)
                    (unless (not (presentation-typep object (car parameters)))
                      (return-from presentation-typep nil)))
                   (t (unless (presentation-typep object real-type)
                        (return-from presentation-typep nil))))))
  t)

(define-presentation-method present (object (type and)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (present object (expand-presentation-type-abbreviation (car types))
           :stream stream :view view
           :acceptably acceptably
           :for-context-type for-context-type))

(define-presentation-method accept
    ((type and) (stream input-editing-stream) (view textual-view) &rest args &key)
  (let ((subtype (first types)))
    (multiple-value-bind (obj ptype)
        (apply-presentation-generic-function accept subtype stream view args)
      (declare (ignore ptype))
      (unless (presentation-typep obj type)
        (simple-parse-error "Input object ~S is not of type ~S" obj type))
      obj)))

(define-presentation-type-abbreviation token-or-type (tokens type)
  `(or (member-alist ,tokens) ,type))

(define-presentation-type-abbreviation null-or-type (type)
  `(or null ,type))

(define-presentation-type-abbreviation type-or-string (type)
  `(or ,type string))

(define-presentation-method presentation-typep (object (type expression))
  (declare (ignore object))
  t)

(define-presentation-method present (object (type expression)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (let ((*print-readably* acceptably))
    (prin1 object stream)))
