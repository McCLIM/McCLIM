(in-package :creating-assoc)

;;; Doesn't work:
;;;
;;; (defun creating-assoc (item alist)
;;;   "assoc that creates the requested alist item on-the-fly if not yet existing"
;;;   (let ((item-assoc (assoc item alist)))
;;;     (unless item-assoc
;;;       (let ((new-item (list item)))
;;;         (push new-item alist)
;;;         (setf item-assoc new-item)))))

;;; Doesn't work:
;;;
;;; (defmacro creating-assoc (item alist)
;;;   "assoc that creates the requested alist item on-the-fly if not yet existing"
;;;   (let ((entry (gensym "entry-"))
;;;         (new-item (gensym "new-item-"))
;;;         (the-item (gensym "the-item-"))
;;;         (the-alist (gensym "the-alist-")))
;;;     `(let* ((,the-item ,item)
;;;             (,the-alist ,alist)
;;;             (,entry (assoc ,the-item ,the-alist)))
;;;        (unless ,entry
;;;          (let ((,new-item (list ,the-item)))
;;;            (push ,new-item ,the-alist)
;;;            (setf ,entry ,new-item))))))

;;; Does work, but ALIST will be evaluated twice:
;;;
;;; (defmacro creating-assoc (item alist)
;;;   "assoc that creates the requested alist item on-the-fly if not yet existing"
;;;   (let ((entry (gensym "entry-"))
;;;         (new-item (gensym "new-item-"))
;;;         (the-item (gensym "the-item-")))
;;;     `(let* ((,the-item ,item)
;;;             (,entry (assoc ,the-item ,alist)))
;;;        (unless ,entry
;;;          (let ((,new-item (list ,the-item)))
;;;            (push ,new-item ,alist)
;;;            (setf ,entry ,new-item))))))

;;; From SBCL source, uses GET-SETF-METHOD, a relic from pre-ANSI Common Lisp:
;;;
;;; (defmacro-mundanely push (obj place &environment env)
;;;   #!+sb-doc
;;;   "Takes an object and a location holding a list. Conses the object onto
;;;   the list, returning the modified list. OBJ is evaluated before PLACE."
;;;   (multiple-value-bind (dummies vals newval setter getter)
;;;       (get-setf-method place env)
;;;     (let ((g (gensym)))
;;;       `(let* ((,g ,obj)
;;;               ,@(mapcar #'list dummies vals)
;;;               (,(car newval) (cons ,g ,getter)))
;;;          ,setter))))

;;; Example CLHS page on GET-SETF-EXPANSION:
;;; (Notice that there is an error, "(if (cdr new)" should be replaced by "(if (cdr ,(car new))".)
;;;
;;;  (defmacro xpop (place &environment env)
;;;    (multiple-value-bind (dummies vals new setter getter)
;;;                         (get-setf-expansion place env)
;;;       `(let* (,@(mapcar #'list dummies vals) (,(car new) ,getter))
;;;          (if (cdr new) (error "Can't expand this."))
;;;          (prog1 (car ,(car new))
;;;                 (setq ,(car new) (cdr ,(car new)))
;;;                 ,setter))))

;;; New version, still does not work:
;;;
;;; (defun creating-assoc (item alist)
;;;   "assoc that creates the requested alist item on-the-fly if not yet existing"
;;;   (or (assoc item alist)
;;;       (first (push (list item) alist))))

;;; Macro based on the new (not-working) defun, works and is nice:
;;;
(defmacro creating-assoc (item alist &environment env)
  "assoc that creates the requested alist item on-the-fly if not yet existing"
   (multiple-value-bind (dummies vals new setter getter)
                        (get-setf-expansion alist env)
     (let ((object (gensym "object-")))
       `(let* ((,object ,item)
               ,@(mapcar #'list dummies vals)
               (,(car new) ,getter))
          (prog1
              (or (assoc ,object ,(car new))
                  (first (setq ,(car new) (cons (list ,object) ,(car new)))))
            ,setter)))))

;;; Example:
;;;
;;; (let* ((list '((foo 1))))
;;;             (list (assoc 'foo list)
;;;                   (assoc 'baz list)
;;;                   (creating-assoc 'baz list)
;;;                   (assoc 'baz list)
;;;                   list))
;;; => ((FOO 1)
;;;     NIL
;;;     (BAZ)
;;;     (BAZ)
;;;     ((BAZ) (FOO 1)))

;;;
;;;    (creating-assoc 'baz list)
;;;
;;; expands to:
;;;
;;;    (LET* ((#:|object-15058| 'BAZ) (#:G15057 LIST))
;;;      (PROG1
;;;          (OR (ASSOC #:|object-15058| #:G15057)
;;;              (FIRST (SETQ #:G15057 (CONS (LIST #:|object-15058|) #:G15057))))
;;;        (SETQ LIST #:G15057)))


;;; Have a look at http://paste.lisp.org/display/13846#2 if you want to.
