(in-package :clim-listener)

;;; (C) Copyright 2003 by Andy Hefner (hefner1@umbc.edu)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.



;;; Assorted commands intended for lisp development.


;; TODO: Split these into several command tables, and eventually
;; into seperate files. We need seperate command tables for lisp
;; development, filesystem manipulation, UNIX crap, and whatever
;; else suggests itself. Mostly to make them layout nice in a
;; command menu, if anyone wants such a thing.

;; Some of the presentation translators might be broken, need to look ino that.

(define-command-table dev-commands)


;;; Presentation types

(define-presentation-type class () :inherit-from 'expression)
(define-presentation-type class-name () :inherit-from 'symbol)
(define-presentation-type slot-definition () :inherit-from 'expression)
(define-presentation-type function-name () :inherit-from 'symbol)
(define-presentation-type process () :inherit-from 'expression)
(define-presentation-type directory-stack () :inherit-from 'expression)

;;; Presentation translators

(define-presentation-translator class-name-to-class
  (class-name class dev-commands
     :documentation ((name stream) (format stream "Class object ~A" name ))
     :gesture T)
  (object)
  (find-class object))

(define-presentation-translator symbol-to-class
  (symbol class dev-commands
     :documentation ((name stream) (format stream "Class object ~A" name))
     :gesture T
     :tester ((object) (not (not (find-class object nil))))
     :tester-definitive T)
  (object)
  (find-class object))

(define-presentation-translator symbol-to-class-name
  (symbol class-name dev-commands
     :documentation ((name stream) (format stream "Class ~A" name))
     :gesture T
     :tester ((object) (not (not (find-class object nil))))
     :tester-definitive T)
  (object)
  object)

(define-presentation-translator class-to-class-name
  (class class-name dev-commands
     :documentation ((class stream) (format stream "Class of ~A" class))
     :gesture T)
  (object)
  (clim-mop:class-name object))

(define-presentation-translator symbol-to-function-name
  (symbol function-name dev-commands
     :documentation ((object stream) (format stream "Function ~A" object))
     :gesture T
     :tester ((object) (fboundp object))
     :tester-definitive T)
  (object) object)


;;; Mundane commands that don't really belong here.
;;; Arguably, these belong in the standard command table.

(define-command (com-clear-output :name "Clear Output History"
				   :command-table dev-commands)
    ()
  (window-clear *standard-output*))

(define-command (com-exit :name "Exit"
			  :command-table dev-commands)
    ()
  (frame-exit *application-frame*))


;;; Commands related to Lisp development
;;; ------------------------------------

(defvar *apropos-list* nil
  "The apropos command stores its output here.")

(defun present-symbol (symbol &optional (stream *standard-output*) show-package)
  (clim:with-output-as-presentation (stream symbol 'clim:symbol)
        (let ((style (clim:make-text-style :fix
					   (if (fboundp symbol)
					       :bold
					       (if (boundp symbol)
						   :roman
						   :italic))
					   nil)))
	  (clim:with-text-style (stream style)
            (if show-package (let ((*package* (find-package :common-lisp-user)))
			       (format stream "~W" symbol))
	      (princ (symbol-name symbol) stream))
	    (when (boundp symbol)		  
	      (clim:with-text-face (stream :roman)
		 (clim:with-text-size (stream :small)		     
		    (format stream " = ~A" (symbol-value symbol)))))))))

(define-command (com-apropos :name "Apropos"
			     :command-table dev-commands)
    ((string 'clim:string :prompt "string"))
; Fix keyword handling in the CP sometime..
;     &key (package-name 'package-name :prompt "in package" :default nil)     
  (setf *apropos-list* (apropos-list string #+nil(find-package package-name)))
  (dolist (sym *apropos-list*)
    (present-symbol sym *standard-output* T)
      (terpri))
  (clim:with-text-face (T :italic)
       (format T "Results have been saved to *APROPOS-LIST*~%")))

(define-command (com-trace :name "Trace"
			   :command-table dev-commands)
    ((fsym 'function-name :prompt "function-name"))
  (if (fboundp fsym)
      (progn 
	(eval `(trace ,fsym))
	(format T "~&Tracing ~W.~%" fsym))
    (format T "~&Function ~W is not defined.~%" fsym)))

(define-command (com-untrace :name "Untrace"
			     :command-table dev-commands)
    ((fsym 'symbol :prompt "function name"))
  (if (fboundp fsym)
      (progn
	(eval `(untrace ,fsym))
	(format T "~&~W will no longer be traced.~%" fsym))
    (format T "~&Function ~W is not defined.~%" fsym)))


(define-command (com-load-file :name "Load File"
                               :command-table dev-commands)
  ((pathname 'pathname :prompt "pathname"))
  (load pathname))

(define-command (com-compile-file :name "Compile File"
                                  :command-table dev-commands)
  ((pathname 'pathname :prompt "pathname"))
  (compile-file pathname))

(define-command (com-compile-and-load :name "Compile and load"
                                      :command-table dev-commands)
  ((pathname 'pathname :prmopt "pathname"))
  (load (compile-file pathname)))

;;; CLOS introspection commands

;; Merge these.. due to (whatever) the use of normal/shaded ink here is screwy, and
;; out to be ripped out or rethought.
(defun graph-superclasses (stream class)
  "Displays a class and its superclasses on stream using graph formatting."
  (let ((cpl (clim-mop:class-precedence-list class)) 
	(normal-ink +foreground-ink+)
	(shaded-ink (make-rgb-color 0.60 0.66 0.77))
	(text-style (make-text-style :serif :roman :small)))
    (format-graph-from-roots (list class)
			     #'(lambda (class stream)
				 (with-drawing-options (stream :ink (if (find class cpl)
									normal-ink
								      shaded-ink)
							       :text-style text-style)						       
				   (surrounding-output-with-border (stream :shape :drop-shadow)
				     (present (clim-mop:class-name class) 'class-name))))
			     #'(lambda (class)
				 (if (find class cpl)
				     (prog1 (clim-mop:class-direct-superclasses class)
				       (setf cpl (delete class cpl))) ; FIXME?
				   nil))
			     :stream stream
			     :orientation :horizontal
			     :arc-drawer
			     #'(lambda (stream foo bar x1 y1 x2 y2)
				 (declare (ignore foo bar))
				 (draw-arrow* stream x1 y1 x2 y2)))))

(defun graph-subclasses (stream class)
  "Displays a class and its subclasses on stream using graph formatting."
  (let ((normal-ink +foreground-ink+)	
	(text-style (make-text-style :serif :roman :small)))
    (format-graph-from-roots (list class)
			     #'(lambda (class stream)
				 (with-drawing-options (stream :ink normal-ink
							       :text-style text-style)
				   (with-output-as-presentation (stream class 'clim-mop:class-name)
				    (surrounding-output-with-border (stream :shape :drop-shadow)
 					 (present (clim-mop:class-name class))))  ))
			     #'(lambda (class)
				   (clim-mop:class-direct-subclasses class))
			     :stream stream
			     :orientation :horizontal
			     :arc-drawer
			     #'(lambda (stream foo bar x1 y1 x2 y2)
				 (declare (ignore foo bar))
				 (draw-arrow* stream x1 y1 x2 y2)))))

(defun frob-to-class (spec)
  (if (typep spec 'class)
      spec
    (find-class spec nil)))

(define-command (com-show-class-superclasses :name "Show Class Superclasses"
                                             :command-table dev-commands)
    ((class-spec 'class-name :prompt "class"))
  (let ((class (frob-to-class class-spec)))
    (if (null class)
	(format T "~&~A is not a defined class.~%" class-spec)
      (graph-superclasses *standard-output* class))))

(define-command (com-show-class-subclasses :name "Show Class Subclasses"
                                           :command-table dev-commands)
    ((class-spec 'class-name :prompt "class"))
  (let ((class (frob-to-class class-spec)))
    (if (not (null class))
	(graph-subclasses *standard-output* class)
      (format T "~&~A is not a defined class.~%" class-spec))))


; Lookup direct slots from along the CPL given a class and a slot name.
; Returns them in an order parallel with the CPL.
; Need this to find readers/writers, which exist in the direct slot
; definitions, not the effective slot definitions. (ouch)
(defun direct-slot-definitions (class slot-name)
  (let ((cpl (reverse (clim-mop:class-precedence-list class)))
        (direct-slots nil))
    (dolist (foo cpl)
      (let ((dslots (clim-mop:class-direct-slots foo)))
        (dolist (slot dslots)
          (when (eq slot-name (clim-mop:slot-definition-name slot))
            (push slot direct-slots)))))
    direct-slots))

(defparameter *slot-name-ink*     +black+)
(defparameter *slot-type-ink*     +gray50+)
(defparameter *slot-initargs-ink* +red+)
(defparameter *slot-initform-ink* +goldenrod3+)
(defparameter *slot-readers-ink*  +black+)
(defparameter *slot-writers-ink*  +black+)
(defparameter *slot-documentation-ink* +turquoise4+)

(defmacro italisansf (&rest args)
  `(italic (T)
     (with-text-family (T :sans-serif)
       (format T ,@args))))   

;; Oops, I'm ignoring STREAM.
(defun present-slot (slot class &key (stream *standard-output*))
  "Formats a slot definition into a table row."
  (let* ((name (clim-mop:slot-definition-name slot))
         (type (clim-mop:slot-definition-type slot))
         (initargs (clim-mop:slot-definition-initargs slot))
         (initfunc (clim-mop:slot-definition-initfunction slot))
         (initform (clim-mop:slot-definition-initform slot))
         (direct-slots (direct-slot-definitions class name))
         (readers (reduce #'append (filtermap direct-slots #'clim-mop:slot-definition-readers)))
         (writers (reduce #'append (filtermap direct-slots #'clim-mop:slot-definition-writers)))
         (documentation (first (filtermap direct-slots (lambda (x) (documentation x T))))))

  (macrolet ((fcell ((var align-x &rest cell-opts) &body body)
                `(formatting-cell (T :align-x ,align-x ,@cell-opts)
                   (with-drawing-options (T :ink ,(intern (concatenate 'string "*SLOT-" (symbol-name var) "-INK*")))
                    ,@body))))
    
    (fcell (name :left)    
     (with-output-as-presentation (stream slot 'slot-definition)
       (princ name)))

    (fcell (type :center :min-width (text-size (sheet-medium *standard-output*) "integer"))
      (princ type))

    (fcell (initargs :right)
      (dolist (x initargs)
        (format T "~W~%" x)))

    (fcell (initform :left)
      (if initfunc                          
          (format T "~W" initform)
        (italisansf "No initform")))

    (fcell (readers :center)
      (if readers
          (dolist (reader readers)  (format T "~A~%" reader))
        (italisansf " No readers")))

    (fcell (writers :center)
      (if writers
          (dolist (writer writers)  (format T "~A~%" writer))
        (italisansf "No writers")))

    (fcell (documentation :left)
      (when documentation (with-text-family (T :serif) (princ documentation))))
)))


(defun earliest-slot-definer (slot class)
  "Returns the earliest class in the CPL of CLASS which defines SLOT."
  (let ((name (clim-mop:slot-definition-name slot)))
    (dolist (class (reverse (clim-mop:class-precedence-list class)))
      (dolist (slot-b (clim-mop:class-direct-slots class))
        (when (eq name (clim-mop:slot-definition-name slot-b))
          (return-from earliest-slot-definer class)))))
  (error "Slot ~W doesn't seem to be defined in ~W" slot class))

(defun class-sorted-slots (class)
  "Sort the slots in order of definition within the CPL, superclasses first."
  (let ((cpl (clim-mop:class-precedence-list class)))
    (sort (clim-mop:class-slots class)
          (lambda (a b)
            (< (position (earliest-slot-definer a class) cpl)
               (position (earliest-slot-definer b class) cpl))))))

;; What is going on with this and the table formatter?
;; This is out of sync with PRESENT-SLOT, presently.
(defun print-slot-table-heading (stream)
  (debugf "Before formatting-row")
  (formatting-row (T)
    (debugf "Inside formatting-row")
    (dolist (name '("Slot name" "Type" "Initargs" "Initform"))
      (debugf name)
      (formatting-cell (stream :align-x :center)
        (debugf "inside the cell")
        (princ name stream)
        #+nil(underlining (stream) (princ name stream))))))

(defun present-slot-list (slots class)
  (formatting-table (T)
    ;(debugf "before header")
    ;(print-slot-table-heading T)
    ;(debugf "after header")
      (dolist (slot slots)
        (formatting-row (T)
          (present-slot slot class)))))

(defun friendly-slot-allocation-type (allocation)
  (if (typep allocation 'standard-class)
      (class-name allocation)
    allocation))

(defun present-the-slots (class)  
  (let* ((slots (class-sorted-slots class))
         (instance-slots (remove-if (lambda (x) (not (eq :instance (clim-mop:slot-definition-allocation x))))
                                    slots))
         (other-slots (set-difference slots instance-slots))
         (allocation-types (remove-duplicates (mapcar #'clim-mop:slot-definition-allocation other-slots))))
    (when other-slots
      (underlining (T) (format T "~&Instance Slots~%")))
    (present-slot-list instance-slots class)
    (dolist (alloc allocation-types)
      (underlining (T)
        (format T "~&Allocation: ~A~%" (friendly-slot-allocation-type alloc)))
      (present-slot-list (remove-if (lambda (x)
                                      (not (eq alloc (clim-mop:slot-definition-allocation x))))
                                    other-slots)
                         class))))

(define-command (com-show-class-slots :name "Show Class Slots"
				      :command-table dev-commands)
    ((class-name 'clim:symbol :prompt "class name"))
  (let ((class (find-class class-name nil)))
    (if (null class)
	(format T "~&~A is not a defined class.~%" class-name)
      (let ((slots (clim-mop:class-slots class)))
	(if (null slots)
	    (with-text-face (T :italic)
	       (format T "~&~%This class has no slots.~%~%"))
	  (progn
            ; oddly, looks much better in courier, because of all the capital letters.
;            (with-text-family (T :sans-serif)
              (invoke-as-heading
                (lambda ()
                  (format T "~&Slots for ")
                  (with-output-as-presentation (T (clim-mop:class-name class) 'class-name)
                    (princ (clim-mop:class-name class)))))         
              (terpri)
              (present-the-slots class) ))))))



;;; Filesystem Commands
;;; -------------------

(defun gen-wild-pathname (pathname)
  "Build a pathname with appropriate :wild components for the directory listing."
  (make-pathname :host   (pathname-host pathname)
                 :device (pathname-device pathname)
                 :directory (pathname-directory pathname)
                 :name (or (pathname-name pathname) :wild)
                 :type (or (pathname-type pathname) :wild)
                 :version (or (pathname-version pathname) :wild)))

(defun strip-filespec (pathname)
  "Removes file, directory, and version spcs from a pathname."
  (make-pathname :host   (pathname-host pathname)
                 :device (pathname-device pathname)
                 :directory (pathname-directory pathname)
                 :name nil
                 :type nil
                 :version nil))

(defun parent-directory (pathname)
  "Returns a pathname designating the directory 'up' from PATHNAME"
  (let ((dir (pathname-directory (truename (strip-filespec pathname)))))
    (when (and (eq (first dir) :absolute)
               (not (zerop (length (rest dir)))))
      (make-pathname :host   (pathname-host pathname)
                     :device (pathname-device pathname)
                     :directory `(:absolute ,@(nreverse (rest (reverse (rest dir)))))
                     :name (pathname-name pathname)
                     :type (pathname-type pathname)
                     :version (pathname-version pathname)))))

(defun pretty-pretty-pathname (pathname)
  (with-output-as-presentation (T pathname 'clim:pathname)
    (let ((icon (icon-of pathname)))
      (when icon  (draw-icon T icon :extra-spacing 3)))
    (princ pathname))
  (fresh-line))

;; Change show-directory to use the format-items at some point,
;; but presently it doesn't seem to work. (for me, at least)
#+nil
(defparameter *show-dir-table-min-items* 10
  "The minimum number of items in a directory listing required to invoke table formatting.")
#+nil
(defparameter *show-dir-table-max-items* T
  "The maximum number of items in a directory listing for which table formatting should be used.
Presumably you would set this if you decided table formatting on your 600 line home directory
just took too long.")

;; Change to using an :ICONIC view for pathnames?
;; I'm not certain views work in McCLIM right now, so put it off..
(define-command (com-show-directory :name "Show Directory"
				    :command-table dev-commands)
    ((pathname 'pathname #+nil(or 'string 'pathname) :prompt "pathname"))  
  (let* ((pathname (if (wild-pathname-p pathname) ;; WHAT THE HELL
                      (merge-pathnames pathname)  ;; AM I DOING ??
                    pathname))
         (dir (directory (gen-wild-pathname pathname) #+cmu :truenamep #+cmu nil)))
    (with-text-family (T :sans-serif)
      (invoke-as-heading
       (lambda ()
         (format T "~&Directory contents of ")
         (present pathname)))
    (format T "~%")
    (stream-increment-cursor-position *standard-output* 0 3)
    
    (when (parent-directory pathname)
      (with-output-as-presentation (T (parent-directory pathname) 'clim:pathname)
        (draw-icon T (standard-icon "up-folder.xpm") :extra-spacing 3)
        (format T "Parent Directory~%")))

;    (format-items dir))))
    
    (dolist (ent dir)
      (let ((ent (merge-pathnames ent pathname))) ; This is for CMUCL, see above.
        (pretty-pretty-pathname ent))))))

#+nil
(define-presentation-to-command-translator show-directory-translator
  (clim:pathname com-show-directory dev-commands :gesture :select
		 :pointer-documentation ((object stream)
					 (format stream "Show directory ~A" object))
                 :tester-definitive T
		 :tester ((object)
			  (directoryp object)))
  (object)
  (list object))


(define-command (com-change-directory :name "Change Directory"
                                      :command-table dev-commands)
  ((pathname 'pathname :prompt "pathname"))
  (let ((pathname (merge-pathnames pathname)))
    (if (directoryp pathname)
        (setf *default-pathname-defaults* pathname)
      (format T "~&~A is not a directory.~%" pathname))))

(define-command (com-up-directory :name "Up Directory"
                                  :command-table dev-commands)
  ()
  (let ((parent (parent-directory *default-pathname-defaults*)))
    (when parent
      (setf *default-pathname-defaults* parent)
      (italic (T) (format T "~&The current directory is now ")
              (present (truename parent))
              (terpri)))))
  
(define-gesture-name :change-directory :pointer-button-press
  (:middle))

(define-presentation-to-command-translator change-directory-translator
  (clim:pathname com-change-directory dev-commands :gesture :change-directory
		 :pointer-documentation ((object stream)
					 (format stream "Change directory to ~A" object))
		 :tester ((object)
			  (directoryp object)))
  (object)
  (list object))


;;; External file viewers

;; This pathname translator stuff is really turning into a mess.
;; What I need to do is merge mime types with presentations, and
;; rip all the rest of this shit out.

(defmethod mime-type-to-command (mime-type pathname)
  (declare (ignore mime-type pathname))
  (values nil nil))

(defmethod mime-type-to-command ((mime-type null) pathname)
  (declare (ignore #|mime-type|# pathname))
  (values nil nil))

(defmethod mime-type-to-command ((mime-type symbol) pathname)
  (mime-type-to-command (clim-mop:class-prototype (find-class mime-type nil)) pathname))

;; Move these elsewhere.

(defmethod mime-type-to-command ((mime-type text/x-lisp-source) pathname)
  (values `(com-compile-and-load ,pathname)
          (format nil "Compile and load ~A" pathname)))

(defmethod mime-type-to-command ((mime-type application/x-lisp-fasl) pathname)
  (values `(com-load-file ,pathname)
          (format nil "Load ~A" pathname)))

; FIXME: Make this smart enough to load systems, somehow. Given that a system
; could be mk-defsystem or ASDF (or Symbolics or god knows what else), this
; might be tricky. Alternately, setup a seperate type for .ASD files that can
; operate on them smartly.
(defmethod mime-type-to-command ((mime-type text/x-lisp-system) pathname)
  (values `(com-load-file ,pathname)
          (format nil "Load ~A" pathname)))

(defun pprint-viewspec (viewspec)
  (when viewspec
    (with-output-to-string (out)
      (format out "Run ")
      (dolist (x (rest (second viewspec)))
        (princ x out)
        (princ " " out)))))


(defun automagic-translator (pathname)
  "Returns two values, the command translation, and a documentation string for the translation."
;  (debugf pathname)
  (when (or (wild-pathname-p pathname) 
            (not (probe-file pathname)))
    (return-from automagic-translator nil))
  (if (directoryp pathname)
      (values `(com-show-directory ,pathname) ; hopefully this doesn't happen, and the other translator runs.
              (format nil "Show directory ~A" pathname))
    (let ((viewspec (find-viewspec pathname))
          (mime-type (pathname-mime-type pathname)))
      (mv-or
       (when mime-type (mime-type-to-command mime-type pathname))
       (when viewspec
         (values `(com-run ,@viewspec)
                 (pprint-viewspec viewspec)))))))

(define-presentation-translator automagic-pathname-translator
  (clim:pathname clim:command dev-commands
                 :gesture :select
                 :tester ((object)
                          (automagic-translator object))
                 :pointer-documentation ((object stream) (princ (nth-value 1 (automagic-translator object)) stream)))
  (object)
  (values
   (automagic-translator object)
   'command))


;;; The directory stack.

(defvar *directory-stack* nil)

(define-command (com-push-directory :name "Push Directory"
                                    :command-table dev-commands)
  ((pathname 'pathname :prompt "pathname"))
  (let ((pathname (merge-pathnames pathname)))
    (if (and (probe-file pathname)
             (directoryp pathname));; hrm.. need smart conversion to directories..
        (progn (push *default-pathname-defaults* *directory-stack*)
               (com-change-directory pathname))
      (italic (T)
        (fresh-line) (present (truename pathname))
        (format T " does not exist or is not a directory.~%")) )))

(defun comment-on-dir-stack ()
  (if *directory-stack*
      (progn
        (format T "~&The top of the directory stack is now ")
        (present (truename (first *directory-stack*)))
        (terpri))
    (format T "~&The directory stack is now empty.~%")))

(define-command (com-pop-direcory :name "Pop Directory"
                                  :command-table dev-commands)
  ()
  (if (null *directory-stack*)
      (italic (T) (format T "~&The directory stack is empty!~%"))
    (progn 
      (com-change-directory (pop *directory-stack*))
      (italic (T) (comment-on-dir-stack)))))

(define-command (com-drop-directory :name "Drop Directory"
                                    :command-table dev-commands)
  ()
  (italic (T)
    (if (null *directory-stack*)
        (format T "~&The directory stack is empty!~%")
      (progn
        (setf *directory-stack* (rest *directory-stack*))
        (comment-on-dir-stack)))))

(define-command (com-swap-directory :name "Swap Directory"
                                    :command-table dev-commands)
  ()
  (italic (T)
    (if (null *directory-stack*)
        (format T "~&The directory stack is empty!~%")
      (progn
        (psetf (first *directory-stack*) *default-pathname-defaults*
               *default-pathname-defaults* (first *directory-stack*))
        (comment-on-dir-stack)))))

(define-command (com-display-directory-stack :name "Display Directory Stack"
                                          :command-table dev-commands)
  ()
  (if (null *directory-stack*)
      (italic (T) (format T "~&The directory stack is empty!~%"))
    (dolist (pathname *directory-stack*)
      (fresh-line)
      ;(present (truename pathname))
      (pretty-pretty-pathname (truename pathname)) )))

(define-presentation-to-command-translator display-dir-stack-translator
  (directory-stack com-display-directory-stack dev-commands :gesture :select)
  () ())




;; Edit File and Show File are both horribly broken right now.

(define-command (com-edit-file :name "Edit File" :command-table dev-commands)
  ((pathname 'pathname  :prompt "pathname"))
  (clim-sys:make-process (lambda () (ed pathname))))

#+nil
(define-presentation-to-command-translator edit-file
  (clim:pathname com-edit-file dev-commands :gesture :select
		 :pointer-documentation ((object stream)
					 (format stream "Edit ~A" object))
		 :tester ((object)
			  (and (probe-file object)
			       (pathname-name object))))
  (object)
  (list object))

(define-command (com-show-file :name "Show File" :command-table dev-commands)
  ((object 'pathname :prompt "pathname"))
  (show-file object))

;; CLIM:OPEN-WINDOW-STREAM seems to be broken.
;; Less broken since I hacked on it, but still bad..
(defun show-file (pathname)
  (let ( #+ignore(*standard-output* (open-window-stream :scroll-bars :both)) )
    (with-open-file (in pathname)
      (loop for line = (read-line in nil)
	while line
	do (progn (princ line)
		  (terpri))))))

;; You have to seperate command arguments with commas..
(define-command (com-run :name "Run" :command-table dev-commands)
  ((program 'string :prompt "command")
   (args '(sequence string) :prompt "args"))
  (debugf args)
  (run-program program args :wait nil :output nil))

;;; Eval

(defun display-evalues (values)
  (with-drawing-options (T :ink +olivedrab+)
    (cond ((null values)
           (format T "No values.~%"))
          ((= 1 (length values))           
           (present (first values) 'expression)
           (fresh-line))
          (T (do ((i 0 (1+ i))
                  (item values (rest item)))
                 ((null item))               
               (with-drawing-options (T :ink +limegreen+)
                 (with-text-style (T (make-text-style nil :italic :small))
                   (format T "~A  " i)))
                 (present (first item)  'expression)
                 (fresh-line))))))

(defun shuffle-specials (form values)
  (setf +++ ++
        ++  +
        +   form
        /// //
        //  /
        /   values
        *** **
        **  *
        *   (first values)))

(define-command (com-eval :command-table dev-commands)
    ((form 'clim:form :prompt "form"))  
  (let ((values (multiple-value-list (eval form))))
    (fresh-line)
    (shuffle-specials form values)
    (display-evalues values)
    (fresh-line)))

