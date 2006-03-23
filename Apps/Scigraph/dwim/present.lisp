;;; -*- Syntax: Common-lisp; Package: DWIM -*-
#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(in-package :dwim)

#+clim
;;; I guess this is as good of a place as any to put this.
(make-command-table :global)

#+clim
(defun continuation-output-rectangle (continuation stream)
  ;; Repositioning the cursor seems to improve the reliability of
  ;; this computation in clim 0.9.
  #-clim-2
  (with-output-truncation (stream)
    (multiple-value-bind (x y) (stream-cursor-position* stream)
      (unwind-protect
	  (progn (stream-set-cursor-position* stream 100 100)
		 (let ((record (clim:with-output-to-output-record (stream)
				 (funcall continuation stream))))
		   (clim:bounding-rectangle record)))
	(stream-set-cursor-position* stream x y))))
  #+clim-2
  (let ((record (clim:with-output-to-output-record (stream)
		  (funcall continuation stream))))
    (clim:bounding-rectangle record)))

(defun continuation-output-size (continuation stream)
  ;;(declare (values width height))
  #FEATURE-CASE
  (((not :clim) (dw:continuation-output-size continuation stream))
   (:clim-0.9 (multiple-value-bind (width height)
		  (clim:bounding-rectangle-dimensions
		   (continuation-output-rectangle continuation stream))
		;; edges can be floats, so truncate them.
		(values (truncate width) (truncate height))))
   ((or :clim-1.0 :clim-2)
    (clim:rectangle-size
     (continuation-output-rectangle continuation stream)))))

;;;
;;; Manipulating presentations
;;;

#+clim-1.0
(progn
  (clim:define-gesture-name :left :button :left)
  (clim:define-gesture-name :middle :button :middle)
  (clim:define-gesture-name :right :button :right))

#+clim-2
(progn
  (clim:define-gesture-name :left :pointer-button :left)
  (clim:define-gesture-name :middle :pointer-button :middle)
  (clim:define-gesture-name :right :pointer-button :right))

(defun erase-graphics-presentation (presentation &key (stream *standard-output*))
  #FEATURE-CASE
  (((not :clim)
    ;; Ordinarily, graphics:erase-graphics-presentation would be the right thing.
    ;; This modification makes one slight change to the procedure
    ;; so that the entire area is cleared at once, rather than erasing
    ;; every inferior ad nauseum.
    (when (typep stream 'dw:dynamic-window)
      (labels ((eraser (p)
		 (when (typep p 'dw:presentation)
		   (dolist (inf (scl:symbol-value-in-instance p 'dw::inferiors))
		     (eraser inf))
		   (setf (scl:symbol-value-in-instance p 'dw::inferiors) nil))
		 ;; delete from coordinate sorted set of window:
		 (if (typep p 'dw::text-displayed-presentation)
		     (dw::erase-displayed-presentation p stream t nil t)
		   (send stream :delete-graphics-displayed-presentation p))))
	(eraser presentation))
      ;; Delete from superior's list of inferiors:
      (send stream :delete-displayed-presentation presentation)
      ;; Clear the rectangle and redraw overlapping presentations:
      (send stream :redraw-inside-sets
	    (dw:presentation-displayed-box presentation) t)))
   ((or :clim-0.9 :clim-1.0)
    ;; If clim can't find the presentation, it signals an error.
    ;; As far as I'm concerned, if it can't find it, it's as good as erased.
    (multiple-value-bind (value errorp)
	(ignore-errors (clim:erase-output-record presentation stream))
      (if errorp
	  ;; If an error happened, the display is probably screwed up.
	  ;; So beep to admit responsibility and try somehow to recover.
	  (progn #+ig (beep)
		 (clim:output-recording-stream-replay stream presentation))
	value)))
   (:clim-2
    (clim:erase-output-record presentation stream nil))))

(defun presentation-under-pointer (stream)
  #FEATURE-CASE
  (((not :clim) (scl:send stream :last-highlighted-presentation))
   (:clim-0.9 (ci::highlighted-presentation stream))
   (:clim-1.0 (multiple-value-bind (x y) (clim:stream-pointer-position* stream)
		(clim::find-innermost-applicable-presentation 
		 '((t)) stream x y)))
   (:clim-2 (multiple-value-bind (x y) (clim:stream-pointer-position stream)
		(clim::find-innermost-applicable-presentation '((t)) stream x y)))))

(defun presentation-p (object)
  #FEATURE-CASE
  (((not :clim) (typep object 'dw::presentation))
   (:clim-0.9 (ci::presentation-p object))
   ((or :clim-1.0 :clim-2) (typep* object 'clim:presentation))))

(defun presentation-superior (presentation)
  #+clim (clim:output-record-parent presentation)
  #-clim (dw:presentation-superior presentation))

(defun presentation-object (presentation)
  #-clim (dw:presentation-object presentation)
  #+clim (if (presentation-p presentation) (clim:presentation-object presentation)))

(defun presentation-subtypep (subtype supertype)
  #-clim (dw:presentation-subtypep subtype supertype)
  #+clim (clim:presentation-subtypep subtype supertype))

(defun presentation-type-p (type)
  #-clim (dw:presentation-type-p type)
  #+clim (clim:presentation-type-specifier-p type))

(defun describe-presentation-type (type &optional (stream *standard-output*))
  #-clim (dw:describe-presentation-type type stream)
  #+clim (clim:describe-presentation-type type stream))

(defun bounding-rectangle* (presentation)
  "Get the bounding edges of the presentation."
  ;;(declare (values left top right bottom))
  #FEATURE-CASE
  (((not :clim)
    (when (presentation-p presentation)
      (let ((box (dw:presentation-displayed-box presentation)))
	(if box (dw:box-edges box)))))
   (:clim-0.9
    (multiple-value-bind (left top right bottom)
	(clim:bounding-rectangle* presentation)
      ;; edges can be floats, so truncate them.
      (values (truncate left) (truncate top) (truncate right) (truncate bottom))))
   ((or :clim-1.0 :clim-2)
    (let ((stream *standard-output*))
      ;; Seem to need to know the stream under the presentation.
      ;; Take a wild guess.
      (multiple-value-bind (xoff yoff)
	  (#-mcclim clim::convert-from-relative-to-absolute-coordinates
	   #+mcclim climi::convert-from-relative-to-absolute-coordinates
	   stream
	   (clim::output-record-parent presentation))
	(clim:with-bounding-rectangle*
	    (left top right bottom) presentation
	    (values (+ left xoff) (+ top yoff) (+ right xoff) (+ bottom yoff))))))))

(defun redisplay (record stream)
  #FEATURE-CASE
  (((not :clim) (dw:do-redisplay record stream :truncate-p nil))
   ((or :clim-1.0 :clim-2) (clim:redisplay record stream :check-overlapping nil))))

(defun redisplayable? (stream)
  #FEATURE-CASE
  ((:clim-2 (clim:redisplayable-stream-p stream))
   (:clim-1.0 (clim:stream-redisplayable-p stream))
   (:clim-0.9 (clim-internals::stream-redisplayable-p stream))
   ((not :clim) stream)))

(defun redisplayable-format (stream string &rest args)
  (if (eq stream 't) (setq stream *standard-output*))
  (if (redisplayable? stream)
      (let ((a (copy-list args)))
	(with-redisplayable-output (:stream stream
					    :unique-id string
					    :cache-value a
					    :cache-test #'equal)
	  (apply #'format stream string a)))
      (apply #'format stream string args)))

(defun accept (presentation-type &key
				 (view nil view-p)
				 (stream *standard-output*)
				 (prompt #+clim t #-clim :enter-type)
				 default query-identifier)
  #FEATURE-CASE
  (((not :clim) (dw:accept presentation-type
			   :stream stream
			   :prompt prompt
			   :default default
			   :query-identifier query-identifier
			   :newline-after-query nil))
   (:clim-0.9
    (clim:accept presentation-type
		 :stream stream
		 :prompt prompt
		 :default default
		 :query-identifier query-identifier))
   ((or :clim-1.0 :clim-2)
    (if view-p
	(clim:accept presentation-type
		     :view view
		     :stream stream
		     :prompt prompt
		     :default default
		     :display-default nil
		     :query-identifier query-identifier)
      (clim:accept presentation-type
		   :stream stream
		   :prompt prompt
		   :default default
		   :display-default nil
		   :query-identifier query-identifier)))))

#+(and clim-1.0 (not mcl))
(progn
  ;;CLIM additions to the input editor to conform to my old EMACs ways
  (clim::add-input-editor-command #\meta-< 'clim::com-ie-beginning-of-buffer)
  (clim::add-input-editor-command #\meta-> 'clim::com-ie-end-of-buffer)
  (clim::add-input-editor-command #\meta-\b 'clim::com-ie-backward-word)
  (clim::add-input-editor-command #\meta-\f 'clim::com-ie-forward-word)
  (clim::add-input-editor-command #\meta-rubout 'clim::com-ie-rubout-word)
  (clim::add-input-editor-command #\meta-\d 'clim::com-ie-delete-word)
  (clim::add-input-editor-command #\control-meta-\y 'clim::com-ie-history-yank)
  (clim::add-input-editor-command #\meta-\y 'clim::com-ie-yank-next)
  (clim::add-input-editor-command #\meta-\v 'clim::com-ie-scroll-backward))


;;;Alas there is no <End> key so let's fake it out with the <control-d> key
#+(and clim-1.0 (not genera) (not mcl))
(clim::define-accept-values-command (com-exit-avv :keystroke #\control-\d) ()
  (invoke-restart 'clim::frame-exit))

(defun accept-values (descriptions &key (prompt nil)
					(stream *query-io*)
					(own-window nil))
  (values-list
   (accepting-values 
    (stream :own-window own-window :label prompt)
    (mapcar #'(lambda (description)
		(destructuring-bind (type &rest options)
		    description
		  (prog1 (apply #'accept type :stream stream
				:query-identifier
				(getf options :query-identifier (car description))
				options)
		    (terpri stream))))
	    descriptions))))

(defun menu-choose (choices
		    &key (prompt "Choose:")
		         default-choice)
  #FEATURE-CASE
  ((:clim-0.9
    (prog1 (clim:menu-choose choices
			     :label prompt
			     :associated-window *standard-input*
			     :default-item default-choice
			     )
      ;; kludge city.  menu-lose leaves your mouse click in the buffer.
      (stream-clear-input *standard-input*)))
   (:clim-1.0
    (clim:menu-choose choices
		      :associated-window *standard-input*
		      :label prompt :default-item default-choice))
   (:clim-2
    (clim:menu-choose choices
		      :associated-window *standard-input*
		      :label prompt :default-item default-choice))
   ((not :clim) (dw:menu-choose choices :prompt prompt))))

;;;
;;; formatting table etc.
;;;

(defmacro formatting-table ((stream &key (inter-column-spacing 8)) &body body)
  #FEATURE-CASE
  (((not :clim)
    `(dw:formatting-table (,stream :dont-snapshot-variables t
				   :inter-column-spacing ,inter-column-spacing)
			  ,@body))
   (:clim-0.9 `(clim:formatting-table (,stream) ,@body))
   (:clim-1.0 `(clim:formatting-table
		(,stream :inter-column-spacing ,inter-column-spacing)
		,@body))
   (:clim-2 `(clim:formatting-table
		(,stream :x-spacing ,inter-column-spacing)
		,@body))))

(defmacro formatting-row ((stream) &body body)
  #-clim `(dw:formatting-row (,stream :dont-snapshot-variables t) ,@body)
  #+clim `(clim:formatting-row (,stream) ,@body))

(defmacro formatting-column ((stream) &body body)
  #-clim `(dw:formatting-column (,stream :dont-snapshot-variables t) ,@body)
  #+clim `(clim:formatting-column (,stream) ,@body))

(defmacro formatting-column-headings ((stream &key (underline-p nil)) &body body)
  #-clim `(dw:formatting-column-headings
	    (,stream :underline-p ,underline-p :dont-snapshot-variables t)
	    ,@body)
  ;; dont have the time to do this one justice.
  #+clim (if underline-p
	     `(formatting-row (,stream) (with-underlining (,stream) ,@body))
	     `(formatting-row (,stream) ,@body)))

(defmacro formatting-cell ((stream &key (align-x :left) align-y) &body body)
  #+clim `(clim:formatting-cell (,stream ,@(if align-x `(:align-x ,align-x))
					 ,@(if align-y `(:align-y ,align-y)))
				 ,@body)
  #-clim `(dw:formatting-cell (,stream :align-x ,align-x :align-y ,align-y)
			       ; :dont-snapshot-variables t ;;++This seems to not work
	    ,@body))

(defmacro formatting-item-list ((stream &rest options) &body body)
  #+clim `(clim:formatting-item-list (,stream ,@options) ,@body)
  #-clim `(dw:formatting-item-list (,stream ,@options) ,@body))

(defmacro format-item-list (list &rest keys)
  #-clim `(dw:format-item-list ,list ,@keys)
  #+clim
  (let ((stream (or (second (member :stream keys)) t)))
    `(formatting-item-list (,stream)
       (dolist (item ,list)
	 (formatting-cell (,stream)
	   (format ,stream "~A" item))))))

;;;
;;; Presentation parser primitives
;;;

(defun read-char-for-accept (stream)
  #+clim (read-char stream nil nil)
  #-clim
  (let ((char (loop thereis (send stream :any-tyi)
		    until (not (interactive-stream-p stream )))))	;bug in IE
    (cond ((listp char) char)
	  #+dw-is-brain-damaged
	  ((accept-blip-p char) (list ':accept char nil))
	  (t char))))

(defun unread-char-for-accept (char stream)
  #FEATURE-CASE
  (((or :clim-0.9 :clim-1.0)
    (if (clim::activation-character-p char)
	;; unreading an activation character causes problems for stream-unread-gesture.
	(clim:with-activation-characters (nil :override t) (unread-char char stream))
      (unread-char char stream)))
   (:clim-2
    (if (clim:activation-gesture-p char)
	;; unreading an activation character causes problems for stream-unread-gesture.
	(clim:with-activation-gestures (nil :override t) (unread-char char stream))
      (unread-char char stream)))
   ((not :clim)
    (dw:unread-char-for-accept char stream))))

(defun peek-char-for-accept (stream &optional hang)
  (let ((ch (and (or hang
		     (not (interactive-stream-p stream))
		     (< (input-position stream) (insertion-pointer stream)))
		 (read-char-for-accept stream))))
    (when ch
      (unread-char-for-accept ch stream))
    ch))

(defun compare-char-for-accept (char-from-accept comparandum)
  (and char-from-accept
       (typecase char-from-accept
	 (character (char-equal comparandum char-from-accept))
	 (list
	   ;; this should only happen in DW.
	   (and (member (first char-from-accept) '(:activation :blip-character :accept))
		(characterp (second char-from-accept))
		(char-equal comparandum (second char-from-accept)))))))

(defun read-token (stream)
  #+clim (clim:read-token stream)
  #-clim (dw:read-standard-token stream))

(defun input-position (stream)
  ;; This location identifies the current position of the parser in a (buffered)
  ;; input stream.  When a character gets read by read-char-for-accept, this pointer
  ;; gets incremented.  Upon failure, the parser backtracks by decrementing it.
  #FEATURE-CASE
  (((not :clim) (scl:send stream :read-location))
   (:clim-0.9 (clim::input-position stream))
   (:clim-1.0 (if (clim:extended-input-stream-p stream)
		 (clim::input-position stream)
	       (file-position stream)))
   (:clim-2 (if (clim:input-editing-stream-p stream)
		  (clim:stream-scan-pointer stream)
		(file-position stream)))))

(defmethod (setf input-position) (new stream)
  #FEATURE-CASE
  (((not :clim) (scl:send stream :set-location new))
   (:clim-1.0 (if (clim:extended-input-stream-p stream)
		  (setf (clim::input-position stream) new)
		  (file-position stream new)))
   (:clim-2 (if (clim:input-editing-stream-p stream)
		  (setf (clim:stream-scan-pointer stream) new)
		  (file-position stream new)))))

(defun insertion-pointer (stream)
  (cond ((interactive-stream-p stream)
	 #FEATURE-CASE
	 (((not :clim) (scl:send stream :typein-location))
	  (:clim-0.9 (ci::insertion-pointer stream))
	  (:clim-1.0 (clim::insertion-pointer stream))
	  (:clim-2 (clim:stream-insertion-pointer stream))))
	(t				; string-input-stream
	 #FEATURE-CASE
	 ((:genera (scl:send stream :length))
	  (:allegro (slot-value stream 'excl::buffer-ptr))))))

(defvar *%%ready-to-catch%%* nil)

(defmacro catching-parser-failures (form &rest failure-actions)
  "Use this to catch a parser failure and do something about it."
  (let ((normal (gensym)))
    `(let ((*%%ready-to-catch%%* t))
       (catch ',normal
	 (catch 'catch-parser-failures
	   (handler-bind ((error #'(lambda (error)
				     #+clim-2
				     (when (and (typep error 'clim:abort-gesture)
						(find-restart 'abort))
				       (invoke-restart 'abort))
				     (throw 'catch-parser-failures t))))
	     (throw ',normal ,form)))
	 ,@failure-actions))))

(defun input-not-of-required-type (stream object type)
  "Use this to signal a parser failure and cause backtracking."
  (declare (ignore stream))
  ;; Used by faes expression editor.  Don't use the one from clim or dw,
  ;; it's so fancy that it outsmarts itself.
  (if *%%ready-to-catch%%* (throw 'catch-parser-failures t))
  #FEATURE-CASE
  (((or clim-1.0 clim-2)
    (if (eq object :failure)
	(parse-error "The input read was not of the required type.")
      (clim:input-not-of-required-type object type)))
   (clim-0.9 (clim:input-not-of-required-type stream object type))
   ((not clim) (zl:parse-ferror "The input read, ~A, was not ~A" object type))))

(defun parse-error (format-string &rest args)
  #FEATURE-CASE
  (((or :clim-0.9 :clim-1.0) (apply #'clim::parse-error format-string args))
   (:clim-2 (apply #'clim:simple-parse-error format-string args))
   ((not :clim) (apply #'sys::parse-error format-string args))))

(defun validate-object (object ptype)
  #FEATURE-CASE
  (((and (not :clim) :slow) (dw::ptypep object ptype))
   ((not :clim)
    ;; This is at least 6x faster than dw::ptypep.  I hope it works as well.
    (let ((name (cond ((symbolp ptype) ptype)
		      ((symbolp (car ptype)) (car ptype))
		      (t (caar ptype)))))
      (cond ((member name '(t sys:expression)) t)
	    ((member name '(and or not))
	     (return-from validate-object
	       (typep object (if (or (symbolp ptype) (symbolp (car ptype)))
				 ptype
			       (car ptype)))))
	    ((dw::with-type-descriptor ((type-desc expanded-type) ptype :exact-only t)
	       (when type-desc
		 (dw:with-type-decoded (type-sym type-dargs) expanded-type
		      (declare (ignore type-sym))
		      (let* ((typep-function
			      (dw::presentation-type-descriptor-typep-function type-desc))
			     (predicate
			      (when typep-function
				(dw::compute-type-predicate typep-function type-dargs nil))))
			(return-from validate-object
			  (if predicate (funcall predicate object) t)))))))
	    ((multiple-value-bind (flavor-or-class structure-p typep)
		 (dw::symbol-flavor-or-cl-type name)
	       (when (or flavor-or-class structure-p typep)
		 (return-from validate-object
		   (typep object (if (or (symbolp ptype) (symbolp (car ptype)))
				     ptype
				   (car ptype)))))))
	    (t t))))
   (:clim-0.9 (ci::validate-object object ptype))
   ((or :clim-1.0 :clim-2)
    (let ((p (clim:expand-presentation-type-abbreviation ptype)))
      (clim::presentation-typep object p)))))

(defmacro with-accept-activation-chars ((additional-characters &key override) &body body)
  #FEATURE-CASE
  (((not :clim)
    `(dw:with-accept-activation-chars (,additional-characters :override ,override)
       ,@body))
   ((or :clim-0.9 :clim-1.0)
    `(clim:with-activation-characters (,additional-characters :override ,override)
       ,@body))
   (:clim-2
    `(clim:with-activation-gestures (,additional-characters :override ,override)
      ,@body))))

(defun accept-activation-p (char &optional
			    (achars #-clim dw::*accept-activation-chars*
				    #+clim-0.9 nil
				    #+clim-1.0 clim:*activation-characters*))
  (declare (ignore bchars))
  (declare (ignorable achars))
  #FEATURE-CASE
  ((:clim-0.9 (clim::activation-character-p char))
   (:clim-2 (clim:activation-gesture-p char))
   ((or :clim-1.0 :clim-2 (not :clim))
    (and
     (if (consp char) (setq char (second char)) t)
     (dolist (l achars nil)
       (if (member char l) (return t)))))))

(defmacro with-accept-blip-chars ((additional-characters &key override) &body body)
  #FEATURE-CASE
  (((not :clim)
    `(dw:with-accept-blip-chars (,additional-characters :override ,override) ,@body))
   ((or :clim-0.9 :clim-1.0)
    `(clim:with-blip-characters (,additional-characters :override ,override) ,@body))
   (:clim-2
    `(clim:with-delimiter-gestures (,additional-characters :override ,override) ,@body))))

(defun accept-blip-p (char &optional (chars #-clim dw::*accept-blip-chars*
					    #+clim-1.0 clim:*blip-characters*
					    #+clim-0.9 nil))
  (declare (ignorable chars))
  #FEATURE-CASE
  ((:clim-0.9 (clim::blip-character-p char))
   (:clim-2 (clim:delimiter-gesture-p char))
   ((or :clim-1.0 :clim-2 (not :clim))
    (loop for l in chars
	thereis (and (characterp char) (member char l :test #'char-equal))))))

(defmacro with-activation-characters ((additional-characters &key override) &body body)
  `(with-accept-activation-chars (,additional-characters :override ,override) ,@body))

(defmacro with-blip-characters ((additional-characters &key override) &body body)
  `(with-accept-blip-chars (,additional-characters :override ,override) ,@body))

(defmacro completing-from-suggestions
	  ((stream &key delimiters allow-any-input
		   (initially-display-possibilities nil idp)
		   (type nil typep))
	   &body body)
  #+clim (declare (ignore initially-display-possibilities type))
  #-clim (declare (ignore idp typep))
  #-clim `(dw:completing-from-suggestions
	    (,stream
	     :allow-any-input ,allow-any-input :delimiters ,delimiters :type ,type
	     :initially-display-possibilities ,initially-display-possibilities)
	    ,@body)
  #+clim
  (progn
    (if idp (format t "~% completing-from-suggestions :initially-display-possibilities not supported."))
    (if typep (format t "~% completing-from-suggestions :type not supported."))
    `(clim:completing-from-suggestions
	 (,stream :partial-completers ,delimiters :allow-any-input ,allow-any-input)
       ,@body)))

(eval-when (compile load eval)
  (import #-clim 'dw::suggest
	  #+clim 'clim:suggest
	  'dwim))

(defun complete-from-sequence (sequence stream &key type (name-key #'string))
  #+clim (declare (ignore type))
  (completing-from-suggestions (stream #-clim :type #-clim type)
    (map nil #'(lambda (elt) (suggest (funcall name-key elt) elt)) sequence)))

;;; JPM.  This isn't really portable because it generates a
;;; DWish blip object.  Use with-input-context below, if you can.
(defmacro with-presentation-input-context
	  ((PRESENTATION-TYPE &rest OPTIONS)
	   (&optional (BLIP-VAR   '.BLIP.))
	   NON-BLIP-FORM
	   &body CLAUSES)
  #+Genera  (declare (zwei:indentation 0 2 2 4 3 2))
  #-clim
  `(dw:with-presentation-input-context
      (,PRESENTATION-TYPE ,@OPTIONS)
      (,BLIP-VAR)
	,NON-BLIP-FORM
      ,@CLAUSES)
  #+clim
  `(clim:with-input-context (,PRESENTATION-TYPE :override ,(getf options :inherit))
			    (.object. .presentation-type. .gesture.)
	,NON-BLIP-FORM
	,@(mapcar #'(lambda (clause)
		      `(,(first clause)
			(let ((,blip-var (list .presentation-type.
					       .gesture.
					       .object.)))
			  ,blip-var
			  ,@(rest clause))))
		  CLAUSES)))

(defmacro with-input-context
	  ((PRESENTATION-TYPE &key OVERRIDE STREAM)
	   (&optional (OBJECT-VAR '.object.)
		      (PT-VAR     '.presentation-type.)
		      (GESTURE-VAR   '.gesture. gesture-p))
	   NON-BLIP-FORM
	   &body CLAUSES)
  #+Genera (declare (zwei:indentation 0 2 2 4 3 2))
  #-clim
  (let ((blip-var '.blip.))
    `(dw:with-presentation-input-context
	(,PRESENTATION-TYPE :inherit (not ,override) :stream ,stream)
	(,BLIP-VAR)
	  ,NON-BLIP-FORM
	,@(mapcar #'(lambda (clause)
		      `(,(first clause)
			(let ((,object-var (dw:presentation-blip-object ,blip-var))
			      (,pt-var (dw:presentation-blip-presentation-type
					 ,blip-var))
			      (,gesture-var (dw:presentation-blip-mouse-char
					      ,blip-var)))
			  (and ,object-var ,pt-var ,gesture-var)
			  ,@(rest clause))))
		  clauses)))
  #+clim (declare (ignore stream))
  #+clim
  `(clim:with-input-context (,PRESENTATION-TYPE :override ,OVERRIDE)
			    (,OBJECT-VAR ,PT-VAR ,@(if gesture-p GESTURE-VAR))
	,NON-BLIP-FORM
      ,@CLAUSES))

;;;
;;; Presentation types
;;;

(define-presentation-type sheet ()
   :parser ((stream)
	    #+clim
	    (progn
	      (read-char stream)
	      (error "The SHEET presentation type is broken.  Sorry."))
	    #-clim (dw:accept 'tv:sheet :stream stream :prompt nil))
   :printer ((window stream)
	     (let ((*print-readably* nil))
	       #-ansi-cl (declare (special *print-readably*))
	       #+clim (format stream "~A" window)
	       #-clim (present window 'tv:sheet :stream stream)))
   :description "a window")

#-clim
(eval-when (compile load eval)
  (import 'dw:alist-subset 'dwim))

#+(or clim-1.0 clim-2)
(clim:define-presentation-type-abbreviation alist-member (&key alist (test 'eql))
  `(clim:member-alist ,alist :test ,test))

(defun menu-execute-no-side-effects (item)
  (cond ((atom item) item)
	((atom (cdr item)) (cdr item))
	((atom (cddr item)) (cadr item))
	((eq (second item) :value) (third item))))

(defun token-element-string (element)
  (typecase element
    (null (symbol-name element))		
    (cons (string (first element)))
    (symbol (string-capitalize (symbol-name element)))
    (string element)
    (otherwise (present-to-string element))))

(defun make-accept-values-choices (&key query-identifier sequence select-action)
  (declare (ignorable sequence))
  #FEATURE-CASE
  ((:clim-2
	      (clim-internals::make-accept-values-multiple-choices
	       :query-identifier query-identifier
	       :select-action select-action))
   (:clim-1.0 
	      (clim::make-accept-values-multiple-choices
	       :query-identifier query-identifier
	       :select-action select-action))
   (:clim-0.9 (ci::make-accept-values-choices
	       :query-identifier query-identifier
	       :sequence sequence
	       :select-action select-action))
   ((not :clim) (dw::make-accept-values-choices
		 :query-identifier query-identifier
		 :sequence sequence
		 :select-action select-action))))

(defun make-accept-values-choice (&key choices choice value documentation)
  #+clim (declare (ignore documentation))
  (declare (ignorable choice))
  #FEATURE-CASE
  ((:clim-2 
	      (clim-internals::make-accept-values-multiple-choice
	       :choices choices
	       :value value))
   (:clim-1.0
	      (clim::make-accept-values-multiple-choice
	       :choices choices
	       :value value))
   (:clim-0.9 (ci::make-accept-values-choice
	       :choices choices
	       :choice choice
	       :value value))
   ((not :clim) (dw::make-accept-values-choice
		 :choices choices
		 :choice choice
		 :value value
		 :documentation documentation))))

(defun type-for-avv-choice ()
  #FEATURE-CASE
  ((:clim-2 'clim-internals::accept-values-one-of)
   (:clim-1.0 'clim::accept-values-one-of)
   (:clim-0.9 'ci::accept-values-choice-display)
   ((not :clim) 'dw::accept-values-choice-display)))

(defun accept-values-choose-from-sequence
    (stream sequence selected-value query-identifier
     &key
     drawer select-action
     n-columns n-rows
     (selection-test #'eq)
     (value-key #'menu-execute-no-side-effects)
     (name-key #'token-element-string)
     (choice-type (type-for-avv-choice))
     (make-choices #'make-accept-values-choices)
     (make-choice #'make-accept-values-choice))
  "Used for the ACCEPT-VALUES method of some presentation types."
  ;; This is how ALIST-MEMBER works.
  ;; DRAWER: how to draw an element in the sequence.
  ;; SELECT-ACTION: how to combine the selected choice with the default value.
  (when (not drawer)
    (setq drawer
      #'(lambda (str obj name selected)
	  (declare (ignore obj))
	  (formatting-cell (str)
			   (if selected
			       (with-text-face (:bold str) (write-string name str))
			     (write-string name str))))))
  (when (not select-action)
    (setq select-action
      #'(lambda (choice default-val)
	  (declare (ignore default-val))
	  choice)))
  (let ((choices (funcall make-choices
			  :query-identifier query-identifier
			  :sequence sequence
			  :select-action select-action))
	;;(width (- (stream-viewport-size stream) (stream-cursor-position* stream)))
	)
    (labels ((draw-one (item value pretty-name selected-p stream)
	       (with-output-as-presentation
		   (:type choice-type
			  :stream stream
			  :object (funcall make-choice
					   :choices choices
					   :choice item
					   :value value
					   :documentation pretty-name)
			  :single-box t)
		 #+clim-0.9
		 (formatting-cell (stream)
		   (funcall drawer stream value pretty-name selected-p))
		 #+(not clim)
		 (formatting-cell (stream)
		      (funcall drawer stream value pretty-name selected-p))
	         #+(or clim-1.0 clim-2)
		 (clim:with-room-for-graphics (stream)
		     (formatting-cell (stream)
		        (funcall drawer stream value pretty-name selected-p)))))
	     (draw-all (sequence stream)
	       (dolist (item sequence)
		 (let* ((value (funcall value-key item))
			(pretty-name (funcall name-key item))
			(selected-p (funcall selection-test value selected-value)))
		   (draw-one item value pretty-name selected-p stream)))))
      (with-output-as-presentation (:stream stream :single-box t)
	(formatting-item-list
	 (stream :n-columns n-columns
		 :n-rows n-rows
		 ;;#+clim :max-width #-clim :inside-width width
		 #-clim-2 :inter-column-spacing #+clim-2 :x-spacing
		 '(2 :character))
	 (draw-all sequence stream))))))

#+clim
(define-presentation-type alist-subset (&key alist)
  ;; Yes, I know clim 1.0 has one of these, but it doesn't work in avv mode!.
  :parser ((stream)
	   (accept `(sequence (alist-member :alist ,alist))
		   :stream stream :prompt nil))
  :printer ((object stream)
	    (do ((sequence object (cdr sequence)))
		((null sequence))
	      (let ((element (find (first sequence) alist
				   :key #'menu-execute-no-side-effects)))
		(write-string (token-element-string element) stream))
	      (unless (= (length sequence) 1)
		(write-string ", " stream))))
  :typep ((object)
	  (block testem
	    (dolist (element object)
	      (or (find element alist :key #'menu-execute-no-side-effects)
		  (return-from testem nil)))
	    t))
  :describer ((stream)
	      (write-string "any of " stream)
	      ;; -- CLIM doesn't have a general list formatter yet
	      (let (length)
		(do ((rest-of-elements alist (rest rest-of-elements)))
		    ((not rest-of-elements))
		  (setq length (length rest-of-elements))
		  (format stream "~A" (token-element-string (car rest-of-elements)))
		  (cond ((> length 2)
			 (write-string ", " stream))
			((= length 2)
			 (write-string " or " stream))))))
  :accept-values-displayer
  ((stream object query-identifier)
   ;; OBJECT is the currently chosen subset.
   ;; OBJECT is the currently chosen subset.
   (accept-values-choose-from-sequence
     stream alist object query-identifier
     :select-action
     #'(lambda (new list)
	 (cond ((not (listp list)) (list new))
	       ((member new list)  (remove new list))
	       (t (adjoin new list))))
     :selection-test #'member
     :drawer
     #'(lambda (stream object name selected-p)
	 (declare (ignore object))
	 (if selected-p
	     (with-character-face (:bold stream)
	       (format stream "~A" name))
	     (format stream "~A" name))))))

(defun all-characters (&optional (n (1+ (char-code #\rubout))))
  (let ((list nil))
    (dotimes (i n) (push (code-char i) list))
    list))

(defvar *all-characters* nil)

(defun readline-no-echo (stream)
  #FEATURE-CASE
  ((:clim-2 
    (clim:with-output-recording-options (stream :draw nil :record nil)
      (accept 'string :stream stream :prompt nil :default nil)))
   (:clim-1.0
    (let ((clim::*disable-input-editor-echo* t))
      (declare (ignorable clim::*disable-input-editor-echo*))
      ;; This variable is defined in a patch file (echo-patch.lisp)
      ;; that came from Scott MacKay and has not been made a part of DWIM.
      ;; You must load it separately.
      (accept 'string :stream stream :prompt nil :default nil)))
   ((not :clim)
    (let ((all-characters (or *all-characters*
			      (setq *all-characters* (all-characters))))
	  (return (elt (format nil "~%") 0)))
      ;; The trick to echo suppression is to define every character as an
      ;; activation character.
      (with-accept-activation-chars (all-characters :override t)
	(let ((line (make-array 1 :fill-pointer 0
				:adjustable t
				:element-type #+genera 'string-char
				#-genera 'character)))
	  (loop
	    (let ((char (read-char-for-accept stream)))
	      (if (consp char) (setq char (second char)))
	      (cond ((eql char return)
		     (return (values line char)))
		    ((eql char #\rubout)
		     (if (zerop (fill-pointer line))
			 (beep)
		       (decf (fill-pointer line))))
		    ((not (characterp char))
		     (beep))
		    (t
		     (vector-push-extend char line)))))))))))

;;; A hack so the user doesnt have to see some ugly commands get echoed.
;;; Also seems like a useful way to read a password.
(define-presentation-type invisible-object ()
  :parser ((stream)
	   (values (readline-no-echo stream) 'invisible-object))
  :printer ((object stream)
            (declare (ignore object))
	    (write-string "*" stream)))
