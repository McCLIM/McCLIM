;;; -*- Mode: Lisp; Package: DREI-CORE -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

(in-package :drei-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Misc stuff

(defun possibly-fill-line ()
  (let* ((pane (current-window))
         (buffer (buffer pane)))
    (when (auto-fill-mode pane)
      (let* ((fill-column (auto-fill-column pane))
             (point (point pane))
             (offset (offset point))
             (tab-width (tab-space-count (stream-default-view pane)))
             (syntax (syntax buffer)))
        (when (>= (buffer-display-column buffer offset tab-width)
                  (1- fill-column))
          (fill-line point
                     (lambda (mark)
                       (syntax-line-indentation mark tab-width syntax))
                     fill-column
                     tab-width
                     (syntax buffer)))))))

(defun back-to-indentation (mark syntax)
  (beginning-of-line mark)
  (loop until (end-of-line-p mark)
     while (whitespacep syntax (object-after mark))
     do (forward-object mark)))

(defun insert-character (char)
  (let* ((window (current-window))
	 (point (point window)))
    (unless (constituentp char)
      (possibly-expand-abbrev point))
    (when (whitespacep (syntax (buffer window)) char)
      (possibly-fill-line))
    (if (and (slot-value window 'overwrite-mode) (not (end-of-line-p point)))
	(progn
	  (delete-range point)
	  (insert-object point char))
	(insert-object point char))))

(defun delete-horizontal-space (mark syntax &optional (backward-only-p nil))
  (let ((mark2 (clone-mark mark)))
    (loop until (beginning-of-line-p mark)
	  while (whitespacep syntax (object-before mark))
	  do (backward-object mark))
    (unless backward-only-p
      (loop until (end-of-line-p mark2)
	    while (whitespacep syntax (object-after mark2))
	    do (forward-object mark2)))
    (delete-region mark mark2)))

(defun indent-current-line (pane point)
  (let* ((buffer (buffer pane))
         (view (stream-default-view pane))
         (tab-space-count (tab-space-count view))
         (indentation (syntax-line-indentation point
                                               tab-space-count
                                               (syntax buffer))))
    (indent-line point indentation (and (indent-tabs-mode buffer)
                                        tab-space-count))))

(defun insert-pair (mark syntax &optional (count 0) (open #\() (close #\)))
  (cond ((> count 0)
	 (loop while (and (not (end-of-buffer-p mark))
			  (whitespacep syntax (object-after mark)))
	       do (forward-object mark)))
	((< count 0)
	 (setf count (- count))
	 (loop repeat count do (backward-expression mark syntax))))
  (unless (or (beginning-of-buffer-p mark)
	      (whitespacep syntax (object-before mark)))
    (insert-object mark #\Space))
  (insert-object mark open)
  (let ((here (clone-mark mark)))
    (loop repeat count
	  do (forward-expression here syntax))
    (insert-object here close)
    (unless (or (end-of-buffer-p here)
		(whitespacep syntax (object-after here)))
      (insert-object here #\Space))))

(defun goto-position (mark pos)
  (setf (offset mark) pos))

(defun goto-line (mark line-number)
  (loop with m = (clone-mark (low-mark (buffer mark))
		       :right)
	initially (beginning-of-buffer m)
       	repeat (1- line-number)
	until (end-of-buffer-p m)
       	do (end-of-line m)
	do (incf (offset m))
	   (end-of-line m)
	finally (beginning-of-line m)
		(setf (offset mark) (offset m))))

(defun replace-one-string (mark length newstring &optional (use-region-case t))
  "Replace LENGTH objects at MARK with NEWSTRING,
using the case of those objects if USE-REGION-CASE is true."
  (let* ((start (offset mark))
	 (end (+ start length))
	 (region-case (and use-region-case
			   (buffer-region-case (buffer mark)
					       start
					       end)))) 
    (delete-range mark length)
    (insert-sequence mark newstring)
    (when (and use-region-case region-case)
      (let ((buffer (buffer mark))
	    (end2 (+ start (length newstring))))
	(funcall (case region-case
		   (:upper-case #'upcase-buffer-region)
		   (:lower-case #'downcase-buffer-region)
		   (:capitalized #'capitalize-buffer-region))
		 buffer
		 start
		 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Character case

(defun downcase-word (mark syntax &optional (n 1))
  "Convert the next N words to lowercase, leaving mark after the last word."
  (loop repeat n
     do (forward-to-word-boundary mark syntax)
     (let ((offset (offset mark)))
       (forward-word mark syntax 1 nil)
       (downcase-region offset mark))))

(defun upcase-word (mark syntax &optional (n 1))
  "Convert the next N words to uppercase, leaving mark after the last word."
  (loop repeat n
     do (forward-to-word-boundary mark syntax)
     (let ((offset (offset mark)))
       (forward-word mark syntax 1 nil)
       (upcase-region offset mark))))

(defun capitalize-word (mark syntax &optional (n 1))
  "Capitalize the next N words, leaving mark after the last word."
  (loop repeat n
     do (forward-to-word-boundary mark syntax)
     (let ((offset (offset mark)))
       (forward-word mark syntax 1 nil)
       (capitalize-region offset mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Indentation

(defun indent-region (pane mark1 mark2)
  "Indent all lines in the region delimited by `mark1' and `mark2'
   according to the rules of the active syntax in `pane'."
  (let* ((buffer (buffer pane))
         (view (clim:stream-default-view pane))
         (tab-space-count (tab-space-count view))
         (tab-width (and (indent-tabs-mode buffer)
                         tab-space-count))
         (syntax (syntax buffer)))
    (do-buffer-region-lines (line mark1 mark2)
      (let ((indentation (syntax-line-indentation
                          line
                          tab-space-count
                          syntax)))
        (indent-line line indentation tab-width))
      ;; We need to update the syntax every time we perform an
      ;; indentation, so that subsequent indentations will be
      ;; correctly indented (this matters in list forms). FIXME: This
      ;; should probably happen automatically.
      (update-syntax buffer syntax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Auto fill

(defun fill-line (mark syntax-line-indentation-function fill-column tab-width syntax
		  &optional (compress-whitespaces t))
  "Breaks the contents of line pointed to by MARK up to MARK into
multiple lines such that none of them is longer than FILL-COLUMN. If
COMPRESS-WHITESPACES is non-nil, whitespaces are compressed after the
decision is made to break the line at a point. For now, the
compression means just the deletion of trailing whitespaces."
  (let ((begin-mark (clone-mark mark)))
    (beginning-of-line begin-mark)
    (loop with column = 0
          with line-beginning-offset = (offset begin-mark)
          with walking-mark = (clone-mark begin-mark)
          while (mark< walking-mark mark)
          do (let ((object (object-after walking-mark)))
               (case object
                 (#\Space
                  (setf (offset begin-mark) (offset walking-mark))
                  (incf column))
                 (#\Tab
                  (setf (offset begin-mark) (offset walking-mark))
                  (incf column (- tab-width (mod column tab-width))))
                 (t
                  (incf column))))
             (when (and (>= column fill-column)
			(/= (offset begin-mark) line-beginning-offset))
	       (when compress-whitespaces
		 (let ((offset (buffer-search-backward
				(buffer begin-mark)
				(offset begin-mark)
				#(nil)
				:test #'(lambda (o1 o2)
					  (declare (ignore o2))
					  (not (whitespacep syntax o1))))))
		   (when offset
		     (delete-region begin-mark (1+ offset)))))
               (insert-object begin-mark #\Newline)
               (incf (offset begin-mark))
               (let ((indentation
                      (funcall syntax-line-indentation-function begin-mark)))
                 (indent-line begin-mark indentation tab-width))
               (beginning-of-line begin-mark)
               (setf line-beginning-offset (offset begin-mark))
               (setf (offset walking-mark) (offset begin-mark))
               (setf column 0))
             (incf (offset walking-mark)))))

(defun fill-region (mark1 mark2 syntax-line-indentation-function fill-column tab-width syntax
                    &optional (compress-whitespaces t))
  "Fill the region delimited by `mark1' and `mark2'. `Mark1' must be
mark<= `mark2.'"
  (let* ((buffer (buffer mark1)))
    (do-buffer-region (object offset buffer
                              (offset mark1) (offset mark2))
      (when (eql object #\Newline)
        (setf object #\Space)))
    (when (>= (buffer-display-column buffer (offset mark2) tab-width)
              (1- fill-column))
      (fill-line mark2
                 syntax-line-indentation-function
                 fill-column
                 tab-width
                 syntax
                 compress-whitespaces))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Indentation

(defgeneric indent-line (mark indentation tab-width)
  (:documentation "Indent the line containing mark with indentation
spaces. Use tabs and spaces if tab-width is not nil, otherwise use
spaces only."))

(defun indent-line* (mark indentation tab-width left)
  (let ((mark2 (clone-mark mark)))
    (beginning-of-line mark2)
    (loop until (end-of-buffer-p mark2)
       while (or (eql (object-after mark2) #\Space)
                 (eql (object-after mark2) #\Tab))
       do (delete-range mark2 1))
    (loop until (zerop indentation)
       do (cond ((and tab-width (>= indentation tab-width))
		 (insert-object mark2 #\Tab)
		 (when left             ; spaces must follow tabs
		   (forward-object mark2))
		 (decf indentation tab-width))
		(t
		 (insert-object mark2 #\Space)
		 (decf indentation))))))

(defmethod indent-line ((mark left-sticky-mark) indentation tab-width)
  (indent-line* mark indentation tab-width t))

(defmethod indent-line ((mark right-sticky-mark) indentation tab-width)
  (indent-line* mark indentation tab-width nil))

(defgeneric delete-indentation (syntax mark)
  (:documentation "Delete all indentation in the line of `mark'
with the whitespace rules of `syntax'. The default method just
removes leading whitespace characters."))

(defmethod delete-indentation ((syntax syntax) (mark mark))
  (let ((working-mark (clone-mark mark)))
    (beginning-of-line working-mark)
    (let ((end-offset (loop for offset from (offset working-mark) below (size (current-buffer))
                         for buffer-object = (buffer-object (current-buffer) offset)
                         until (char= buffer-object #\Newline)
                         unless (whitespacep syntax buffer-object)
                         return offset)))
      (when end-offset
        (delete-region working-mark end-offset)))))

(defgeneric join-line (syntax mark)
  (:documentation "Join the line that `mark' is in to the
previous line, and remove whitespace objects at the join
point. `Syntax' is used for judging what a whitespace character
is."))

(defmethod join-line ((syntax syntax) (mark mark))
  (beginning-of-line mark)
  (unless (beginning-of-buffer-p mark)
    (delete-range mark -1)
    (loop until (end-of-buffer-p mark)
       while (whitespacep syntax (object-after mark))
       do (delete-range mark 1))
    (loop until (beginning-of-buffer-p mark)
       while (whitespacep syntax (object-before mark))
       do (delete-range mark -1))
    (when (and (not (beginning-of-buffer-p mark))
	       (constituentp (object-before mark)))
      (insert-object mark #\Space))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Syntax handling

(defgeneric set-syntax (buffer syntax))

(defmethod set-syntax ((buffer drei-buffer) (syntax syntax))
  (setf (syntax buffer) syntax))

;;FIXME - what should this specialise on?
(defmethod set-syntax ((buffer drei-buffer) syntax)
  (set-syntax buffer (make-instance syntax :buffer buffer)))

(defmethod set-syntax ((buffer drei-buffer) (syntax string))
  (let ((syntax-class (syntax-from-name syntax)))
    (cond (syntax-class
	   (set-syntax buffer (make-instance syntax-class
                                             :buffer buffer)))
	  (t
	   (beep)
	   (display-message "No such syntax: ~A." syntax)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Narrowing buffers

(defgeneric invoke-with-narrowed-buffer (drei low-mark high-mark continuation &optional soft)
  (:documentation "Invoke `continuation' with the point and mark
of `drei' narrowed to the region delimited by `low-mark' and
`high-mark'. `low-mark' and `high-mark' may also be T or NIL,
meaning \"beginning/end of buffer\" (as appropriate) and
\"current position of point\", respectively."))

(defmethod invoke-with-narrowed-buffer ((drei drei)
                                        (low-mark left-sticky-mark)
                                        (high-mark right-sticky-mark)
                                        (continuation function)
                                        &optional soft)
  ;; Excessive protection, because ending up with narrowed marks is no
  ;; fun.
  (when soft
    (dolist (mark (list (point drei) (mark drei)))
      (cond ((mark> low-mark mark)
             (setf (offset mark) (offset low-mark)))
            ((mark> mark high-mark)
             (setf (offset mark) (offset high-mark))))))
  (narrow-mark (point drei) low-mark high-mark)
  (unwind-protect (progn (narrow-mark (mark drei) low-mark high-mark)
                         (unwind-protect (funcall continuation)
                           (unnarrow-mark (mark drei))))
    (unnarrow-mark (point drei))))

(defmethod invoke-with-narrowed-buffer ((drei drei)
                                        (low-mark integer)
                                        (high-mark t)
                                        (continuation function)
                                        &optional soft)
  (let ((new-low-mark (clone-mark (point drei) :left)))
    (setf (offset new-low-mark) low-mark)
    (invoke-with-narrowed-buffer drei new-low-mark high-mark continuation soft)))

(defmethod invoke-with-narrowed-buffer ((drei drei)
                                        (low-mark left-sticky-mark)
                                        (high-mark integer)
                                        (continuation function)
                                        &optional soft)
  (let ((new-high-mark (clone-mark (point drei) :right)))
    (setf (offset new-high-mark) high-mark)
    (invoke-with-narrowed-buffer drei low-mark new-high-mark continuation soft)))

(defmethod invoke-with-narrowed-buffer ((drei drei)
                                        (low-mark (eql t))
                                        (high-mark t)
                                        (continuation function)
                                        &optional soft)
  (let ((new-low-mark (clone-mark (point drei) :left)))
    (beginning-of-buffer new-low-mark)
    (invoke-with-narrowed-buffer drei new-low-mark high-mark continuation soft)))

(defmethod invoke-with-narrowed-buffer ((drei drei)
                                        (low-mark left-sticky-mark)
                                        (high-mark (eql t))
                                        (continuation function)
                                        &optional soft)
  (let ((new-high-mark (clone-mark (point drei) :right)))
    (end-of-buffer new-high-mark)
    (invoke-with-narrowed-buffer drei low-mark new-high-mark continuation soft)))

(defmethod invoke-with-narrowed-buffer ((drei drei)
                                        (low-mark null)
                                        (high-mark t)
                                        (continuation function)
                                        &optional soft)
  (let ((new-low-mark (clone-mark (point drei) :left)))
    (invoke-with-narrowed-buffer drei new-low-mark high-mark continuation soft)))

(defmethod invoke-with-narrowed-buffer ((drei drei)
                                        (low-mark left-sticky-mark)
                                        (high-mark null)
                                        (continuation function)
                                        &optional soft)
  (let ((new-high-mark (clone-mark (point drei) :right)))
    (invoke-with-narrowed-buffer drei low-mark new-high-mark continuation soft)))

(defmacro with-narrowed-buffer ((drei low-limit high-limit &optional soft) &body body)
  "Evluate `body' with the point and mark of `drei' narrowed to
the region delimited by `low-mark' and `high-mark', which may
either be a left-sticky-mark and right-sticky mark (respectively)
or two integer offsets. `low-mark' and `high-mark' may also be T
or NIL, meaning \"beginning/end of buffer\" (as appropriate) and
\"current position of point\", respectively. If `soft' is true,
point and mark will be moved to be within the narrowed buffer,
otherwise, this situation is an error."
  `(invoke-with-narrowed-buffer ,drei ,low-limit ,high-limit
                                #'(lambda ()
                                    ,@body)
                                ,soft))
