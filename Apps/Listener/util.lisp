(in-package :clim-listener)

;;; Miscellaneous utilities, UI tools, gross hacks, and non-portable bits.

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



(defun filtermap (list func &optional (filter #'null))
  (declare (type (function (t) t) func))
  (delete-if filter (mapcar func list)))

;(defmacro multiple-value-prog2 (&body body)  `(progn ,(first body) (multiple-value-prog1 ,@(rest body))))

;; multiple-value-or, ugh. Normal OR drops values except from the last form.
(defmacro mv-or (&rest forms)
  (if (null forms)  nil
    (let ((tmp (gensym)))
      `(let ((,tmp (multiple-value-list ,(first forms))))
         (if (first ,tmp) (values-list ,tmp) (mv-or ,@(rest forms)))))))


;; DEBUGF is useful, I can sleep better knowing it's in the image.
(defmacro debugf (&rest stuff)
  `(progn (fresh-line *trace-output*)
     ,@(reduce #'append 
                  (mapcar #'(lambda (x)                              
                              (cond
                                ((stringp x) `((princ ,x *trace-output*)))
                                (T `((princ ',x *trace-output*)
                                     (princ "=" *trace-output*)
                                     (write ,x :stream *trace-output*)
                                     (princ #\space *trace-output*)))))

                          stuff))
     (terpri *trace-output*)))


; There has to be a better way..
(defun directoryp (pathname)
  "Returns pathname when supplied with a directory, otherwise nil"
  (if (or (pathname-name pathname) (pathname-type pathname))
      nil
      pathname))

(defun getenv (var)
  (or 
   #+cmu (cdr (assoc var ext:*environment-list*))
   #+sbcl (sb-ext:posix-getenv var)
   #+lispworks (lw:environment-variable var)
   #+openmcl (ccl::getenv var)
   nil))

;; Need to strip filename/type/version from directory?.. FIXME?
(defun change-directory (pathname)
  "Ensure that the current directory seen by RUN-PROGRAM has changed, and update *default-pathname-defaults*"
  #+CMU (unix:unix-chdir (namestring pathname))
  ; SBCL FIXME?
 (setf *default-pathname-defaults* pathname))

(defun resolve-stream-designator (desi default)
  (if (eq desi t)
      default
    (or desi default)))

;;; LIST-DIRECTORY is a wrapper for the CL DIRECTORY function, which really doesn't
;;; do what I'd like (resolves symbolic links, tends to be horribly buggy, etc.)

#+CMU
(defun list-directory (pathname)
  (directory pathname :truenamep nil))


#+SBCL
(defun sbcl-frob-to-pathname (pathname string)
  "This just keeps getting more disgusting."
  (let* ((parent (strip-filespec pathname))
        (pn (merge-pathnames (make-pathname :name (subseq string 0 (position #\. string :start 1 :from-end T))
                                            :type (let ((x (position #\. string :start 1 :from-end T)))
                                                     (if x (subseq string (1+ x)) nil)))
                              parent))
         (dir (ignore-errors (sb-posix:opendir (namestring pn)))))    
    (cond ((or (string= string ".")
               (string= string ".."))
           nil)
          ((or (null dir)
               (sb-alien:null-alien dir))
           pn)
          (T (merge-pathnames (parse-namestring (concatenate 'string string "/"))
                              parent)))))

;; FIXME: SB-POSIX now has STAT, so USE IT HERE !!!

#+SBCL
(defun list-directory (pathname)
  (let* ((pathname (strip-filespec pathname)) ;; ugh.
         (dir (sb-posix:opendir pathname))
         (list nil))
    (loop
      (let ((dirent (sb-posix:readdir dir)))
        (unwind-protect
            (if (sb-alien:null-alien dirent)
                (return-from list-directory
                  (nreverse list))
              (let ((pn (sbcl-frob-to-pathname pathname (sb-posix::dirent-name dirent))))
                (when pn (push pn list))))
          (sb-posix::free-dirent dirent))))))

#+openmcl
(defun list-directory (pathname)
  (directory pathname :directories t :follow-links nil))

;; Fallback to ANSI CL
#-(OR CMU SBCL OPENMCL)
(defun list-directory (pathname)
  (directory pathname))


;;; A farce of a  "portable" run-program, which grows as I need options from
;;; the CMUCL run-program.
;;; This ought to change the current directory to *default-pathname-defaults*..
;;; (see above)

(defun run-program (program args &key (wait T) (output *standard-output*) (input *standard-input*))    
  #+CMU (ext:run-program program args :input input
                                       :output output :wait wait)

  #+SBCL (sb-ext:run-program program args :input input :search T
                                          :output output :wait wait)
  #+lispworks (system:call-system-showing-output       ; Contributed by Neonsquare.
               (format nil "~A~{ ~A~}" program args)   ; I am uneasy about shell quoting issues here..
               :shell-type "/bin/sh"
               :output-stream output
               :wait wait)

  #-(or CMU SBCL lispworks)
  (format T "~&Sorry, don't know how to run programs in your CL.~%"))

;;;; CLIM/UI utilities

(defmacro bold ((stream) &body body)
  `(with-text-face (,stream :bold)
     ,@body))

(defmacro italic ((stream) &body body)
  `(with-text-face (,stream :italic)
     ,@body))

(defmacro bordering ((stream shape) &body body)
  `(surrounding-output-with-border (,stream :shape ,shape :move-cursor nil)
     ,@body))

(defmacro underlining ((stream) &body body)
  `(bordering (,stream :underline) ,@body))

(defun vertical-gap (stream &optional (fraction 3))
  (when (eq stream t) (setf stream *standard-output*))
  (stream-increment-cursor-position stream 0
      (truncate (/ (text-style-ascent (medium-text-style stream) stream) fraction))))

(defun invoke-as-heading (cont &optional ink)
  (with-drawing-options (T :ink (or ink +royal-blue+) :text-style (make-text-style :sans-serif :bold nil))
    (fresh-line)
    (bordering (T :underline)
      (funcall cont))
    (fresh-line)
    (vertical-gap T)))

(defun indent-to (stream x &optional (spacing 0) )
  "Advances cursor horizontally to coordinate X. If the cursor is already past
this point, increment it by SPACING, which defaults to zero."
  (stream-increment-cursor-position stream
				    (if (> (stream-cursor-position stream) x)
					spacing
				      (- x (stream-cursor-position stream)))))

;;; Pathname evil
;;; Fixme: Invent some more useful operators for manipulating pathnames, add a
;;;        pinch of syntactic sugar, and cut the LOC here down to a fraction.

(defun gen-wild-pathname (pathname)
  "Build a pathname with appropriate :wild components for the directory listing."
  (make-pathname :host   (pathname-host pathname)
                 :device (pathname-device pathname)
                 :directory (pathname-directory pathname)
                 :name (or (pathname-name pathname) :wild)
                 :type (or (pathname-type pathname) :wild)
                 :version (or :wild
                              ;#-SBCL (pathname-version pathname)
                              ;#+SBCL :newest
                              )))

(defun strip-filespec (pathname)
  "Removes name, type, and version components from a pathname."
  (make-pathname :host   (pathname-host pathname)
                 :device (pathname-device pathname)
                 :directory (pathname-directory pathname)
                 :name nil
                 :type nil
                 :version nil))

;; Oops, should I be doing something with relative pathnames here?
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


;;;; Abbreviating item formatter

(defparameter *abbreviating-minimum-items* 6
  "Minimum number of items needed to invoke abbreviation. This must be at least one.")
(defparameter *abbreviating-outlier-threshold* 2.0
  "Number of standard deviations beyond the mean needed to invoke abbreviation.")
(defparameter *abbreviating-minimum-ratio* 1.2
  "A minimum ratio of item width to the mean width, below which abbreviation will not be invoked. This is a safeguard to treat very uniform inputs more sanely, where the test against the standard deviation might behave undesirably.")
(defparameter *abbreviating-maximum-cutoff* 1.8
  "A maximum ratio of item width to mean width, beyond which abbreviation will always be invoked. This is useful to handle cases where one extreme outlier throws the standard deviation all out of whack.") 

(defun text-output-record-style (record)
  "Returns the text style used in RECORD, a text-displayed-output-record."
  (climi::graphics-state-text-style (first (slot-value record 'climi::strings))))

(defun find-text-record (record)
  "If RECORD contains exactly one text-displayed-output-record, we can abbreviate it. Otherwise, give up. Returns a string containing the contents of the record, and a text style."
  (declare (optimize (debug 3) (speed 1) (safety 3)))
  (let ((count 0)
        text-style
        result)
    (labels ((walk (record)
               (typecase record                 
                 (climi::compound-output-record
                  (map-over-output-records #'walk record))
                 (text-displayed-output-record                  
                  (setf result record)                  
                  (setf text-style (text-output-record-style record))
                  (incf count)))))
      (walk record)
      (values
       (if (= count 1) result  nil)       
       (or text-style (medium-text-style (slot-value record 'climi::medium)))))))

;; This logic could be useful in McCLIM's stream-output.lisp, for computing
;; line breaks. At the time, I didn't feel like writing it, but now I do. 
;; Even so, the binary search I used there is probably good enough, but this
;; would improve the quality of the guess, particularly for the extreme case
;; of throwing many lines of text at CLIM within one string.
(defun abbrev-guess-pos (medium string text-style desired-width start end)
  "Makes a guess where to split STRING between START and END in order to fit within WIDTH. Returns the ending character index."
  (let* ((length (- end start))
         (actual-width (text-size medium string :text-style text-style :start start :end end))
         (pixels-per-char (/ actual-width length))
         (guess (truncate (/ desired-width pixels-per-char))))
    (when (< actual-width desired-width)  ; This almost certainly shouldn't happen.
      (return-from abbrev-guess-pos end)) ; But it could.
    (+ start guess)))

;; FIXME: I can do a bit better than this.
;; I'd like to use this only as a fallback, eventually.
(defun abbreviate-string (medium string text-style max-width)
  "Returns an abbreviated version of STRING hopefully less than MAX-WIDTH,
as it would be displayed on MEDIUM using TEXT-STYLE"
  (let* ((ellipsis-width (text-size medium "..." :text-style text-style))
         (working-width (- max-width ellipsis-width)))
    (when (<= working-width 0)   ; weird, just give up.
      (return-from abbreviate-string string))
    ;; FIXME: I was planning to do several stages of refining the guess, but I got lazy..
    ;; Now that I've thought about it, this sort of guesswork with counting text-sizes is
    ;; pretty disgusting anyway, and I ought to just be counting character sizes, and assume
    ;; that text-size isn't just somehow magically faster than doing it myself.
    ;; so.. FIXME!
    (concatenate 'string
                 (subseq string 0 (abbrev-guess-pos medium string text-style working-width 0 (length string)))
                 "...")))

(defvar *tmp* nil)

(defun abbreviate-record (stream record width abbreviator)
  "Attempts to abbreviate the text contained in an output RECORD on STREAM to fit within WIDTH, using the function ABBREVIATOR to produce a shortened string."
  (declare (optimize (debug 3)))  
  (multiple-value-bind (text-record text-style)
      (find-text-record record)
    (when text-record      
      (multiple-value-bind (x y)
          (output-record-position text-record)
        (let* ((parent (output-record-parent text-record))
               (medium (slot-value text-record 'medium))
               (abbreviation (funcall abbreviator medium (text-displayed-output-record-string text-record)
                                      text-style (- width (- (bounding-rectangle-width record)
                                                             (bounding-rectangle-width text-record))))))
          (delete-output-record text-record parent)
          (with-text-style (medium text-style)
            (let ((new-record (with-output-to-output-record (stream)
                                (write-string abbreviation stream))))
              (setf (output-record-position new-record) (values x y))
              (add-output-record new-record parent)
              #+IGNORE (tree-recompute-extent parent)))))))
  record)

(defun abbreviating-format-items (items &rest args &key stream printer
                                        presentation-type (abbreviator #'abbreviate-string)
                                        &allow-other-keys)
  "Similar to FORMAT-ITEMS, but abbreviates excessively long text using a
function specified by :ABBREVIATOR. Abbreviate is controlled by the variables
*ABBREVIATING-OUTLIER-THRESHOLD*, *ABBREVIATING-MINIMUM-RATIO*, and
*ABBREVIATING-MAXIMUM-CUTOFF*."
  (setf stream (resolve-stream-designator stream *standard-output*))  
  (let* ((length  (length items))
         (printer (or printer (lambda (item stream)
                               (present item presentation-type :stream stream))))
         (hash (make-hash-table :test 'eq :size (truncate (* 1.5 length))))
         (mean 0.0)
         (deviation 0.0))

    (when (< length *abbreviating-minimum-items*)
      (apply #'format-items items args)
      (return-from abbreviating-format-items))
    
    (dolist (item items)
      (let ((record (with-output-to-output-record (stream)
                      (with-end-of-line-action (stream :allow)
                        (funcall printer item stream)))))
        (setf (gethash item hash) record)
        (incf mean (bounding-rectangle-width record))))
    (setf mean (/ mean length))
    (maphash (lambda (key val)
               (declare (ignore key))
               (incf deviation (expt (- mean (bounding-rectangle-width val)) 2)))
             hash)
    (unless (= length 1)
      (setf deviation (sqrt (/ deviation (1- length)))))

    (setf args (copy-list args))
    (remf args :printer)
    
    (let* ((stddev-max-width (+ mean (* *abbreviating-outlier-threshold* deviation)))
           (ratio-max-width  (* mean *abbreviating-minimum-ratio*))
           (cutoff-width     (* mean *abbreviating-maximum-cutoff*))
           (max-width        (min cutoff-width (max stddev-max-width ratio-max-width))))
   ;   (hef:debugf mean deviation stddev-max-width ratio-max-width max-width)
      (apply #'format-items items
             :printer (lambda (item stream)
                        (let ((record (gethash item hash)))
                          (when (and (> (bounding-rectangle-width record) stddev-max-width)
                                     (> (bounding-rectangle-width record) ratio-max-width))
                            (setf record (abbreviate-record stream record max-width abbreviator)))
                          (stream-add-output-record stream record)))
             args))))


;;; An attempt at integrating RUN-PROGRAM closer with lisp.
;;; That is, close enough to make it less of a pain in the ass.

;;; This code creates a macro on the #! character sequence which expands
;;; to a lambda closed over a call to RUN-PROGRAM invoked the program
;;; named by the following string, ex. (#!rm :r :f "foodir")

;;; My apologies to anyone using the #! character for something useful.


;; TODO:
;;  * Evil environment variable hack (scan some package for variables prefixed
;;    with '$', build the environment variables from that)
;;  * Figure out what to do with the input/output streams
;;  * Ability to pipe programs together, input/output redirection.
;;  * Utilities for getting data in and out of unix programs through streams    
;;  * Pseudoterminal support (yeah, right)

(defparameter *program-wait* t)

;; Disgusting hacks to make input default to nil, as CMUCL's run-program seems
;; to hang randomly unless I do that. But sometimes I'll need to really change these..
;; ** Goddamn CMUCL's run-program likes to hang randomly even with this dumb hack. Beware..
(defparameter *run-output* T)
(defparameter *run-input* nil)

;; We attempt to translate keywords and a few types of lisp objects
;; used as arguments to make program wrappers feel more "lispy".

(defgeneric transform-program-arg (arg))

(defmethod transform-program-arg ((arg T))
  (values (prin1-to-string arg)))

(defmethod transform-program-arg ((arg string))
  arg)

(defmethod transform-program-arg ((arg sequence))
  (values-list (map 'list #'transform-program-arg arg)))

(defmethod transform-program-arg ((arg symbol))
  (let ((name (string-downcase (symbol-name arg))))
    (if (keywordp arg)
        (values (concatenate 'string (if (= 1 (length name)) "-" "--") name))
      name)))  ;; do some less horrible DWIM downcasing of vanilla symbols? hmm..

(defmethod transform-program-arg ((arg pathname))
  (if (wild-pathname-p arg)
      (values-list (mapcar #'transform-program-arg
                           (directory arg))) ;; (with-fingers-crossed ...)
    (values (namestring arg))))

(defun transform-program-arguments (args)
  (let ((list nil))
    (dolist (arg args)
      (setf list (nconc list (multiple-value-list (transform-program-arg arg)))))
    list))
;  (mapcar #'transform-program-arg args)

(defun program-wrapper (name)
  "Returns a closure which invokes the NAMEd program through the operating system,
with some attempt to convert arguments intelligently."
  (lambda (&rest args)
    (run-program name (transform-program-arguments args)
                 :wait *program-wait*
                 :output (resolve-stream-designator *run-output* *standard-output*)
                 :input  nil #+NIL (resolve-stream-designator *run-input* *standard-input*))))

(defun read-stringlet (stream)
  (with-output-to-string (out)
    (unread-char                          
     (do ((c (read-char stream) (read-char stream)))
         ((or (member c '(#\Space #\Tab #\Newline #\Linefeed #\Page #\Return)) ;; What..??
              (multiple-value-bind (a b) (get-macro-character c)
                (and a (not b))))
          c)       
       (when (eql c #\\)
         (setf c (read-char stream)))       
       (write-char c out))
     stream)))

(set-dispatch-macro-character #\# #\!
  #'(lambda (stream char p)
      (declare (ignore char p))
      (let ((name (read-stringlet stream)))
        `(lambda (&rest args)
           (apply (program-wrapper ,name) args)))))

