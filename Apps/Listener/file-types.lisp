;;; File types

(in-package :clim-listener)

; This implementation of MIME types is rather silly.
; I'm not sure that it's that important to do a better job
; at the moment.

;; TODO:
;;  * Link mime-types with presentation-types
;;  * ..then, tie them into presentation-type-of to make it automagic.
;;  * Smarter detection of file types

(defclass mime-media-type ()
  ((media-type-name :reader media-type-name))
  (:documentation "MIME top-level media type"))

(defclass text  (mime-media-type)
  ((media-type-name :initform 'text))
  (:documentation "Textual information"))
  
(defclass image (mime-media-type)
  ((media-type-name :initform 'image))
  (:documentation "Image data"))

(defclass audio (mime-media-type)
  ((media-type-name :initform 'audio))
  (:documentation "Audio data"))

(defclass video (mime-media-type)
  ((media-type-name :initform 'video))
  (:documentation "Video data"))

(defclass application (mime-media-type)
  ((media-type-name :initform 'application))
  (:documentation "Application data"))

(defclass inode (mime-media-type)
  ((media-type-name :initform 'inode))
  (:documentation "Unix inode"))

;; Note that specific mime types should inherit mime-type as well as
;; a media-type class, preferably in that order.
(defclass mime-type (mime-media-type)
  ((media-subtype :reader media-subtype-name)
   #+nil (extensions :initform nil :reader extensions))
  (:documentation "A full specified MIME content-type"))

;; Default mime icons

(defmethod icon-of ((obj mime-media-type))
  *document-icon*)

(defmethod icon-of ((obj text))
  (standard-icon "text.xpm"))

(defmethod icon-of ((obj audio))
  (standard-icon "audio.xpm"))

(defmethod icon-of ((obj image))
  (standard-icon "image.xpm"))

(defmethod icon-of ((obj video))
  (standard-icon "video.xpm"))

(defmethod icon-of ((obj application))
  (standard-icon "simple-object.xpm"))

;; Useful methods related to mime types

#+IGNORE (defmethod mime-command-translator ((type T) pathname)
  nil)


(defvar *extension-mapping* (make-hash-table :test #'equalp)
  "Mapping from file extension string to symbols naming mime type classes.")

(defvar *icon-mapping* (make-hash-table :test #'eq)
  "Mapping from symbols naming mime type classes to icon patterns.")

(defvar *view-command-mapping* (make-hash-table :test #'eq)
  "Mapping from symbols naming mime types to hash tables containing view-command
   information parsed from /etc/mime.types")

(defun pathname-extension (pathname)
  "Returns the 'extension' of a file, whatever the hell that means."
  ;; FIXME: This is all wrong. As a call to pathname-type, this function
  ;; seems rather silly, but we can do better than this.
  (pathname-type pathname))


;; A lot of magic needs to be done here to handle various things which
;; are not conveyed through file extensions. Most importantly, looking
;; at the executable bit to distinguish binaries from everything else.

(defvar *magic-name-mappings* (make-hash-table :test #'equalp))

(defmacro defmagic (type &rest args)
  `(dolist (filename ',args)
     (setf (gethash filename *magic-name-mappings*) ',type)))

(defun lookup-magic-name (pathname)
  (let* ((type (pathname-type pathname))
         (name (pathname-name pathname))
         (key (if type (concatenate 'string name "." type)
                (concatenate 'string name)))                              
         (item (gethash key *magic-name-mappings*)))
    item))

(defun pathname-mime-type (pathname)
  (or (lookup-magic-name pathname)            
      (gethash (pathname-extension pathname) *extension-mapping*)))

(defmacro define-mime-type ((media-type subtype) &rest options)
  (assert (find-class media-type nil))
  ; XXX Bad, probably I should put all the symbols in one MIME package or something.
  (let ((full-type (intern (concatenate 'string (symbol-name media-type) "/" (symbol-name subtype))
                           (symbol-package subtype))))
    `(progn (defclass ,full-type (mime-type ,media-type)
              ((media-subtype :initform ',subtype)))
            ,@(mapcar (lambda (opt)
                        (case (first opt)
                          (:extensions `(dolist (ext ',(rest opt))
                                          (setf (gethash ext *extension-mapping*) ',full-type)))
                          (:names `(defmagic ,full-type ,@(rest opt)))
                          (:icon `(setf (gethash ',full-type *icon-mapping*) ,(second opt)))))
                      options))))
 
(defmethod icon-of ((pathname pathname))
;  (debugf "ICON-OF (pathname) " pathname)
  (cond ((not (probe-file pathname)) (standard-icon "invalid.xpm"))
        ((directoryp pathname) *folder-icon*) ;; FIXME: use inode mime types
        ((find-class (pathname-mime-type pathname) nil)
         (icon-of (clim-mop:class-prototype (find-class (pathname-mime-type pathname) nil))))
        (T *document-icon*)))

(defmethod icon-of ((obj mime-type))
;  (debugf "ICON-OF (mime-type) "  obj)
  (let ((cpl (clim-mop:class-precedence-list (class-of obj))))
    (dolist (class cpl)
;      (debugf "   " class)
      (let ((icon (gethash (class-name class) *icon-mapping*)))
        (when icon (return-from icon-of icon)))))
  (call-next-method))

;; Some predefined MIME types
;; Don't need to do too much here, most of them will be grabbed from
;; the /etc/mime.types file.

(define-mime-type (text plain)
  (:extensions "txt" "text")
  (:icon *document-icon*))

(define-mime-type (text plain)
  (:extensions "txt" "text")
  (:icon *document-icon*))

(define-mime-type (text x-makefile)
  (:names "Makefile"))

(define-mime-type (text x-lisp-source)
  (:extensions "lisp")
  (:icon (standard-icon "lambda.xpm")))

(define-mime-type (text x-csrc)
  (:extensions "c")
  (:icon (standard-icon "c.xpm")))

(define-mime-type (text x-chdr)
  (:extensions "h")
  (:icon (standard-icon "h.xpm")))

(define-mime-type (text x-lisp-system)
  (:extensions "system" "asd")
  (:names "system.lisp" "defsystem.lisp")
  (:icon (standard-icon "design.xpm")))

(define-mime-type (application x-lisp-fasl)
  (:extensions "x86f" "fasl" "ibin" "dfsl") ; MORE!
  (:icon (standard-icon "object.xpm")))

(define-mime-type (text x-shellscript)
  (:extensions "sh")
  (:icon (standard-icon "script.xpm")))

;; Magic name mappings (very silly things)
;; It occurs to me for these types of mappings, a "prefix" mapping would be 
;; vastly more useful than what I have here. That is, it would be more useful
;; to match patterns like Makefile*, INSTALL*, README*, etc.

(defmagic text/plain "readme" "read.me" "copying" "copyright" "install")


;;; /etc/mime.types parser

(defun read-slashified-line (stream &optional (accumulation nil))
  (let ((line (read-line stream nil)))
    (cond ((null line) (values nil nil))
          ((zerop (length line)) (values accumulation T))
          ((and (null accumulation)  ;; # Comment
                (char= (elt line 0) #\#))
           (values nil T))
          (T (if (char= #\\ (elt line (1- (length line))))
                 (read-slashified-line stream
                                       (concatenate 'string accumulation
                                           (subseq line 0 (1- (length line)))))
               (values (concatenate 'string accumulation line) T))))))

(defun read-the-lines (pathname)
  (let ((elements nil))
    (with-open-file (in pathname)
      (loop
        (multiple-value-bind (val more-input)
            (read-slashified-line in)
          (unless more-input (return-from read-the-lines elements))
          (when val (push val elements)))))))

(defun skip-whitespace (string &optional (start 0) end)
  (when start
    (or (position-if (lambda (c)
                       (and (graphic-char-p c)
                            (not (char= c #\space))))
                     string :start start :end end) end)))

(defun file-char-p (char)
  (and (graphic-char-p char)
       (not (char= char #\space))))

(defun read-extensions (string &optional (start 0))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (setf start (skip-whitespace string start))
  (when start
    (let ((pos (or (position-if-not #'file-char-p string :start start)
                   (length string))))
      (cons (subseq string start pos)
            (read-extensions string pos)))))

(defun read-mime-type (string &optional (start 0))
  (setf start (skip-whitespace string start))
  (let* ((pos-slash (position #\/ string  :test #'char= :start start))
         (pos-end ;(position #\space string :test #'char= :start (1+ pos-slash))
                  (position-if (lambda (c) (member c '(#\space #\tab)))
                               string  :start (1+ pos-slash)))
         (media-type (string-upcase (subseq string start pos-slash)))
         (media-type-sym (intern media-type))
         (subtype (string-upcase (subseq string (1+ pos-slash) pos-end)))
         (full-symbol (intern (concatenate 'string media-type "/" subtype))))
    (values media-type-sym full-symbol (intern subtype) pos-end)))

;;; PARSE-NETSCAPE-MIME-TYPE and PARSE-STANDARD-MIME-TYPE return the various
;;; properties of each type in a hash table. The primary ones of concern are
;;; :TYPE, :MEDIA-TYPE, :EXTS, and :DESC.


;; Is this even a standard format to put things in? The only thing I've seen
;; make records of this type is Netscape, and things like Pine claim they
;; don't parse it at all. Stupid netscape.

; * hefner cringes.

(defun parse-netscrapings (table string &optional (start 0))
  "Recursively parse FOO=BAR pairs, returning the result in a hash table."
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (setf start (skip-whitespace string start))
  (when start
    (let ((split-pos (position #\= string :start start)))
      (when split-pos
        (let* ((foo (subseq string start split-pos))
               (pos (skip-whitespace string (1+ split-pos))))
;          (format T "~%*****   foo=~A~%" foo)
          (when pos
            (let* ((end (or (if (eql (elt string pos) #\")
                                (1+ (position-if (lambda (c)
                                                   (char= c #\"))
                                                 string :start (1+ pos)))
                              (position-if (lambda (c)
                                             (member c '(#\space #\tab)))
                                           string :start pos))
                       (length string)))
                   (real-start (if (eql #\" (elt string pos))
                                   (1+ pos)
                                 pos))
                   (real-end (if (eql #\" (elt string (1- end)))
                                 (1- end)
                               end))
                   (bar (subseq string real-start real-end))
                   (keysym (intern (string-upcase foo) (find-package :keyword)))
                   (value (case keysym
                            (:type (nth-value 1 (read-mime-type bar)))
                            (:exts (read-extensions bar))
                            (otherwise bar))))
              (when (eq keysym :type)
                (setf (gethash :subtype table) (nth-value 2 (read-mime-type bar)))
                (setf (gethash :media-type table) (read-mime-type bar)))
;              (format T "~&~W => ~W~%" foo bar)
              (setf (gethash keysym table) value)
              (parse-netscrapings table string end) ))))))
  table)

(defun parse-netscape-mime-type (elt)
  "Parse a mimetype of the form 'type=foo/bar desc=baz...'"
  (let ((table (make-hash-table :size 4)))
    (parse-netscrapings table elt)
    table))

(defun parse-standard-mime-type (elt)
  "Parse a 'normal' mime.types entry"
  (let ((table (make-hash-table :size 4)))
    (multiple-value-bind (media-type type subtype pos)
        (read-mime-type elt)
      (setf (gethash :media-type table) media-type)
      (setf (gethash :type table) type)
      (setf (gethash :subtype table) subtype)
      (setf (gethash :exts table)
            (read-extensions elt pos))
      table)))

(defun parse-mt-elt (elt)
  (if (search "type=" elt)
      (parse-netscape-mime-type elt)
    (parse-standard-mime-type elt)))

(defun process-mime-type (elt)
  (when elt
    (if (find-class (gethash :media-type elt) nil)
        (let ((media-type (gethash :media-type elt))
              (subtype (gethash :subtype elt))
              (exts (gethash :exts elt)))
          (eval `(define-mime-type (,media-type ,subtype)
                   (:extensions ,@exts))))
      #+nil(format T "Ignoring ~W, unknown media type.~%" (gethash :type elt))))
)
  
(defun parse-mime-types-file (pathname)
  (mapcar (lambda (x) (process-mime-type (parse-mt-elt x)))
          (read-the-lines pathname)))


;;; Mailcap parser (RFC 1524)

;Location of the Mailcap File(s)

;   For UNIX, a path search of mailcap files is specified.  The default
;   path search is specified as including at least the following:

;   $HOME/.mailcap:/etc/mailcap:/usr/etc/mailcap:/usr/local/etc/mailcap

;Semantics of executable commands

;   Several portions of a mailcap entry specify commands to be executed.
;   In particular, the mandatory second fie ld, the view-command, takes a
;   command to be executed, as do the optional print, edit, test, and
;   compose fields.

;   On a UNIX system, such commands will each be a full shell command
;   line, including the path name for a program and its arguments.
;   (Because of differences in shells and the implementation and behavior
;   of the same shell from one system to another, it is specified that
;   the command line be intended as input to the Bourne shell, i.e., that
;   it is implicitly preceded by "/bin/sh -c " on the command line.)

;   The two characters "%s", if used, will be replaced by the name of a
;   file for the actual mail body data.   [snip]

;   Furthermore, any occurrence of "%t" will be replaced by the content-
;   type and subtype specification.  (That is, if the content-type is
;   "text/plain", then %t will be replaced by "text/plain".)

; Semantics of the "test" field... ignoring this for now.
; > Don't see any of them on my system that aren't just tests against
; > the DISPLAY variable, anyway, and we know that's set.
; > Will fix later..

;;; This is not a mail client, so I assume I don't need most of this stuff.

;(defun

(defun remove-surrounding-whitespace (string)
  (let ((start (position-if (lambda (c) (not (eql c #\space))) string))
        (end   (position-if (lambda (c) (not (eql c #\space))) string :from-end t)))
    (when (and start end)
      (subseq string start (1+ end)))))

#+nil  ;; Oops, forgot to parse quote characters.
(defun read-mailcap-field (string &optional (start 0))
  "Seperates a field of a mailcap entry, delimited by either semicolons or the end
   of the string. Returns two values, the string contents of the field, and the
   new position index (or nil if out of input)."
  (if (and start (< start (length string)))
    (let ((end-pos (or (position #\; string :start start) (length string))))
      (values (remove-surrounding-whitespace (subseq string start end-pos))
              (1+ end-pos)))
    (values nil nil)))

(defun read-mailcap-field (string &optional (start 0))
  (let ((index start)
        (chars nil))
    (loop named poop while (< index (length string)) do       
      (let ((c (elt string index)))
        (cond ((eql c #\\)  ; quoted character?
               (when (< index (1- (length string)))
                 (push (elt string (incf index)) chars)))
              ((eql c #\;) (return-from poop chars))
              (T (push c chars)))
        (incf index)))
    (values 
     (remove-surrounding-whitespace (concatenate 'string (nreverse chars)))
     (if (>= (1+ index) (length string)) nil (1+ index)))))

(defun parse-mt-field (string)
  (let* ((sep-pos (position #\= string))
         (field-name (subseq string 0 (or sep-pos (length string)))))
    (values (intern (string-upcase field-name) (find-package :keyword))
            (ignore-errors (or (when sep-pos (subseq string (1+ sep-pos))) T)))))

(defun parse-mailcap-entry (line)
  "Parses a line of the mailcap file, returning either nil or the properties
   of the type in a hash table."
  (let ((table (make-hash-table))
        (foo nil)) ; <- position after reading required fields
    (when                    ;; First read the required fields.
        (multiple-value-bind (text pos)
            (read-mailcap-field line)
          (multiple-value-bind (media-type type subtype)
              (read-mime-type text)
            (multiple-value-bind (view-command pos)
                (read-mailcap-field line pos)                
              (setf foo pos)
              (setf (gethash :type table) type)
              (setf (gethash :subtype table) subtype)
              (setf (gethash :media-type table) media-type)
    #| --> |# (setf (gethash :view-command table) view-command))))
      ;; If the required fields were read successfully, read
      ;; the options into the hash table.
      (loop
        (when (null foo)  (return-from parse-mailcap-entry table))
        (multiple-value-bind (text pos)
            (ignore-errors (read-mailcap-field line foo))
          (setf foo pos)
          (when text
            (multiple-value-bind (field value)
                (parse-mt-field text)
              (when field
                (setf (gethash field table) value))))) ))))

(defun process-mailcap-entry (entry)
  (when entry
    (setf (gethash (gethash :type entry) *view-command-mapping*) entry)))          

(defun parse-mailcap-file (pathname)  
  (mapcar (lambda (x) (process-mailcap-entry
                       (parse-mailcap-entry x)))
          (read-the-lines pathname)))

;;; These functions invoke the parsing of the mime.types and mailcap files,
;;; and bring the data into the system as needed.

;; Search paths - in addition to these, the user's home directory will
;; be checked.

(defparameter *mime.types-search-path*
  '(#p"/etc/mime.types" #p"/usr/etc/mime.types" #p"/usr/local/etc/mime.types"))

(defparameter *mailcap-search-path*
  '(#p"/etc/mailcap" #p"/usr/etc/mailcap" #p"/usr/local/etc/mailcap"))


(defun load-mime-types ()
  (let ((search-path (cons (merge-pathnames #P".mime.types" (user-homedir-pathname))
                           *mime.types-search-path*)))
    (dolist (path (reverse search-path))
      (when (probe-file path)
        (format T "Loading mime types from ~A~%" path)
        (parse-mime-types-file path)))))

(defun load-mailcaps ()
  (let ((search-path (cons (merge-pathnames #P".mailcap" (user-homedir-pathname))
                           *mailcap-search-path*)))
    (dolist (path (reverse search-path))
      (when (probe-file path)
        (format T "Loading mailcap from ~A~%" path)
        (parse-mailcap-file path)))))


;; Running external viewers..
;; I don't have the quoting for things quite right, I can't
;; seem to play any mp3s with quotes in their titles. (how embarassing!)

(defun quote-shell-characters (string)
  (let ((shell-chars '(#\` #\$ #\\ #\" #\')))
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for c = (read-char in nil) while c do
        (when (member c shell-chars)
          (write-char #\\ out))
        (write-char c out))))))
  
(defun gen-view-command-line (spec pathname)
  (with-output-to-string (out)
    (with-input-from-string (in (gethash :view-command spec))
      (loop for c = (read-char in nil) while c do 
        (if (char= c #\%)
            (let ((d (read-char in nil)))
              (cond ((eql d #\s)  (princ   (quote-shell-characters (namestring (truename pathname))) out))
                    ((eql d #\t)  (princ (gethash :type spec) out))
                    (T (debugf "Ignoring unknown % syntax." d))))
          (write-char c out))))))

(defun find-viewspec (pathname)
  (let* ((type (pathname-mime-type pathname))
         (def  (gethash type *view-command-mapping*)))
    (when (and def
               (probe-file pathname)
               (gethash :view-command def)
               (not (gethash :needsterminal def)))
      (list "/bin/sh" `("-c" ,(gen-view-command-line def pathname) )))))

(defun run-view-command (pathname)
  (let* ((type (pathname-mime-type pathname))
         (def  (gethash type *view-command-mapping*)))
    (when def
      (let* ((view-command (gethash :view-command def))
             (test (gethash :test def))
             (needsterminal (gethash :needsterminal def)))
        (if needsterminal
            (format T "Sorry, the viewer app needs a terminal. Fix this.~%")
          (progn
            (when test
              (debugf "Sorry, ignoring TEST option right now.. " test))
            (if view-command 
                (run-program "/bin/sh" `("-c" ,(gen-view-command-line def pathname) "&"))
              (format T "~&No view-command!~%"))))))))








(eval-when (:load-toplevel :execute)
  (load-mime-types)
  (load-mailcaps))