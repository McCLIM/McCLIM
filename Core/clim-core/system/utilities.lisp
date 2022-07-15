;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001 Arnaud Rouanet <rouanet@emi.u-bordeaux.fr>
;;;  (c) copyright 2018 Cyrus Harmon <ch-lisp@bobobeach.com>
;;;  (c) copyright 2017-2020 Daniel Kochmański <daniel@turtleware.eu>
;;;  (c) copyright 2019, 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Utilities used through the McCLIM codebase.

(in-package #:climi)

;;;; Early special variables

(defvar *application-frame* nil)
(defvar *pointer-documentation-output* nil)

;;; Command name utilities that are useful elsewhere.

(defun command-name-from-symbol (symbol)
  (let ((name (symbol-name symbol)))
    (string-capitalize
     (substitute
      #\Space #\-
      (subseq name (if (string= '#:com- name :end2 (min (length name) 4))
                       4
                       0))))))

(defun keyword-arg-name-from-symbol (item)
  (format nil "~(:~a~)" item))

;;; The command object is specified to be a list where the first argument is
;;; the command name (a symbol). However we allow symbols to be put in menus
;;; and drei inserts sometimes a literal function instead of a symbol.
(deftype command-designator ()
  `(or (cons function-designator) function-designator))

;;; Move this early so it can be used in presentations.lisp, which comes before
;;; commands.lisp.
(defmacro do-command-table-inheritance ((command-table-var command-table)
                                        &body body)
  `(apply-with-command-table-inheritance
    #'(lambda (,command-table-var)
        ,@body)
    (find-command-table ,command-table)))

;;; Convenience macro
(defmacro gesture-case (event &body cases)
  (once-only (event)
    (flet ((make-match (match body)
             `((event-matches-gesture-name-p ,event ,match) ,@body)))
      `(cond ,@(loop for (match . body) in cases
                     appending (if (atom match)
                                   (list (if (member match '(otherwise t))
                                             `(t ,@body)
                                             (make-match match body)))
                                   (loop for match in match
                                         collect (make-match match body))))))))

;;; Utilities
(defun parse-method (description)
  (loop
    for (qualifier-or-ll . body) on description
    until (listp qualifier-or-ll)
    collect qualifier-or-ll into qualifiers
    finally (return
              (values qualifiers
                      (c2mop:extract-specializer-names qualifier-or-ll)
                      (c2mop:extract-lambda-list qualifier-or-ll)
                      body))))

(defun get-body-declarations (body)
  "Collect all declaration forms from a body of forms that may have
 declarations at its top. Returns as values a list of the declarations and the
 rest of the body."
  (loop for bod on body
        for (form) = bod
        if (and (consp form) (eq (car form) 'declare))
          collect form into decls
        else
          return (values decls bod)
        finally         (return (values decls nil)))) ;It's all (declare ...)


;;;; ----------------------------------------------------------------------

;;; FIXME valid space specification format is described in the section
;;; describing a FORMATTING-TABLE macro. This should be generalized
;;; for other possible space definitions and described in a separate
;;; section. This functionality partially overlaps with a space
;;; specification format described for the layout macros like
;;; VERTICALLY, We should scram a superset of both in PARSE-SPACE and
;;; add a special handling for the non-stream panes. -- jd 2019-11-02

(deftype space-spec ()
  `(or real
       string
       character
       function
       (cons real
             (cons (member :character :line :point :pixel :mm)
                   null))))

(defun parse-space (stream specification direction)
  "Returns the amount of space given by SPECIFICATION relating to the
STREAM in the direction DIRECTION."
  ;; This implementation lives under the assumption that an
  ;; extended-output stream is also a sheet and has a graft.
  ;; --GB 2002-08-14
  (etypecase specification
    (real specification)
    ((or string character) (multiple-value-bind (width height)
                               (text-size stream (string specification))
                             (ecase direction
                               (:horizontal width)
                               (:vertical height))))
    (function (let ((record
                      (invoke-with-output-to-output-record
                       stream (lambda (s o)
                                (declare (ignore s o))
                                (funcall specification))
                       'standard-sequence-output-record)))
                (ecase direction
                  (:horizontal (bounding-rectangle-width record))
                  (:vertical (bounding-rectangle-height record)))))
    (cons
     (destructuring-bind (value unit)
         specification
       (ecase unit
         (:character
          (ecase direction
            (:horizontal (* value (stream-character-width stream #\M)))
            (:vertical   (* value (stream-line-height stream)))))
         (:line
          (ecase direction
            (:horizontal (* value (stream-line-width stream)))
            (:vertical   (* value (stream-line-height stream)))))
         ((:point :pixel :mm)
          (let* ((graft (graft stream))
                 (gunit (graft-units graft)))
            ;; mungle specification into what grafts talk about
            (case unit
              ((:point) (setf value (/ value 72) unit :inches))
              ((:pixel) (setf unit :device))
              ((:mm)    (setf unit :millimeters)))
            ;;
            (multiple-value-bind (dx dy)
                (multiple-value-call
                    #'transform-distance
                  (compose-scaling-with-transformation
                   (sheet-delta-transformation stream graft)
                   (/ (graft-width graft :units unit)
                      (graft-width graft :units gunit))
                   (/ (graft-height graft :units unit)
                      (graft-height graft :units gunit)))
                  (ecase direction
                    (:horizontal (values 1 0))
                    (:vertical   (values 0 1))))
              (/ value (sqrt (+ (* dx dx) (* dy dy))))))))))))

(defun valid-margin-spec-p (margins)
  (ignore-errors ; destructuring-bind may error; that yields invalid spec
   (destructuring-bind (&key left top right bottom) margins
     (flet ((margin-spec-p (margin)
              (destructuring-bind (anchor value) margin
                (and (member anchor '(:relative :absolute))
                     ;; Value must be a valid argument to PARSE-SPACE,
                     ;; not necessarily a number. -- jd 2019-10-31
                     (typep value 'space-spec)))))
       (every #'margin-spec-p (list left top right bottom))))))

(deftype margin-spec ()
  `(satisfies valid-margin-spec-p))

(defun normalize-margin-spec (plist defaults)
  (loop with plist = (copy-list plist)
        for edge in '(:left :top :right :bottom)
        for value = (getf plist edge)
        do
           (typecase value
             (null (setf (getf plist edge) (getf defaults edge)))
             (atom (setf (getf plist edge) `(:relative ,value)))
             (list #| do nothing |#))
        finally
           (check-type plist margin-spec)
           (return plist)))

;;; Returns an index of the character _before_ which we should break
;;; the line (may be used as END argument in SUBSEQ). When FROM-END is
;;; T, then we return the rightmost opportunity which does not violate
;;; hard line breaks. Second value indicates the kind of a line break.
(defun line-end (string break-fn start limit from-end)
  (loop with opportunity = nil
        for i from start below limit
        do (ecase (funcall break-fn string start i)
             (:hard (return (values (1+ i) :hard)))
             (:soft (if from-end
                        (setf opportunity (1+ i))
                        (return (values (1+ i) :soft))))
             ((nil) #| do nothing |#))
        finally
           (if (null opportunity)
               (return (values limit :emergency))
               (return (values opportunity :soft)))))

(defun bisect (start end predicate-fn &optional predicament-fn)
  "Finds the rightmost index meeting the PREDICATE-FN between START and END. It
is assumed that START meets the predicate while END doesn't. These indexes are
*not* tested.

PREDICATE-FN INDEX
Should return NIL if index does not meet the predicate and something else
otherwise.

PREDICAMENT-FN INDEX-1 INDEX-2
Returns next index between its arguments for test.  If there is nothing more to
test must return NIL. When not supplied default function looks always for an
index being halfway between INDEX-1 and INDEX-2."
  (unless predicament-fn
    (setf predicament-fn (lambda (last-good last-bad)
                           (let ((predicament (floor (+ last-good last-bad) 2)))
                             (and (/= predicament last-good)
                                  (/= predicament last-bad)
                                  predicament)))))
  (loop
     with last-good = start
     with last-bad = end
     as current-guess = (funcall predicament-fn last-good last-bad)
     until (null current-guess)
     do (if (funcall predicate-fn current-guess)
            (setf last-good current-guess)
            (setf last-bad current-guess))
     finally (return last-good)))

(defun make-break-function (break-strategy)
  (etypecase break-strategy
    (function
     break-strategy)
    ;; break everywhere
    ((eql nil)
     (lambda (string start index)
       (declare (ignore start))
       (case (char string index)
         (#\newline :hard)
         (otherwise :soft))))
    ;; default strategy
    ((eql t)
     (lambda (string start index)
       (declare (ignore start))
       (case (char string index)
         (#\newline :hard)
         (#\space   :soft)
         (otherwise nil))))
    ;; In case of sequences we assume
    ;; that the string is a single line
    ;; without any hard line breaks
    (list
     (lambda (string start index)
       (declare (ignore start))
       (when (member (char string index)
                     break-strategy)
         :soft)))
    (vector
     (lambda (string start index)
       (declare (ignore start string))
       (when (find index break-strategy)
         :soft)))))

(defun line-breaks (string width
                    &key
                      (break-strategy t)
                      initial-offset
                      (margin nil margin-p)
                      (start 0) end count
                    &aux
                      (width-fn (etypecase width
                                  (function width)
                                  (number (lambda (string start end)
                                            (declare (ignore string))
                                            (* width (- end start))))))
                      (width (etypecase width
                               (function nil)
                               (number width)))
                      (break-fn (make-break-function break-strategy))
                      (initial-offset (or initial-offset 0))
                      (margin (if margin-p
                                  margin
                                  (* 80 (funcall width-fn "m" 0 1))))
                      (start (or start 0))
                      (end (or end (length string))))
  "Function takes a string and returns a list of indexes where it should be split.

WIDTH is a function accepting STRING, START and END arguments which should
return string width for these boundaries. Alternatively for fixed font width it
is a number.

INITIAL-OFFSET is an initial position for the first line (may be negative). All
remaining lines will start from the line beginning. Default is line beginning.

MARGIN is a maximum width at which line should break. Defaults to 80
characters (width of a character m is taken as a reference
value). When explicitly specified as NIL then only hard line breaks
are returned.

BREAK-STRATEGY may be:
- symbol T implementing a default line breaking by word strategy,
- symbol NIL implementing a line breaking by character strategy,
- function accepting index which should return T for break opportunity,
- list of characters which are break opportunities (i.e space),
- vector of string indexes which are break opportunities.

START/END designate the sub-sequence of STRING beginning and ending
offset. The sub-sequence may contain newline characters and it is up
to the BREAK-STRATEGY whenever it assigns any meaning to to them.

COUNT specifies how many breaks we want to collect."
  (when (= start end)
    (return-from line-breaks nil))
  (assert (and (array-in-bounds-p string start)
               (array-in-bounds-p string (1- end))
               (< start end)))
  (check-type count (or null (integer 0)))
  (when (and count (zerop count))
    (return-from line-breaks))
  ;; Margin explicitly specified as NIL (break only on hard breaks).
  (when (null margin)
    (collect (break-line)
      (loop (multiple-value-bind (index break)
                (line-end string break-fn start end t)
              (when (= index end)
                (when (eq break :hard)
                  (break-line index))
                (return))
              (break-line index)
              (when (and count (zerop (decf count)))
                (return))))
      (return-from line-breaks (break-line))))
  ;; Before each call to next-line we want to narrow the break
  ;; opportunity boundaries.
  (flet ((narrow-end (start end line-width)
           (if width
               (min end (+ start (floor line-width width)))
               (bisect start (1+ end)
                       (lambda (index)
                         (<= (funcall width-fn string start index)
                             line-width))))))
    (collect (break-line)
      (when (>= initial-offset margin)
        (break-line start)
        (setf initial-offset 0))
      (loop for offset = initial-offset then 0
            for new-end = (narrow-end start end (- margin offset))
            do (multiple-value-bind (index break)
                   (line-end string break-fn start new-end t)
                 (when (and (= end new-end)
                            (not (eq break :hard)))
                   (break-line end)
                   (return-from line-breaks (break-line)))
                 ;; Degenerate case (ditto).
                 (when (and (= index start)
                            (<= offset 0))
                   (incf index))
                 (break-line index)
                 (setf start index)
                 (when (= index end)
                   (return)))
            until (and count (zerop (decf count))))
      (break-line))))
