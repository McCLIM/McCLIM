;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

(in-package :common-lisp-user)

;;;
;;; CLIM-LISP
;;;

;; Our CLIM-LISP also contains gray streams, as I consider them part
;; of Common Lisp.

;; If you want to patch a CL symbol, you define it in CLIM-LISP-PATCH
;; and export it.

#.(let ((all-ansi-symbols
         '("&ALLOW-OTHER-KEYS" "&AUX" "&BODY" "&ENVIRONMENT" "&KEY" "&OPTIONAL" "&REST" "&WHOLE" "*"
           "**" "***" "*BREAK-ON-SIGNALS*" "*COMPILE-FILE-PATHNAME*" "*COMPILE-FILE-TRUENAME*"
           "*COMPILE-PRINT*" "*COMPILE-VERBOSE*" "*DEBUG-IO*" "*DEBUGGER-HOOK*"
           "*DEFAULT-PATHNAME-DEFAULTS*" "*ERROR-OUTPUT*" "*FEATURES*" "*GENSYM-COUNTER*"
           "*LOAD-PATHNAME*" "*LOAD-PRINT*" "*LOAD-TRUENAME*" "*LOAD-VERBOSE*" "*MACROEXPAND-HOOK*"
           "*MODULES*" "*PACKAGE*" "*PRINT-ARRAY*" "*PRINT-BASE*" "*PRINT-CASE*" "*PRINT-CIRCLE*"
           "*PRINT-ESCAPE*" "*PRINT-GENSYM*" "*PRINT-LENGTH*" "*PRINT-LEVEL*" "*PRINT-LINES*"
           "*PRINT-MISER-WIDTH*" "*PRINT-PPRINT-DISPATCH*" "*PRINT-PRETTY*" "*PRINT-RADIX*"
           "*PRINT-READABLY*" "*PRINT-RIGHT-MARGIN*" "*QUERY-IO*" "*RANDOM-STATE*" "*READ-BASE*"
           "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*" "*READ-SUPPRESS*" "*READTABLE*"
           "*STANDARD-INPUT*" "*STANDARD-OUTPUT*" "*TERMINAL-IO*" "*TRACE-OUTPUT*" "+" "++" "+++" "-"
           "/" "//" "///" "/=" "1+" "1-" "<" "<=" "=" ">" ">=" "ABORT" "ABS" "ACONS" "ACOS" "ACOSH"
           "ADD-METHOD" "ADJOIN" "ADJUST-ARRAY" "ADJUSTABLE-ARRAY-P" "ALLOCATE-INSTANCE"
           "ALPHA-CHAR-P" "ALPHANUMERICP" "AND" "APPEND" "APPLY" "APROPOS" "APROPOS-LIST" "AREF"
           "ARITHMETIC-ERROR" "ARITHMETIC-ERROR-OPERANDS" "ARITHMETIC-ERROR-OPERATION" "ARRAY"
           "ARRAY-DIMENSION" "ARRAY-DIMENSION-LIMIT" "ARRAY-DIMENSIONS" "ARRAY-DISPLACEMENT"
           "ARRAY-ELEMENT-TYPE" "ARRAY-HAS-FILL-POINTER-P" "ARRAY-IN-BOUNDS-P" "ARRAY-RANK"
           "ARRAY-RANK-LIMIT" "ARRAY-ROW-MAJOR-INDEX" "ARRAY-TOTAL-SIZE" "ARRAY-TOTAL-SIZE-LIMIT"
           "ARRAYP" "ASH" "ASIN" "ASINH" "ASSERT" "ASSOC" "ASSOC-IF" "ASSOC-IF-NOT" "ATAN" "ATANH"
           "ATOM" "BASE-CHAR" "BASE-STRING" "BIGNUM" "BIT" "BIT-AND" "BIT-ANDC1" "BIT-ANDC2"
           "BIT-EQV" "BIT-IOR" "BIT-NAND" "BIT-NOR" "BIT-NOT" "BIT-ORC1" "BIT-ORC2" "BIT-VECTOR"
           "BIT-VECTOR-P" "BIT-XOR" "BLOCK" "BOOLE" "BOOLE-1" "BOOLE-2" "BOOLE-AND" "BOOLE-ANDC1"
           "BOOLE-ANDC2" "BOOLE-C1" "BOOLE-C2" "BOOLE-CLR" "BOOLE-EQV" "BOOLE-IOR" "BOOLE-NAND"
           "BOOLE-NOR" "BOOLE-ORC1" "BOOLE-ORC2" "BOOLE-SET" "BOOLE-XOR" "BOOLEAN" "BOTH-CASE-P"
           "BOUNDP" "BREAK" "BROADCAST-STREAM" "BROADCAST-STREAM-STREAMS" "BUILT-IN-CLASS" "BUTLAST"
           "BYTE" "BYTE-POSITION" "BYTE-SIZE" "CAAAAR" "CAAADR" "CAAAR" "CAADAR" "CAADDR" "CAADR"
           "CAAR" "CADAAR" "CADADR" "CADAR" "CADDAR" "CADDDR" "CADDR" "CADR" "CALL-ARGUMENTS-LIMIT"
           "CALL-METHOD" "CALL-NEXT-METHOD" "CAR" "CASE" "CATCH" "CCASE" "CDAAAR" "CDAADR" "CDAAR"
           "CDADAR" "CDADDR" "CDADR" "CDAR" "CDDAAR" "CDDADR" "CDDAR" "CDDDAR" "CDDDDR" "CDDDR"
           "CDDR" "CDR" "CEILING" "CELL-ERROR" "CELL-ERROR-NAME" "CERROR" "CHANGE-CLASS" "CHAR"
           "CHAR-CODE" "CHAR-CODE-LIMIT" "CHAR-DOWNCASE" "CHAR-EQUAL" "CHAR-GREATERP" "CHAR-INT"
           "CHAR-LESSP" "CHAR-NAME" "CHAR-NOT-EQUAL" "CHAR-NOT-GREATERP" "CHAR-NOT-LESSP"
           "CHAR-UPCASE" "CHAR/=" "CHAR<" "CHAR<=" "CHAR=" "CHAR>" "CHAR>=" "CHARACTER" "CHARACTERP"
           "CHECK-TYPE" "CIS" "CLASS" "CLASS-NAME" "CLASS-OF" "CLEAR-INPUT" "CLEAR-OUTPUT" "CLOSE"
           "CLRHASH" "CODE-CHAR" "COERCE" "COMPILATION-SPEED" "COMPILE" "COMPILE-FILE"
           "COMPILE-FILE-PATHNAME" "COMPILED-FUNCTION" "COMPILED-FUNCTION-P" "COMPILER-MACRO"
           "COMPILER-MACRO-FUNCTION" "COMPLEMENT" "COMPLEX" "COMPLEXP" "COMPUTE-APPLICABLE-METHODS"
           "COMPUTE-RESTARTS" "CONCATENATE" "CONCATENATED-STREAM" "CONCATENATED-STREAM-STREAMS"
           "COND" "CONDITION" "CONJUGATE" "CONS" "CONSP" "CONSTANTLY" "CONSTANTP" "CONTINUE"
           "CONTROL-ERROR" "COPY-ALIST" "COPY-LIST" "COPY-PPRINT-DISPATCH" "COPY-READTABLE"
           "COPY-SEQ" "COPY-STRUCTURE" "COPY-SYMBOL" "COPY-TREE" "COS" "COSH" "COUNT" "COUNT-IF"
           "COUNT-IF-NOT" "CTYPECASE" "DEBUG" "DECF" "DECLAIM" "DECLARATION" "DECLARE" "DECODE-FLOAT"
           "DECODE-UNIVERSAL-TIME" "DEFCLASS" "DEFCONSTANT" "DEFGENERIC" "DEFINE-COMPILER-MACRO"
           "DEFINE-CONDITION" "DEFINE-METHOD-COMBINATION" "DEFINE-MODIFY-MACRO"
           "DEFINE-SETF-EXPANDER" "DEFINE-SYMBOL-MACRO" "DEFMACRO" "DEFMETHOD" "DEFPACKAGE"
           "DEFPARAMETER" "DEFSETF" "DEFSTRUCT" "DEFTYPE" "DEFUN" "DEFVAR" "DELETE"
           "DELETE-DUPLICATES" "DELETE-FILE" "DELETE-IF" "DELETE-IF-NOT" "DELETE-PACKAGE"
           "DENOMINATOR" "DEPOSIT-FIELD" "DESCRIBE" "DESCRIBE-OBJECT" "DESTRUCTURING-BIND"
           "DIGIT-CHAR" "DIGIT-CHAR-P" "DIRECTORY" "DIRECTORY-NAMESTRING" "DISASSEMBLE"
           "DIVISION-BY-ZERO" "DO" "DO*" "DO-ALL-SYMBOLS" "DO-EXTERNAL-SYMBOLS" "DO-SYMBOLS"
           "DOCUMENTATION" "DOLIST" "DOTIMES" "DOUBLE-FLOAT" "DOUBLE-FLOAT-EPSILON"
           "DOUBLE-FLOAT-NEGATIVE-EPSILON" "DPB" "DRIBBLE" "DYNAMIC-EXTENT" "ECASE" "ECHO-STREAM"
           "ECHO-STREAM-INPUT-STREAM" "ECHO-STREAM-OUTPUT-STREAM" "ED" "EIGHTH" "ELT"
           "ENCODE-UNIVERSAL-TIME" "END-OF-FILE" "ENDP" "ENOUGH-NAMESTRING"
           "ENSURE-DIRECTORIES-EXIST" "ENSURE-GENERIC-FUNCTION" "EQ" "EQL" "EQUAL" "EQUALP" "ERROR"
           "ETYPECASE" "EVAL" "EVAL-WHEN" "EVENP" "EVERY" "EXP" "EXPORT" "EXPT" "EXTENDED-CHAR"
           "FBOUNDP" "FCEILING" "FDEFINITION" "FFLOOR" "FIFTH" "FILE-AUTHOR" "FILE-ERROR"
           "FILE-ERROR-PATHNAME" "FILE-LENGTH" "FILE-NAMESTRING" "FILE-POSITION" "FILE-STREAM"
           "FILE-STRING-LENGTH" "FILE-WRITE-DATE" "FILL" "FILL-POINTER" "FIND" "FIND-ALL-SYMBOLS"
           "FIND-CLASS" "FIND-IF" "FIND-IF-NOT" "FIND-METHOD" "FIND-PACKAGE" "FIND-RESTART"
           "FIND-SYMBOL" "FINISH-OUTPUT" "FIRST" "FIXNUM" "FLET" "FLOAT" "FLOAT-DIGITS"
           "FLOAT-PRECISION" "FLOAT-RADIX" "FLOAT-SIGN" "FLOATING-POINT-INEXACT"
           "FLOATING-POINT-INVALID-OPERATION" "FLOATING-POINT-OVERFLOW" "FLOATING-POINT-UNDERFLOW"
           "FLOATP" "FLOOR" "FMAKUNBOUND" "FORCE-OUTPUT" "FORMAT" "FORMATTER" "FOURTH" "FRESH-LINE"
           "FROUND" "FTRUNCATE" "FTYPE" "FUNCALL" "FUNCTION" "FUNCTION-KEYWORDS"
           "FUNCTION-LAMBDA-EXPRESSION" "FUNCTIONP" "GCD" "GENERIC-FUNCTION" "GENSYM" "GENTEMP" "GET"
           "GET-DECODED-TIME" "GET-DISPATCH-MACRO-CHARACTER" "GET-INTERNAL-REAL-TIME"
           "GET-INTERNAL-RUN-TIME" "GET-MACRO-CHARACTER" "GET-OUTPUT-STREAM-STRING" "GET-PROPERTIES"
           "GET-SETF-EXPANSION" "GET-UNIVERSAL-TIME" "GETF" "GETHASH" "GO" "GRAPHIC-CHAR-P"
           "HANDLER-BIND" "HANDLER-CASE" "HASH-TABLE" "HASH-TABLE-COUNT" "HASH-TABLE-P"
           "HASH-TABLE-REHASH-SIZE" "HASH-TABLE-REHASH-THRESHOLD" "HASH-TABLE-SIZE" "HASH-TABLE-TEST"
           "HOST-NAMESTRING" "IDENTITY" "IF" "IGNORABLE" "IGNORE" "IGNORE-ERRORS" "IMAGPART" "IMPORT"
           "IN-PACKAGE" "INCF" "INITIALIZE-INSTANCE" "INLINE" "INPUT-STREAM-P" "INSPECT" "INTEGER"
           "INTEGER-DECODE-FLOAT" "INTEGER-LENGTH" "INTEGERP" "INTERACTIVE-STREAM-P" "INTERN"
           "INTERNAL-TIME-UNITS-PER-SECOND" "INTERSECTION" "INVALID-METHOD-ERROR" "INVOKE-DEBUGGER"
           "INVOKE-RESTART" "INVOKE-RESTART-INTERACTIVELY" "ISQRT" "KEYWORD" "KEYWORDP" "LABELS"
           "LAMBDA" "LAMBDA-LIST-KEYWORDS" "LAMBDA-PARAMETERS-LIMIT" "LAST" "LCM" "LDB" "LDB-TEST"
           "LDIFF" "LEAST-NEGATIVE-DOUBLE-FLOAT" "LEAST-NEGATIVE-LONG-FLOAT"
           "LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT" "LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT"
           "LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT" "LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT"
           "LEAST-NEGATIVE-SHORT-FLOAT" "LEAST-NEGATIVE-SINGLE-FLOAT" "LEAST-POSITIVE-DOUBLE-FLOAT"
           "LEAST-POSITIVE-LONG-FLOAT" "LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT"
           "LEAST-POSITIVE-NORMALIZED-LONG-FLOAT" "LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT"
           "LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT" "LEAST-POSITIVE-SHORT-FLOAT"
           "LEAST-POSITIVE-SINGLE-FLOAT" "LENGTH" "LET" "LET*" "LISP-IMPLEMENTATION-TYPE"
           "LISP-IMPLEMENTATION-VERSION" "LIST" "LIST*" "LIST-ALL-PACKAGES" "LIST-LENGTH" "LISTEN"
           "LISTP" "LOAD" "LOAD-LOGICAL-PATHNAME-TRANSLATIONS" "LOAD-TIME-VALUE" "LOCALLY" "LOG"
           "LOGAND" "LOGANDC1" "LOGANDC2" "LOGBITP" "LOGCOUNT" "LOGEQV" "LOGICAL-PATHNAME"
           "LOGICAL-PATHNAME-TRANSLATIONS" "LOGIOR" "LOGNAND" "LOGNOR" "LOGNOT" "LOGORC1" "LOGORC2"
           "LOGTEST" "LOGXOR" "LONG-FLOAT" "LONG-FLOAT-EPSILON" "LONG-FLOAT-NEGATIVE-EPSILON"
           "LONG-SITE-NAME" "LOOP" "LOOP-FINISH" "LOWER-CASE-P" "MACHINE-INSTANCE" "MACHINE-TYPE"
           "MACHINE-VERSION" "MACRO-FUNCTION" "MACROEXPAND" "MACROEXPAND-1" "MACROLET" "MAKE-ARRAY"
           "MAKE-BROADCAST-STREAM" "MAKE-CONCATENATED-STREAM" "MAKE-CONDITION"
           "MAKE-DISPATCH-MACRO-CHARACTER" "MAKE-ECHO-STREAM" "MAKE-HASH-TABLE" "MAKE-INSTANCE"
           "MAKE-INSTANCES-OBSOLETE" "MAKE-LIST" "MAKE-LOAD-FORM" "MAKE-LOAD-FORM-SAVING-SLOTS"
           "MAKE-METHOD" "MAKE-PACKAGE" "MAKE-PATHNAME" "MAKE-RANDOM-STATE" "MAKE-SEQUENCE"
           "MAKE-STRING" "MAKE-STRING-INPUT-STREAM" "MAKE-STRING-OUTPUT-STREAM" "MAKE-SYMBOL"
           "MAKE-SYNONYM-STREAM" "MAKE-TWO-WAY-STREAM" "MAKUNBOUND" "MAP" "MAP-INTO" "MAPC" "MAPCAN"
           "MAPCAR" "MAPCON" "MAPHASH" "MAPL" "MAPLIST" "MASK-FIELD" "MAX" "MEMBER" "MEMBER-IF"
           "MEMBER-IF-NOT" "MERGE" "MERGE-PATHNAMES" "METHOD" "METHOD-COMBINATION"
           "METHOD-COMBINATION-ERROR" "METHOD-QUALIFIERS" "MIN" "MINUSP" "MISMATCH" "MOD"
           "MOST-NEGATIVE-DOUBLE-FLOAT" "MOST-NEGATIVE-FIXNUM" "MOST-NEGATIVE-LONG-FLOAT"
           "MOST-NEGATIVE-SHORT-FLOAT" "MOST-NEGATIVE-SINGLE-FLOAT" "MOST-POSITIVE-DOUBLE-FLOAT"
           "MOST-POSITIVE-FIXNUM" "MOST-POSITIVE-LONG-FLOAT" "MOST-POSITIVE-SHORT-FLOAT"
           "MOST-POSITIVE-SINGLE-FLOAT" "MUFFLE-WARNING" "MULTIPLE-VALUE-BIND" "MULTIPLE-VALUE-CALL"
           "MULTIPLE-VALUE-LIST" "MULTIPLE-VALUE-PROG1" "MULTIPLE-VALUE-SETQ" "MULTIPLE-VALUES-LIMIT"
           "NAME-CHAR" "NAMESTRING" "NBUTLAST" "NCONC" "NEXT-METHOD-P" "NIL" "NINTERSECTION" "NINTH"
           "NO-APPLICABLE-METHOD" "NO-NEXT-METHOD" "NOT" "NOTANY" "NOTEVERY" "NOTINLINE" "NRECONC"
           "NREVERSE" "NSET-DIFFERENCE" "NSET-EXCLUSIVE-OR" "NSTRING-CAPITALIZE" "NSTRING-DOWNCASE"
           "NSTRING-UPCASE" "NSUBLIS" "NSUBST" "NSUBST-IF" "NSUBST-IF-NOT" "NSUBSTITUTE"
           "NSUBSTITUTE-IF" "NSUBSTITUTE-IF-NOT" "NTH" "NTH-VALUE" "NTHCDR" "NULL" "NUMBER" "NUMBERP"
           "NUMERATOR" "NUNION" "ODDP" "OPEN" "OPEN-STREAM-P" "OPTIMIZE" "OR" "OTHERWISE"
           "OUTPUT-STREAM-P" "PACKAGE" "PACKAGE-ERROR" "PACKAGE-ERROR-PACKAGE" "PACKAGE-NAME"
           "PACKAGE-NICKNAMES" "PACKAGE-SHADOWING-SYMBOLS" "PACKAGE-USE-LIST" "PACKAGE-USED-BY-LIST"
           "PACKAGEP" "PAIRLIS" "PARSE-ERROR" "PARSE-INTEGER" "PARSE-NAMESTRING" "PATHNAME"
           "PATHNAME-DEVICE" "PATHNAME-DIRECTORY" "PATHNAME-HOST" "PATHNAME-MATCH-P" "PATHNAME-NAME"
           "PATHNAME-TYPE" "PATHNAME-VERSION" "PATHNAMEP" "PEEK-CHAR" "PHASE" "PI" "PLUSP" "POP"
           "POSITION" "POSITION-IF" "POSITION-IF-NOT" "PPRINT" "PPRINT-DISPATCH"
           "PPRINT-EXIT-IF-LIST-EXHAUSTED" "PPRINT-FILL" "PPRINT-INDENT" "PPRINT-LINEAR"
           "PPRINT-LOGICAL-BLOCK" "PPRINT-NEWLINE" "PPRINT-POP" "PPRINT-TAB" "PPRINT-TABULAR" "PRIN1"
           "PRIN1-TO-STRING" "PRINC" "PRINC-TO-STRING" "PRINT" "PRINT-NOT-READABLE"
           "PRINT-NOT-READABLE-OBJECT" "PRINT-OBJECT" "PRINT-UNREADABLE-OBJECT" "PROBE-FILE"
           "PROCLAIM" "PROG" "PROG*" "PROG1" "PROG2" "PROGN" "PROGRAM-ERROR" "PROGV" "PROVIDE"
           "PSETF" "PSETQ" "PUSH" "PUSHNEW" "QUOTE" "RANDOM" "RANDOM-STATE" "RANDOM-STATE-P" "RASSOC"
           "RASSOC-IF" "RASSOC-IF-NOT" "RATIO" "RATIONAL" "RATIONALIZE" "RATIONALP" "READ"
           "READ-BYTE" "READ-CHAR" "READ-CHAR-NO-HANG" "READ-DELIMITED-LIST" "READ-FROM-STRING"
           "READ-LINE" "READ-PRESERVING-WHITESPACE" "READ-SEQUENCE" "READER-ERROR" "READTABLE"
           "READTABLE-CASE" "READTABLEP" "REAL" "REALP" "REALPART" "REDUCE" "REINITIALIZE-INSTANCE"
           "REM" "REMF" "REMHASH" "REMOVE" "REMOVE-DUPLICATES" "REMOVE-IF" "REMOVE-IF-NOT"
           "REMOVE-METHOD" "REMPROP" "RENAME-FILE" "RENAME-PACKAGE" "REPLACE" "REQUIRE" "REST"
           "RESTART" "RESTART-BIND" "RESTART-CASE" "RESTART-NAME" "RETURN" "RETURN-FROM" "REVAPPEND"
           "REVERSE" "ROOM" "ROTATEF" "ROUND" "ROW-MAJOR-AREF" "RPLACA" "RPLACD" "SAFETY" "SATISFIES"
           "SBIT" "SCALE-FLOAT" "SCHAR" "SEARCH" "SECOND" "SEQUENCE" "SERIOUS-CONDITION" "SET"
           "SET-DIFFERENCE" "SET-DISPATCH-MACRO-CHARACTER" "SET-EXCLUSIVE-OR" "SET-MACRO-CHARACTER"
           "SET-PPRINT-DISPATCH" "SET-SYNTAX-FROM-CHAR" "SETF" "SETQ" "SEVENTH" "SHADOW"
           "SHADOWING-IMPORT" "SHARED-INITIALIZE" "SHIFTF" "SHORT-FLOAT" "SHORT-FLOAT-EPSILON"
           "SHORT-FLOAT-NEGATIVE-EPSILON" "SHORT-SITE-NAME" "SIGNAL" "SIGNED-BYTE" "SIGNUM"
           "SIMPLE-ARRAY" "SIMPLE-BASE-STRING" "SIMPLE-BIT-VECTOR" "SIMPLE-BIT-VECTOR-P"
           "SIMPLE-CONDITION" "SIMPLE-CONDITION-FORMAT-ARGUMENTS" "SIMPLE-CONDITION-FORMAT-CONTROL"
           "SIMPLE-ERROR" "SIMPLE-STRING" "SIMPLE-STRING-P" "SIMPLE-TYPE-ERROR" "SIMPLE-VECTOR"
           "SIMPLE-VECTOR-P" "SIMPLE-WARNING" "SIN" "SINGLE-FLOAT" "SINGLE-FLOAT-EPSILON"
           "SINGLE-FLOAT-NEGATIVE-EPSILON" "SINH" "SIXTH" "SLEEP" "SLOT-BOUNDP" "SLOT-EXISTS-P"
           "SLOT-MAKUNBOUND" "SLOT-MISSING" "SLOT-UNBOUND" "SLOT-VALUE" "SOFTWARE-TYPE"
           "SOFTWARE-VERSION" "SOME" "SORT" "SPACE" "SPECIAL" "SPECIAL-OPERATOR-P" "SPEED" "SQRT"
           "STABLE-SORT" "STANDARD" "STANDARD-CHAR" "STANDARD-CHAR-P" "STANDARD-CLASS"
           "STANDARD-GENERIC-FUNCTION" "STANDARD-METHOD" "STANDARD-OBJECT" "STEP" "STORAGE-CONDITION"
           "STORE-VALUE" "STREAM" "STREAM-ELEMENT-TYPE" "STREAM-ERROR" "STREAM-ERROR-STREAM"
           "STREAM-EXTERNAL-FORMAT" "STREAMP" "STRING" "STRING-CAPITALIZE" "STRING-DOWNCASE"
           "STRING-EQUAL" "STRING-GREATERP" "STRING-LEFT-TRIM" "STRING-LESSP" "STRING-NOT-EQUAL"
           "STRING-NOT-GREATERP" "STRING-NOT-LESSP" "STRING-RIGHT-TRIM" "STRING-STREAM" "STRING-TRIM"
           "STRING-UPCASE" "STRING/=" "STRING<" "STRING<=" "STRING=" "STRING>" "STRING>=" "STRINGP"
           "STRUCTURE" "STRUCTURE-CLASS" "STRUCTURE-OBJECT" "STYLE-WARNING" "SUBLIS" "SUBSEQ"
           "SUBSETP" "SUBST" "SUBST-IF" "SUBST-IF-NOT" "SUBSTITUTE" "SUBSTITUTE-IF"
           "SUBSTITUTE-IF-NOT" "SUBTYPEP" "SVREF" "SXHASH" "SYMBOL" "SYMBOL-FUNCTION"
           "SYMBOL-MACROLET" "SYMBOL-NAME" "SYMBOL-PACKAGE" "SYMBOL-PLIST" "SYMBOL-VALUE" "SYMBOLP"
           "SYNONYM-STREAM" "SYNONYM-STREAM-SYMBOL" "T" "TAGBODY" "TAILP" "TAN" "TANH" "TENTH"
           "TERPRI" "THE" "THIRD" "THROW" "TIME" "TRACE" "TRANSLATE-LOGICAL-PATHNAME"
           "TRANSLATE-PATHNAME" "TREE-EQUAL" "TRUENAME" "TRUNCATE" "TWO-WAY-STREAM"
           "TWO-WAY-STREAM-INPUT-STREAM" "TWO-WAY-STREAM-OUTPUT-STREAM" "TYPE" "TYPE-ERROR"
           "TYPE-ERROR-DATUM" "TYPE-ERROR-EXPECTED-TYPE" "TYPE-OF" "TYPECASE" "TYPEP" "UNBOUND-SLOT"
           "UNBOUND-SLOT-INSTANCE" "UNBOUND-VARIABLE" "UNDEFINED-FUNCTION" "UNEXPORT" "UNINTERN"
           "UNION" "UNLESS" "UNREAD-CHAR" "UNSIGNED-BYTE" "UNTRACE" "UNUSE-PACKAGE" "UNWIND-PROTECT"
           "UPDATE-INSTANCE-FOR-DIFFERENT-CLASS" "UPDATE-INSTANCE-FOR-REDEFINED-CLASS"
           "UPGRADED-ARRAY-ELEMENT-TYPE" "UPGRADED-COMPLEX-PART-TYPE" "UPPER-CASE-P" "USE-PACKAGE"
           "USE-VALUE" "USER-HOMEDIR-PATHNAME" "VALUES" "VALUES-LIST" "VARIABLE" "VECTOR"
           "VECTOR-POP" "VECTOR-PUSH" "VECTOR-PUSH-EXTEND" "VECTORP" "WARN" "WARNING" "WHEN"
           "WILD-PATHNAME-P" "WITH-ACCESSORS" "WITH-COMPILATION-UNIT" "WITH-CONDITION-RESTARTS"
           "WITH-HASH-TABLE-ITERATOR" "WITH-INPUT-FROM-STRING" "WITH-OPEN-FILE" "WITH-OPEN-STREAM"
           "WITH-OUTPUT-TO-STRING" "WITH-PACKAGE-ITERATOR" "WITH-SIMPLE-RESTART" "WITH-SLOTS"
           "WITH-STANDARD-IO-SYNTAX" "WRITE" "WRITE-BYTE" "WRITE-CHAR" "WRITE-LINE" "WRITE-SEQUENCE"
           "WRITE-STRING" "WRITE-TO-STRING" "Y-OR-N-P" "YES-OR-NO-P" "ZEROP"))
        (packages
         #+CLISP  '(:common-lisp :clos)
         #+GCL    '(:lisp :pcl)
         #-(OR CLISP GCL) '(:common-lisp))
        (gray-symbols
         '("FUNDAMENTAL-STREAM"
           "FUNDAMENTAL-INPUT-STREAM"
           "FUNDAMENTAL-OUTPUT-STREAM"
           "FUNDAMENTAL-CHARACTER-STREAM"
           "FUNDAMENTAL-BINARY-STREAM"
           "FUNDAMENTAL-CHARACTER-INPUT-STREAM"
           "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
           "FUNDAMENTAL-BINARY-INPUT-STREAM"
           "FUNDAMENTAL-BINARY-OUTPUT-STREAM"
           "STREAM-READ-CHAR"
           "STREAM-UNREAD-CHAR"
           "STREAM-READ-CHAR-NO-HANG"
           "STREAM-PEEK-CHAR"
           "STREAM-LISTEN"
           "STREAM-READ-LINE"
           "STREAM-CLEAR-INPUT"
           "STREAM-WRITE-CHAR"
           "STREAM-LINE-COLUMN"
           "STREAM-START-LINE-P"
           "STREAM-WRITE-STRING"
           "STREAM-TERPRI"
           "STREAM-FRESH-LINE"
           "STREAM-FINISH-OUTPUT"
           "STREAM-FORCE-OUTPUT"
           "STREAM-ADVANCE-TO-COLUMN"
           "STREAM-CLEAR-INPUT"
           "STREAM-READ-BYTE"
           "STREAM-WRITE-BYTE" ))
        (gray-packages
         `(#+:CLISP                 ,@'("LISP")
           #+:CMU                   ,@'("EXT")
           #+:ALLEGRO               ,@'("COMMON-LISP" "EXCL" "STREAM")
           #+:HARLEQUIN-COMMON-LISP ,@'("STREAM")
           #+:SBCL                  ,@'("SB-GRAY"))) )
    ;;
    (labels ((seek-symbol (name packages)
               ;; Seek the a symbol named 'name' in `packages'
               (or (some #'(lambda (p) 
                             (multiple-value-bind (sym res) (find-symbol name p)
                               (if (eql res :external)
                                   (list sym)
                                   nil)))
                         packages)
                   (progn (format T "~&There is no ~A." name)
                          (finish-output)
                          nil)))
             (dump-defpackage (&aux imports export-ansi export-gray)
               (labels ((grok (symbols packages)
                          (let ((res nil))
                            (dolist (nam symbols)
                              (let ((sym (seek-symbol nam packages)))
                                (when sym
                                  (push (car sym) res)
                                  (cond
                                    ((and (find-package :clim-lisp-patch)
                                          (multiple-value-bind (sym2 res) (find-symbol nam :clim-lisp-patch)
                                            (and sym2 (eq res :external))))
                                     ;;
                                     (format T "~&;; ~S is patched." sym)
                                     (finish-output)
                                     (push `(:import-from
                                             :clim-lisp-patch
                                             ,nam)
                                           imports) )
                                    (t
                                     (setf sym (car sym))
                                     ;; CLISP has no (:import ..) ARG!
                                     (push `(:import-from
                                             ,(package-name (symbol-package sym))
                                             ,(symbol-name sym))
                                           imports))))))
                            res)))
                 (setf export-ansi (grok all-ansi-symbols packages))
                 (setf export-gray (grok gray-symbols gray-packages))
                 `(progn
                   (defpackage "CLIM-LISP" (:use)
                     ,@imports
                     (:export
                      ,@(mapcar #'symbol-name export-ansi)
                      ,@(mapcar #'symbol-name export-gray) )) ))))
      (dump-defpackage) ))

(defpackage "CLIM"
  (:use)
  ;;
  (:import-from "CLIM-LISP"
   "AND" 
   "BOOLEAN" 
   "CHARACTER" 
   "CLOSE" 
   "COMPLEX" 
   "FLOAT" 
   "FUNDAMENTAL-BINARY-INPUT-STREAM" 
   "FUNDAMENTAL-BINARY-OUTPUT-STREAM" 
   "FUNDAMENTAL-BINARY-STREAM" 
   "FUNDAMENTAL-CHARACTER-INPUT-STREAM" 
   "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM" 
   "FUNDAMENTAL-CHARACTER-STREAM" 
   "FUNDAMENTAL-INPUT-STREAM" 
   "FUNDAMENTAL-OUTPUT-STREAM" 
   "FUNDAMENTAL-STREAM" 
   "INPUT-STREAM-P" 
   "INTEGER" 
   "INTERACTIVE-STREAM-P" 
   "KEYWORD" 
   "MEMBER"
   "NIL"
   "NULL" 
   "NUMBER" 
   "OPEN-STREAM-P" 
   "OR" 
   "OUTPUT-STREAM-P" 
   "PATHNAME" 
   "RATIO" 
   "RATIONAL" 
   "REAL" 
   "SEQUENCE" 
   "STREAM-ADVANCE-TO-COLUMN" 
   "STREAM-CLEAR-INPUT" 
   "STREAM-ELEMENT-TYPE" 
   "STREAM-FINISH-OUTPUT" 
   "STREAM-FORCE-OUTPUT" 
   "STREAM-FRESH-LINE" 
   "STREAM-LINE-COLUMN" 
   "STREAM-LISTEN" 
   "STREAM-PEEK-CHAR" 
   "STREAM-READ-BYTE" 
   "STREAM-READ-CHAR" 
   "STREAM-READ-CHAR-NO-HANG" 
   "STREAM-READ-LINE" 
   "STREAM-START-LINE-P" 
   "STREAM-TERPRI" 
   "STREAM-UNREAD-CHAR" 
   "STREAM-WRITE-BYTE" 
   "STREAM-WRITE-CHAR" 
   "STREAM-WRITE-STRING" 
   "STREAMP" 
   "STRING" 
   "SYMBOL" 
   "T")
  ;;
  (:export
   "*ABORT-GESTURES*"                   ;variable
   "*ACCELERATOR-GESTURES*"             ;variable
   "*ACTIVATION-GESTURES*"              ;variable
   "*APPLICATION-FRAME*"                ;variable
   "*COMMAND-ARGUMENT-DELIMITERS*"      ;variable
   "*COMMAND-DISPATCHERS*"              ;variable
   "*COMMAND-NAME-DELIMITERS*"          ;variable
   "*COMMAND-PARSER*"                   ;variable
   "*COMMAND-UNPARSER*"                 ;variable
   "*COMPLETION-GESTURES*"              ;variable
   "*DEFAULT-FRAME-MANAGER*"            ;variable
   "*DEFAULT-SERVER-PATH*"              ;variable
   "*DEFAULT-TEXT-STYLE*"               ;constant
   "*DELIMITER-GESTURES*"               ;variable
   "*HELP-GESTURES*"                    ;variable
   "*INPUT-CONTEXT*"                    ;variable
   "*INPUT-WAIT-HANDLER*"               ;variable
   "*INPUT-WAIT-TEST*"                  ;variable
   "*NULL-PRESENTATION*"                ;constant
   "*NUMERIC-ARGUMENT-MARKER*"          ;variable
   "*ORIGINAL-STREAM*"                  ;variable
   "*PARTIAL-COMMAND-PARSER*"           ;variable
   "*POINTER-BUTTON-PRESS-HANDLER*"     ;variable
   "*POINTER-DOCUMENTATION-OUTPUT*"     ;variable
   "*POSSIBILITIES-GESTURES*"           ;variable
   "*STANDARD-ACTIVATION-GESTURES*"     ;variable
   "*UNDEFINED-TEXT-STYLE*"             ;constant
   "*UNSUPPLIED-ARGUMENT-MARKER*"       ;variable
   "+BACKGROUND-INK+"                   ;constant
   "+BLACK+"                            ;constant
   "+BLUE+"                             ;constant
   "+CONTROL-KEY+"                      ;constant
   "+CYAN+"                             ;constant
   "+EVERYWHERE+"                       ;constant
   "+FILL+"                             ;constant
   "+FLIPPING-INK+"                     ;constant
   "+FOREGROUND-INK+"                   ;constant
   "+GADGET-DIALOG-VIEW+"               ;constant
   "+GADGET-MENU-VIEW+"                 ;constant
   "+GADGET-VIEW+"                      ;constant
   "+GREEN+"                            ;constant
   "+HYPER-KEY+"                        ;constant
   "+IDENTITY-TRANSFORMATION+"          ;constant
   "+MAGENTA+"                          ;constant
   "+META-KEY+"                         ;constant
   "+NOWHERE+"                          ;constant
   "+POINTER-DOCUMENTATION-VIEW+"       ;constant
   "+POINTER-LEFT-BUTTON+"              ;constant
   "+POINTER-MIDDLE-BUTTON+"            ;constant
   "+POINTER-RIGHT-BUTTON+"             ;constant
   "+RED+"                              ;constant
   "+SHIFT-KEY+"                        ;constant
   "+SUPER-KEY+"                        ;constant
   "+TEXTUAL-DIALOG-VIEW+"              ;constant
   "+TEXTUAL-MENU-VIEW+"                ;constant
   "+TEXTUAL-VIEW+"                     ;constant
   "+TRANSPARENT-INK+"                  ;constant
   "+WHITE+"                            ;constant
   "+YELLOW+"                           ;constant
   "ABORT-GESTURE"                      ;condition
   "ABORT-GESTURE-EVENT"                ;generic function
   "ACCELERATOR-GESTURE"                ;condition
   "ACCELERATOR-GESTURE-EVENT"          ;generic function
   "ACCELERATOR-GESTURE-NUMERIC-ARGUMENT" ;generic function
   "ACCEPT"                             ;presentation method
   "ACCEPT"                             ;function
   "ACCEPT-1"                           ;function
   "ACCEPT-FROM-STRING"                 ;function
   "ACCEPT-PRESENT-DEFAULT"             ;presentation method
   "ACCEPT-VALUES"                      ;frame
   "ACCEPT-VALUES-COMMAND-BUTTON"       ;macro
   "ACCEPT-VALUES-RESYNCHRONIZE"        ;generic function
   "ACCEPTING-VALUES"                   ;macro
   "ACTION-GADGET"                      ;class
   "ACTIVATE-CALLBACK"                  ;callback
   "ACTIVATE-GADGET"                    ;generic function
   "ACTIVATION-GESTURE-P"               ;function
   "ADD-CHARACTER-OUTPUT-TO-TEXT-RECORD" ;generic function
   "ADD-COMMAND-TO-COMMAND-TABLE"       ;function
   "ADD-GESTURE-NAME"                   ;function
   "ADD-INPUT-EDITOR-COMMAND"           ;function
   "ADD-KEYSTROKE-TO-COMMAND-TABLE"     ;function
   "ADD-MENU-ITEM-TO-COMMAND-TABLE"     ;function
   "ADD-OUTPUT-RECORD"                  ;generic function
   "ADD-PRESENTATION-TRANSLATOR-TO-COMMAND-TABLE" ;function
   "ADD-STRING-OUTPUT-TO-TEXT-RECORD"   ;generic function
   "ADJUST-ITEM-LIST-CELLS"             ;generic function
   "ADJUST-MULTIPLE-COLUMNS"            ;generic function
   "ADJUST-TABLE-CELLS"                 ;generic function
   "ADOPT-FRAME"                        ;generic function
   "ALLOCATE-MEDIUM"                    ;generic function
   "ALLOCATE-PIXMAP"                    ;generic function
   "ALLOCATE-SPACE"                     ;generic function
   "AND"                                ;presentation type
   "APPLICATION-FRAME"                  ;protocol class
   "APPLICATION-FRAME-P"                ;predicate
   "APPLICATION-PANE"                   ;pane
   "APPLY-PRESENTATION-GENERIC-FUNCTION" ;macro
   "AREA"                               ;protocol class
   "AREAP"                              ;predicate
   "ARMED-CALLBACK"                     ;callback
   "AUGMENT-DRAW-SET"                   ;generic function
   "BASIC-GADGET"                       ;class
   "BASIC-MEDIUM"                       ;class
   "BASIC-PANE"                         ;class
   "BASIC-PORT"                         ;class
   "BASIC-SHEET"                        ;class
   "BBOARD-PANE"                        ;pane
   "BEEP"                               ;generic function
   "BLANK-AREA"                         ;presentation type
   "BOOLEAN"                            ;presentation type
   "BOUNDING-RECTANGLE"                 ;protocol class
   "BOUNDING-RECTANGLE"                 ;generic function
   "BOUNDING-RECTANGLE*"                ;generic function
   "BOUNDING-RECTANGLE-HEIGHT"          ;generic function
   "BOUNDING-RECTANGLE-MAX-X"           ;generic function
   "BOUNDING-RECTANGLE-MAX-Y"           ;generic function
   "BOUNDING-RECTANGLE-MIN-X"           ;generic function
   "BOUNDING-RECTANGLE-MIN-Y"           ;generic function
   "BOUNDING-RECTANGLE-P"               ;predicate
   "BOUNDING-RECTANGLE-POSITION"        ;generic function
   "BOUNDING-RECTANGLE-SIZE"            ;generic function
   "BOUNDING-RECTANGLE-WIDTH"           ;generic function
   "BURY-FRAME"                         ;generic function
   "BURY-MIRROR"                        ;generic function
   "BURY-SHEET"                         ;generic function
   "CACHE-OUTPUT-RECORD"                ;generic function
   "CALL-PRESENTATION-MENU"             ;function
   "CALL-PRESENTATION-TRANSLATOR"       ;function
   "CELL-ALIGN-X"                       ;generic function
   "CELL-ALIGN-Y"                       ;generic function
   "CELL-MIN-HEIGHT"                    ;generic function
   "CELL-MIN-WIDTH"                     ;generic function
   "CELL-OUTPUT-RECORD"                 ;protocol class
   "CELL-OUTPUT-RECORD-P"               ;predicate
   "CHANGE-SPACE-REQUIREMENTS"          ;generic function
   "CHANGING-SPACE-REQUIREMENTS"        ;macro
   "CHARACTER"                          ;presentation type
   "CHECK-BOX"                          ;class
   "CHECK-BOX-CURRENT-SELECTION"        ;generic function
   "CHECK-BOX-PANE"                     ;class
   "CHECK-BOX-SELECTIONS"               ;generic function
   "CHILD-CONTAINING-POSITION"          ;generic function
   "CHILDREN-OVERLAPPING-RECTANGLE*"    ;generic function
   "CHILDREN-OVERLAPPING-REGION"        ;generic function
   "CLASS-PRESENTATION-TYPE-NAME"       ;function
   "CLEAR-OUTPUT-RECORD"                ;generic function
   "CLIENT-SETTING"                     ;setf method (through no reader)
   "CLIM-STREAM-PANE"                   ;pane
   "CLOSE"                              ;generic function
   "COLOR"                              ;protocol class
   "COLOR-IHS"                          ;generic function
   "COLOR-RGB"                          ;generic function
   "COLORP"                             ;predicate
   "COLUMN-OUTPUT-RECORD"               ;protocol class
   "COLUMN-OUTPUT-RECORD-P"             ;predicate
   "COMMAND"                            ;presentation type
   "COMMAND-ACCESSIBLE-IN-COMMAND-TABLE-P" ;function
   "COMMAND-ALREADY-PRESENT"            ;error
   "COMMAND-ARGUMENTS"                  ;function
   "COMMAND-ENABLED"                    ;generic function
   "COMMAND-LINE-COMMAND-PARSER"        ;function
   "COMMAND-LINE-COMMAND-UNPARSER"      ;function
   "COMMAND-LINE-NAME-FOR-COMMAND"      ;function
   "COMMAND-LINE-READ-REMAINING-ARGUMENTS-FOR-PARTIAL-COMMAND" ;function
   "COMMAND-MENU-ITEM-OPTIONS"          ;function
   "COMMAND-MENU-ITEM-TYPE"             ;function
   "COMMAND-MENU-ITEM-VALUE"            ;function
   "COMMAND-MENU-PANE"                  ;pane
   "COMMAND-NAME"                       ;presentation type
   "COMMAND-NAME"                       ;function
   "COMMAND-NAME-FROM-SYMBOL"           ;function
   "COMMAND-NOT-ACCESSIBLE"             ;error
   "COMMAND-NOT-PRESENT"                ;error
   "COMMAND-OR-FORM"                    ;presentation type
   "COMMAND-PRESENT-IN-COMMAND-TABLE-P" ;function
   "COMMAND-TABLE"                      ;protocol class
   "COMMAND-TABLE-ALREADY-EXISTS"       ;error
   "COMMAND-TABLE-COMPLETE-INPUT"       ;function
   "COMMAND-TABLE-ERROR"                ;error
   "COMMAND-TABLE-INHERIT-FROM"         ;generic function
   "COMMAND-TABLE-NAME"                 ;generic function
   "COMMAND-TABLE-NOT-FOUND"            ;error
   "COMMAND-TABLE-P"                    ;predicate
   "COMPLETE-FROM-GENERATOR"            ;function
   "COMPLETE-FROM-POSSIBILITIES"        ;function
   "COMPLETE-INPUT"                     ;function
   "COMPLETING-FROM-SUGGESTIONS"        ;macro
   "COMPLETION"                         ;presentation type
   "COMPLEX"                            ;presentation type
   "COMPOSE-IN"                         ;generic function
   "COMPOSE-OUT"                        ;generic function
   "COMPOSE-OVER"                       ;generic function
   "COMPOSE-ROTATION-WITH-TRANSFORMATION" ;function
   "COMPOSE-SCALING-WITH-TRANSFORMATION" ;function
   "COMPOSE-SPACE"                      ;generic function
   "COMPOSE-TRANSFORMATION-WITH-ROTATION" ;function
   "COMPOSE-TRANSFORMATION-WITH-SCALING" ;function
   "COMPOSE-TRANSFORMATION-WITH-TRANSLATION" ;function
   "COMPOSE-TRANSFORMATIONS"            ;generic function
   "COMPOSE-TRANSLATION-WITH-TRANSFORMATION" ;function
   "COMPUTE-DIFFERENCE-SET"             ;generic function
   "COMPUTE-NEW-OUTPUT-RECORDS"         ;generic function
   "CONTRASTING-DASH-PATTERN-LIMIT"     ;generic function
   "CONTRASTING-INKS-LIMIT"             ;generic function
   "COORDINATE"                         ;type
   "COORDINATE"                         ;function
   "COPY-AREA"                          ;generic function
   "COPY-FROM-PIXMAP"                   ;function
   "COPY-TEXTUAL-OUTPUT-HISTORY"        ;function
   "COPY-TO-PIXMAP"                     ;function
   "CURSOR"                             ;protocol class
   "CURSOR-ACTIVE"                      ;generic function
   "CURSOR-FOCUS"                       ;generic function
   "CURSOR-POSITION"                    ;generic function
   "CURSOR-SHEET"                       ;generic function
   "CURSOR-STATE"                       ;generic function
   "CURSOR-VISIBILITY"                  ;generic function
   "CURSORP"                            ;predicate
   "DEACTIVATE-GADGET"                  ;generic function
   "DEALLOCATE-MEDIUM"                  ;generic function
   "DEALLOCATE-PIXMAP"                  ;generic function
   "DECACHE-CHILD-OUTPUT-RECORD"        ;generic function
   "DEFAULT-DESCRIBE-PRESENTATION-TYPE" ;function
   "DEFAULT-FRAME-TOP-LEVEL"            ;generic function
   "DEFINE-APPLICATION-FRAME"           ;macro
   "DEFINE-BORDER-TYPE"                 ;macro
   "DEFINE-COMMAND"                     ;macro
   "DEFINE-COMMAND-TABLE"               ;macro
   "DEFINE-DEFAULT-PRESENTATION-METHOD" ;macro
   "DEFINE-DRAG-AND-DROP-TRANSLATOR"    ;macro
   "DEFINE-GESTURE-NAME"                ;macro
   "DEFINE-GRAPH-TYPE"                  ;macro
   "DEFINE-PRESENTATION-ACTION"         ;macro
   "DEFINE-PRESENTATION-GENERIC-FUNCTION" ;macro
   "DEFINE-PRESENTATION-METHOD"         ;macro
   "DEFINE-PRESENTATION-TO-COMMAND-TRANSLATOR" ;macro
   "DEFINE-PRESENTATION-TRANSLATOR"     ;macro
   "DEFINE-PRESENTATION-TYPE"           ;macro
   "DEFINE-PRESENTATION-TYPE-ABBREVIATION" ;macro
   "DEGRAFT-MEDIUM"                     ;generic function
   "DELEGATE-SHEET-DELEGATE"            ;generic function
   "DELEGATE-SHEET-INPUT-MIXIN"         ;class
   "DELETE-GESTURE-NAME"                ;function
   "DELETE-OUTPUT-RECORD"               ;generic function
   "DELIMITER-GESTURE-P"                ;function
   "DESCRIBE-PRESENTATION-TYPE"         ;presentation method
   "DESCRIBE-PRESENTATION-TYPE"         ;function
   "DESIGN"                             ;protocol class
   "DESIGNP"                            ;predicate
   "DESTROY-FRAME"                      ;generic function
   "DESTROY-MIRROR"                     ;generic function
   "DESTROY-PORT"                       ;generic function
   "DEVICE-EVENT"                       ;class
   "DISABLE-FRAME"                      ;generic function
   "DISARMED-CALLBACK"                  ;callback
   "DISOWN-FRAME"                       ;generic function
   "DISPATCH-EVENT"                     ;generic function
   "DISPLAY-COMMAND-MENU"               ;generic function
   "DISPLAY-COMMAND-TABLE-MENU"         ;generic function
   "DISPLAY-EXIT-BOXES"                 ;generic function
   "DISPLAYED-OUTPUT-RECORD"            ;protocol class
   "DISPLAYED-OUTPUT-RECORD-INK"        ;generic function
   "DISPLAYED-OUTPUT-RECORD-P"          ;predicate
   "DISTRIBUTE-EVENT"                   ;generic function
   "DO-COMMAND-TABLE-INHERITANCE"       ;macro
   "DOCUMENT-PRESENTATION-TRANSLATOR"   ;function
   "DRAG-CALLBACK"                      ;callback
   "DRAG-CALLBACK"                      ;callback
   "DRAG-OUTPUT-RECORD"                 ;generic function
   "DRAGGING-OUTPUT"                    ;macro
   "DRAW-ARROW"                         ;function
   "DRAW-ARROW*"                        ;function
   "DRAW-CIRCLE"                        ;function
   "DRAW-CIRCLE*"                       ;function
   "DRAW-DESIGN"                        ;generic function
   "DRAW-ELLIPSE"                       ;function
   "DRAW-ELLIPSE*"                      ;function
   "DRAW-LINE"                          ;function
   "DRAW-LINE*"                         ;function
   "DRAW-LINES"                         ;function
   "DRAW-LINES*"                        ;function
   "DRAW-OVAL"                          ;function
   "DRAW-OVAL*"                         ;function
   "DRAW-PATTERN*"                      ;function
   "DRAW-POINT"                         ;function
   "DRAW-POINT*"                        ;function
   "DRAW-POINTS"                        ;function
   "DRAW-POINTS*"                       ;function
   "DRAW-POLYGON"                       ;function
   "DRAW-POLYGON*"                      ;function
   "DRAW-RECTANGLE"                     ;function
   "DRAW-RECTANGLE*"                    ;function
   "DRAW-RECTANGLES"                    ;function
   "DRAW-RECTANGLES*"                   ;function
   "DRAW-STANDARD-MENU"                 ;function
   "DRAW-TEXT"                          ;function
   "DRAW-TEXT*"                         ;function
   "ELLIPSE"                            ;protocol class
   "ELLIPSE-CENTER-POINT"               ;generic function
   "ELLIPSE-CENTER-POINT*"              ;generic function
   "ELLIPSE-END-ANGLE"                  ;generic function
   "ELLIPSE-RADII"                      ;generic function
   "ELLIPSE-START-ANGLE"                ;generic function
   "ELLIPSEP"                           ;predicate
   "ELLIPTICAL-ARC"                     ;protocol class
   "ELLIPTICAL-ARC-P"                   ;predicate
   "ENABLE-FRAME"                       ;generic function
   "ENCAPSULATING-STREAM"               ;protocol class
   "ENCAPSULATING-STREAM-P"             ;predicate
   "ENCAPSULATING-STREAM-STREAM"        ;generic function
   "ENGRAFT-MEDIUM"                     ;generic function
   "ERASE-INPUT-BUFFER"                 ;generic function
   "ERASE-OUTPUT-RECORD"                ;generic function
   "EVEN-SCALING-TRANSFORMATION-P"      ;generic function
   "EVENT"                              ;protocol class
   "EVENT-LISTEN"                       ;generic function
   "EVENT-MATCHES-GESTURE-NAME-P"       ;function
   "EVENT-MODIFIER-STATE"               ;generic function
   "EVENT-PEEK"                         ;generic function
   "EVENT-READ"                         ;generic function
   "EVENT-READ-NO-HANG"                 ;generic function
   "EVENT-SHEET"                        ;generic function
   "EVENT-TIMESTAMP"                    ;generic function
   "EVENT-TYPE"                         ;generic function
   "EVENT-UNREAD"                       ;generic function
   "EVENTP"                             ;predicate
   "EXECUTE-FRAME-COMMAND"              ;generic function
   "EXPAND-PRESENTATION-TYPE-ABBREVIATION" ;function
   "EXPAND-PRESENTATION-TYPE-ABBREVIATION-1" ;function
   "EXPRESSION"                         ;presentation type
   "EXTENDED-INPUT-STREAM"              ;protocol class
   "EXTENDED-INPUT-STREAM-P"            ;predicate
   "EXTENDED-OUTPUT-STREAM"             ;protocol class
   "EXTENDED-OUTPUT-STREAM-P"           ;predicate
   "FILLING-OUTPUT"                     ;macro
   "FIND-APPLICABLE-TRANSLATORS"        ;function
   "FIND-CACHED-OUTPUT-RECORD"          ;generic function
   "FIND-CHILD-OUTPUT-RECORD"           ;generic function
   "FIND-COMMAND-FROM-COMMAND-LINE-NAME" ;function
   "FIND-COMMAND-TABLE"                 ;function
   "FIND-FRAME-MANAGER"                 ;function
   "FIND-GRAFT"                         ;function
   "FIND-INNERMOST-APPLICABLE-PRESENTATION" ;function
   "FIND-KEYSTROKE-ITEM"                ;function
   "FIND-MENU-ITEM"                     ;function
   "FIND-PANE-FOR-FRAME"                ;generic function
   "FIND-PANE-NAMED"                    ;generic function
   "FIND-PORT"                          ;function
   "FIND-PRESENTATION-TRANSLATOR"       ;function
   "FIND-PRESENTATION-TRANSLATORS"      ;function
   "FIND-PRESENTATION-TYPE-CLASS"       ;function
   "FLOAT"                              ;presentation type
   "FORM"                               ;presentation type
   "FORMAT-GRAPH-FROM-ROOTS"            ;function
   "FORMAT-ITEMS"                       ;function
   "FORMAT-TEXTUAL-LIST"                ;function
   "FORMATTING-CELL"                    ;macro
   "FORMATTING-COLUMN"                  ;macro
   "FORMATTING-ITEM-LIST"               ;macro
   "FORMATTING-ROW"                     ;macro
   "FORMATTING-TABLE"                   ;macro
   "FRAME-ALL-LAYOUTS"                  ;generic function
   "FRAME-CALLING-FRAME"                ;generic function
   "FRAME-COMMAND-TABLE"                ;generic function
   "FRAME-CURRENT-LAYOUT"               ;generic function
   "FRAME-CURRENT-PANES"                ;generic function
   "FRAME-DOCUMENT-HIGHLIGHTED-PRESENTATION" ;generic function
   "FRAME-DRAG-AND-DROP-FEEDBACK"       ;generic function
   "FRAME-DRAG-AND-DROP-HIGHLIGHTING"   ;generic function
   "FRAME-ERROR-OUTPUT"                 ;generic function
   "FRAME-EXIT"                         ;condition
   "FRAME-EXIT"                         ;generic function
   "FRAME-EXIT-FRAME"                   ;generic function
   "FRAME-FIND-INNERMOST-APPLICABLE-PRESENTATION" ;generic function
   "FRAME-INPUT-CONTEXT-BUTTON-PRESS-HANDLER" ;generic function
   "FRAME-MAINTAIN-PRESENTATION-HISTORIES" ;generic function
   "FRAME-MANAGER"                      ;protocol class
   "FRAME-MANAGER"                      ;generic function
   "FRAME-MANAGER-FRAMES"               ;generic function
   "FRAME-MANAGER-MENU-CHOOSE"          ;generic function
   "FRAME-MANAGER-NOTIFY-USER"          ;generic function
   "FRAME-MANANGER-P"                   ;predicate
   "FRAME-NAME"                         ;generic function
   "FRAME-PANES"                        ;generic function
   "FRAME-PARENT"                       ;generic function
   "FRAME-POINTER-DOCUMENTATION-OUTPUT" ;generic function
   "FRAME-PRETTY-NAME"                  ;generic function
   "FRAME-PROPERTIES"                   ;generic function
   "FRAME-QUERY-IO"                     ;generic function
   "FRAME-REPLAY"                       ;generic function
   "FRAME-STANDARD-INPUT"               ;generic function
   "FRAME-STANDARD-OUTPUT"              ;generic function
   "FRAME-STATE"                        ;generic function
   "FRAME-TOP-LEVEL-SHEET"              ;generic function
   "FUNCALL-PRESENTATION-GENERIC-FUNCTION" ;macro
   "FUNDAMENTAL-BINARY-INPUT-STREAM"    ;class
   "FUNDAMENTAL-BINARY-OUTPUT-STREAM"   ;class
   "FUNDAMENTAL-BINARY-STREAM"          ;class
   "FUNDAMENTAL-CHARACTER-INPUT-STREAM" ;class
   "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM" ;class
   "FUNDAMENTAL-CHARACTER-STREAM"       ;class
   "FUNDAMENTAL-INPUT-STREAM"           ;class
   "FUNDAMENTAL-OUTPUT-STREAM"          ;class
   "FUNDAMENTAL-STREAM"                 ;class
   "GADGET"                             ;protocol class
   "GADGET-ACTIVATE-CALLBACK"           ;generic function
   "GADGET-ACTIVE-P"                    ;generic function
   "GADGET-ARMED-CALLBACK"              ;generic function
   "GADGET-CLIENT"                      ;generic function
   "GADGET-DIALOG-VIEW"                 ;class
   "GADGET-DISARMED-CALLBACK"           ;generic function
   "GADGET-ID"                          ;generic function
   "GADGET-LABEL"                       ;generic function
   "GADGET-LABEL-ALIGN-X"               ;generic function
   "GADGET-LABEL-ALIGN-Y"               ;generic function
   "GADGET-MAX-VALUE"                   ;generic function
   "GADGET-MENU-VIEW"                   ;class
   "GADGET-MIN-VALUE"                   ;generic function
   "GADGET-ORIENTATION"                 ;generic function
   "GADGET-OUTPUT-RECORD"               ;class
   "GADGET-RANGE"                       ;generic function
   "GADGET-RANGE*"                      ;generic function
   "GADGET-SHOW-VALUE-P"                ;generic function
   "GADGET-VALUE"                       ;generic function
   "GADGET-VALUE-CHANGED-CALLBACK"      ;generic function
   "GADGET-VIEW"                        ;class
   "GADGETP"                            ;predicate
   "GENERATE-GRAPH-NODES"               ;generic function
   "GENERATE-PANES"                     ;generic function
   "GENERIC-LIST-PANE"                  ;class
   "GENERIC-OPTION-PANE"                ;class
   "GET-FRAME-PANE"                     ;generic function
   "GLOBAL-COMMAND-TABLE"               ;command table
   "GRAFT"                              ;generic function
   "GRAFT-HEIGHT"                       ;generic function
   "GRAFT-ORIENTATION"                  ;generic function
   "GRAFT-PIXELS-PER-INCH"              ;function
   "GRAFT-PIXELS-PER-MILLIMETER"        ;function
   "GRAFT-UNITS"                        ;generic function
   "GRAFT-WIDTH"                        ;generic function
   "GRAPH-NODE-CHILDREN"                ;generic function
   "GRAPH-NODE-OBJECT"                  ;generic function
   "GRAPH-NODE-OUTPUT-RECORD"           ;protocol class
   "GRAPH-NODE-OUTPUT-RECORD-P"         ;predicate
   "GRAPH-NODE-PARENTS"                 ;generic function
   "GRAPH-OUTPUT-RECORD"                ;protocol class
   "GRAPH-OUTPUT-RECORD-P"              ;predicate
   "GRAPH-ROOT-NODES"                   ;generic function
   "GRAPHICS-DISPLAYED-OUTPUT-RECORD"   ;protocol class
   "GRAPHICS-DISPLAYED-OUTPUT-RECORD-P" ;predicate
   "GRID-PANE"                          ;pane
   "HANDLE-EVENT"                       ;generic function
   "HANDLE-REPAINT"                     ;generic function
   "HBOX-PANE"                          ;pane
   "HIGHLIGHT-APPLICABLE-PRESENTATION"  ;function
   "HIGHLIGHT-OUTPUT-RECORD"            ;generic function
   "HIGHLIGHT-PRESENTATION"             ;presentation method
   "HORIZONTALLY"                       ;macro
   "HRACK-PANE"                         ;pane
   "IDENTITY-TRANSFORMATION-P"          ;generic function
   "IMMEDIATE-REPAINTING-MIXIN"         ;class
   "IMMEDIATE-RESCAN"                   ;generic function
   "IMMEDIATE-SHEET-INPUT-MIXIN"        ;class
   "INCREMENTAL-REDISPLAY"              ;generic function
   "INDENTING-OUTPUT"                   ;macro
   "INPUT-CONTEXT-TYPE"                 ;function
   "INPUT-EDITING-STREAM"               ;protocol class
   "INPUT-EDITING-STREAM-P"             ;predicate
   "INPUT-EDITOR-FORMAT"                ;generic function
   "INPUT-NOT-OF-REQUIRED-TYPE"         ;error
   "INPUT-NOT-OF-REQUIRED-TYPE"         ;function
   "INPUT-STREAM-P"                     ;generic function
   "INTEGER"                            ;presentation type
   "INTERACTIVE-STREAM-P"               ;predicate
   "INTERACTOR-PANE"                    ;pane
   "INVALIDATE-CACHED-REGIONS"          ;generic function
   "INVALIDATE-CACHED-TRANSFORMATIONS"  ;generic function
   "INVERT-TRANSFORMATION"              ;generic function
   "INVERTIBLE-TRANSFORMATION-P"        ;generic function
   "INVOKE-UPDATING-OUTPUT"             ;generic function
   "INVOKE-WITH-DRAWING-OPTIONS"        ;generic function
   "INVOKE-WITH-NEW-OUTPUT-RECORD"      ;generic function
   "INVOKE-WITH-OUTPUT-RECORDING-OPTIONS" ;generic function
   "INVOKE-WITH-OUTPUT-TO-OUTPUT-RECORD" ;generic function
   "INVOKE-WITH-TEXT-STYLE"             ;generic function
   "ITEM-LIST-OUTPUT-RECORD"            ;protocol class
   "ITEM-LIST-OUTPUT-RECORD-P"          ;predicate
   "KEY-PRESS-EVENT"                    ;class
   "KEY-RELEASE-EVENT"                  ;class
   "KEYBOARD-EVENT"                     ;class
   "KEYBOARD-EVENT-CHARACTER"           ;generic function
   "KEYBOARD-EVENT-KEY-NAME"            ;generic function
   "KEYWORD"                            ;presentation type
   "LABEL-PANE"                         ;pane
   "LABELLED-GADGET-MIXIN"              ;class
   "LABELLING"                          ;macro
   "LAYOUT-FRAME"                       ;generic function
   "LAYOUT-GRAPH-EDGES"                 ;generic function
   "LAYOUT-GRAPH-NODES"                 ;generic function
   "LINE"                               ;protocol class
   "LINE-END-POINT"                     ;generic function
   "LINE-END-POINT*"                    ;generic function
   "LINE-START-POINT"                   ;generic function
   "LINE-START-POINT*"                  ;generic function
   "LINE-STYLE"                         ;protocol class
   "LINE-STYLE-CAP-SHAPE"               ;generic function
   "LINE-STYLE-DASHES"                  ;generic function
   "LINE-STYLE-JOINT-SHAPE"             ;generic function
   "LINE-STYLE-P"                       ;predicate
   "LINE-STYLE-THICKNESS"               ;generic function
   "LINE-STYLE-UNIT"                    ;generic function
   "LINEP"                              ;predicate
   "LIST-PANE"                          ;class
   "LOOKUP-KEYSTROKE-COMMAND-ITEM"      ;function
   "LOOKUP-KEYSTROKE-ITEM"              ;function
   "MAKE-3-POINT-TRANSFORMATION"        ;function
   "MAKE-3-POINT-TRANSFORMATION*"       ;function
   "MAKE-APPLICATION-FRAME"             ;function
   "MAKE-BOUNDING-RECTANGLE"            ;function
   "MAKE-CLIM-APPLICATION-PANE"         ;function
   "MAKE-CLIM-INTERACTOR-PANE"          ;function
   "MAKE-CLIM-STREAM-PANE"              ;function
   "MAKE-COMMAND-TABLE"                 ;function
   "MAKE-CONTRASTING-DASH-PATTERNS"     ;function
   "MAKE-CONTRASTING-INKS"              ;function
   "MAKE-DESIGN-FROM-OUTPUT-RECORD"     ;generic function
   "MAKE-DEVICE-FONT-TEXT-STYLE"        ;function
   "MAKE-ELLIPSE"                       ;function
   "MAKE-ELLIPSE*"                      ;function
   "MAKE-ELLIPTICAL-ARC"                ;function
   "MAKE-ELLIPTICAL-ARC*"               ;function
   "MAKE-FLIPPING-INK"                  ;function
   "MAKE-GRAY-COLOR"                    ;function
   "MAKE-IHS-COLOR"                     ;function
   "MAKE-LINE"                          ;function
   "MAKE-LINE*"                         ;function
   "MAKE-LINE-STYLE"                    ;function
   "MAKE-MEDIUM"                        ;generic function
   "MAKE-MODIFIER-STATE"                ;function
   "MAKE-OPACITY"                       ;function
   "MAKE-PANE"                          ;function
   "MAKE-PANE-1"                        ;generic function
   "MAKE-PATTERN"                       ;function
   "MAKE-PATTERN-FROM-BITMAP-FILE"      ;function
   "MAKE-POINT"                         ;function
   "MAKE-POLYGON"                       ;function
   "MAKE-POLYGON*"                      ;function
   "MAKE-POLYLINE"                      ;function
   "MAKE-POLYLINE*"                     ;function
   "MAKE-PRESENTATION-TYPE-SPECIFIER"   ;function
   "MAKE-RECTANGLE"                     ;function
   "MAKE-RECTANGLE*"                    ;function
   "MAKE-RECTANGULAR-TILE"              ;function
   "MAKE-REFLECTION-TRANSFORMATION"     ;function
   "MAKE-REFLECTION-TRANSFORMATION*"    ;function
   "MAKE-RGB-COLOR"                     ;function
   "MAKE-ROTATION-TRANSFORMATION"       ;function
   "MAKE-ROTATION-TRANSFORMATION*"      ;function
   "MAKE-SCALING-TRANSFORMATION"        ;function
   "MAKE-SCALING-TRANSFORMATION*"       ;function
   "MAKE-SPACE-REQUIREMENT"             ;function
   "MAKE-STENCIL"                       ;function
   "MAKE-TEXT-STYLE"                    ;function
   "MAKE-TRANSFORMATION"                ;function
   "MAKE-TRANSLATION-TRANSFORMATION"    ;function
   "MAP-OVER-COMMAND-TABLE-COMMANDS"    ;function
   "MAP-OVER-COMMAND-TABLE-KEYSTROKES"  ;function
   "MAP-OVER-COMMAND-TABLE-MENU-ITEMS"  ;function
   "MAP-OVER-COMMAND-TABLE-NAMES"       ;function
   "MAP-OVER-COMMAND-TABLE-TRANSLATORS" ;function
   "MAP-OVER-FRAMES"                    ;function
   "MAP-OVER-GRAFTS"                    ;function
   "MAP-OVER-ITEM-LIST-CELLS"           ;generic function
   "MAP-OVER-OUTPUT-RECORDS"		;generic function
   "MAP-OVER-OUTPUT-RECORDS-CONTAINING-POSITION" ;generic function
   "MAP-OVER-OUTPUT-RECORDS-OVERLAPPING-REGION" ;generic function
   "MAP-OVER-POLYGON-COORDINATES"       ;generic function
   "MAP-OVER-POLYGON-SEGMENTS"          ;generic function
   "MAP-OVER-PORTS"                     ;function
   "MAP-OVER-PRESENTATION-TYPE-SUPERTYPES" ;presentation method
   "MAP-OVER-PRESENTATION-TYPE-SUPERTYPES" ;function
   "MAP-OVER-REGION-SET-REGIONS"        ;generic function
   "MAP-OVER-ROW-CELLS"                 ;generic function
   "MAP-OVER-ROW-CELLS"                 ;generic function
   "MAP-OVER-SHEETS"                    ;generic function
   "MAP-OVER-SHEETS-CONTAINING-POSITION" ;generic function
   "MAP-OVER-SHEETS-OVERLAPPING-REGION" ;generic function
   "MAP-OVER-TABLE-ELEMENTS"            ;generic function
   "MAP-SHEET-POSITION-TO-CHILD"        ;generic function
   "MAP-SHEET-POSITION-TO-PARENT"       ;generic function
   "MAP-SHEET-RECTANGLE*-TO-CHILD"      ;generic function
   "MAP-SHEET-RECTANGLE*-TO-PARENT"     ;generic function
   "MATCH-OUTPUT-RECORDS"               ;generic function
   "MEDIUM"                             ;protocol class
   "MEDIUM-BACKGROUND"                  ;generic function
   "MEDIUM-BACKGROUND"                  ;generic function
   "MEDIUM-BEEP"                        ;generic function
   "MEDIUM-BUFFERING-OUTPUT-P"          ;generic function
   "MEDIUM-CLEAR-AREA"                  ;generic function
   "MEDIUM-CLIPPING-REGION"             ;generic function
   "MEDIUM-CLIPPING-REGION"             ;generic function
   "MEDIUM-COPY-AREA"                   ;generic function
   "MEDIUM-CURRENT-TEXT-STYLE"          ;generic function
   "MEDIUM-DEFAULT-TEXT-STYLE"          ;generic function
   "MEDIUM-DEFAULT-TEXT-STYLE"          ;generic function
   "MEDIUM-DRAW-ELLIPSE*"               ;generic function
   "MEDIUM-DRAW-LINE*"                  ;generic function
   "MEDIUM-DRAW-LINES*"                 ;generic function
   "MEDIUM-DRAW-POINT*"                 ;generic function
   "MEDIUM-DRAW-POINTS*"                ;generic function
   "MEDIUM-DRAW-POLYGON*"               ;generic function
   "MEDIUM-DRAW-RECTANGLE*"             ;generic function
   "MEDIUM-DRAW-RECTANGLES*"            ;generic function
   "MEDIUM-DRAW-TEXT*"                  ;generic function
   "MEDIUM-DRAWABLE"                    ;generic function
   "MEDIUM-FINISH-OUTPUT"               ;generic function
   "MEDIUM-FORCE-OUTPUT"                ;generic function
   "MEDIUM-FOREGROUND"                  ;generic function
   "MEDIUM-FOREGROUND"                  ;generic function
   "MEDIUM-INK"                         ;generic function
   "MEDIUM-INK"                         ;generic function
   "MEDIUM-LINE-STYLE"                  ;generic function
   "MEDIUM-LINE-STYLE"                  ;generic function
   "MEDIUM-MERGED-TEXT-STYLE"           ;generic function
   "MEDIUM-SHEET"                       ;generic function
   "MEDIUM-TEXT-STYLE"                  ;generic function
   "MEDIUM-TEXT-STYLE"                  ;generic function
   "MEDIUM-TRANSFORMATION"              ;generic function
   "MEDIUM-TRANSFORMATION"              ;generic function
   "MEDIUMP"                            ;predicate
   "MEMBER"                             ;presentation type abbrev
   "MEMBER-ALIST"                       ;presentation type abbrev
   "MEMBER-SEQUENCE"                    ;presentation type abbrev
   "MENU-BUTTON"                        ;class
   "MENU-BUTTON-PANE"                   ;class
   "MENU-CHOOSE"                        ;generic function
   "MENU-CHOOSE-COMMAND-FROM-COMMAND-TABLE" ;function
   "MENU-CHOOSE-FROM-DRAWER"            ;generic function
   "MENU-COMMAND-PARSER"                ;function
   "MENU-ITEM-DISPLAY"                  ;function
   "MENU-ITEM-OPTIONS"                  ;function
   "MENU-ITEM-VALUE"                    ;function
   "MENU-READ-REMAINING-ARGUMENTS-FOR-PARTIAL-COMMAND" ;function
   "MERGE-TEXT-STYLES"                  ;generic function
   "MIRRORED-SHEET-MIXIN"               ;class
   "MODIFIER-STATE-MATCHES-GESTURE-NAME-P" ;function
   "MOVE-AND-RESIZE-SHEET"              ;generic function
   "MOVE-SHEET"                         ;generic function
   "NEW-PAGE"                           ;function
   "NIL"                                ;presentation type
   "NOTE-COMMAND-DISABLED"              ;generic function
   "NOTE-COMMAND-ENABLED"               ;generic function
   "NOTE-FRAME-DEICONIFIED"             ;generic function
   "NOTE-FRAME-DISABLED"                ;generic function
   "NOTE-FRAME-ENABLED"                 ;generic function
   "NOTE-FRAME-ICONIFIED"               ;generic function
   "NOTE-GADGET-ACTIVATED"              ;generic function
   "NOTE-GADGET-DEACTIVATED"            ;generic function
   "NOTE-OUTPUT-RECORD-CHILD-CHANGED"   ;generic function
   "NOTE-SHEET-ADOPTED"                 ;generic function
   "NOTE-SHEET-DEGRAFTED"               ;generic function
   "NOTE-SHEET-DISABLED"                ;generic function
   "NOTE-SHEET-DISOWNED"                ;generic function
   "NOTE-SHEET-ENABLED"                 ;generic function
   "NOTE-SHEET-GRAFTED"                 ;generic function
   "NOTE-SHEET-REGION-CHANGED"          ;generic function
   "NOTE-SHEET-TRANSFORMATION-CHANGED"  ;generic function
   "NOTE-SPACE-REQUIREMENTS-CHANGED"    ;generic function
   "NOTIFY-USER"                        ;generic function
   "NULL"                               ;presentation type
   "NULL-OR-TYPE"                       ;presentation type abbrev
   "NUMBER"                             ;presentation type
   "OPACITY"                            ;protocol class
   "OPACITY-VALUE"                      ;generic function
   "OPACITYP"                           ;predicate
   "OPEN-STREAM-P"                      ;generic function
   "OPEN-WINDOW-STREAM"                 ;function
   "OPTION-PANE"                        ;class
   "OR"                                 ;presentation type
   "ORIENTED-GADGET-MIXIN"              ;class
   "OUTLINED-PANE"                      ;pane
   "OUTLINING"                          ;macro
   "OUTPUT-RECORD"                      ;protocol class
   "OUTPUT-RECORD-CACHE-VALUE"          ;generic function
   "OUTPUT-RECORD-CHILDREN"             ;generic function
   "OUTPUT-RECORD-CONTENTS-OK"          ;generic function
   "OUTPUT-RECORD-COUNT"                ;generic function
   "OUTPUT-RECORD-DISPLAYER"            ;generic function
   "OUTPUT-RECORD-END-CURSOR-POSITION"  ;generic function
   "OUTPUT-RECORD-FIXED-POSITION"       ;generic function
   "OUTPUT-RECORD-HIT-DETECTION-RECTANGLE*" ;generic function
   "OUTPUT-RECORD-P"                    ;predicate
   "OUTPUT-RECORD-PARENT"               ;generic function
   "OUTPUT-RECORD-POSITION"             ;generic function
   "OUTPUT-RECORD-REFINED-POSITION-TEST" ;generic function
   "OUTPUT-RECORD-START-CURSOR-POSITION" ;generic function
   "OUTPUT-RECORD-UNIQUE-ID"            ;generic function
   "OUTPUT-RECORDING-STREAM"            ;protocol class
   "OUTPUT-RECORDING-STREAM-P"          ;predicate
   "OUTPUT-STREAM-P"                    ;generic function
   "PANE"                               ;protocol class
   "PANE-BACKGROUND"                    ;generic function
   "PANE-FOREGROUND"                    ;generic function
   "PANE-FRAME"                         ;generic function
   "PANE-NAME"                          ;generic function
   "PANE-NEEDS-REDISPLAY"               ;generic function
   "PANE-SCROLLER"                      ;generic function
   "PANE-TEXT-STYLE"                    ;generic function
   "PANE-VIEWPORT"                      ;generic function
   "PANE-VIEWPORT-REGION"               ;generic function
   "PANEP"                              ;predicate
   "PARSE-TEXT-STYLE"                   ;function
   "PARTIAL-COMMAND-P"                  ;function
   "PATH"                               ;protocol class
   "PATHNAME"                           ;presentation type
   "PATHP"                              ;predicate
   "PATTERN-HEIGHT"                     ;generic function
   "PATTERN-WIDTH"                      ;generic function
   "PERMANENT-MEDIUM-SHEET-OUTPUT-MIXIN" ;class
   "PIXMAP-DEPTH"                       ;generic function
   "PIXMAP-HEIGHT"                      ;generic function
   "PIXMAP-WIDTH"                       ;generic function
   "POINT"                              ;protocol class
   "POINT-POSITION"                     ;generic function
   "POINT-X"                            ;generic function
   "POINT-Y"                            ;generic function
   "POINTER"                            ;protocol class
   "POINTER-BOUNDARY-EVENT"             ;class
   "POINTER-BOUNDARY-EVENT-KIND"        ;generic function
   "POINTER-BUTTON-EVENT"               ;class
   "POINTER-BUTTON-HOLD-EVENT"          ;class
   "POINTER-BUTTON-PRESS-EVENT"         ;class
   "POINTER-BUTTON-RELEASE-EVENT"       ;class
   "POINTER-BUTTON-STATE"               ;generic function
   "POINTER-CLICK-AND-HOLD-EVENT"       ;class
   "POINTER-CLICK-EVENT"                ;class
   "POINTER-CURSOR"                     ;generic function
   "POINTER-DOCUMENTATION-PANE"         ;pane
   "POINTER-DOCUMENTATION-VIEW"         ;class
   "POINTER-DOUBLE-CLICK-EVENT"         ;class
   "POINTER-ENTER-EVENT"                ;class
   "POINTER-EVENT"                      ;class
   "POINTER-EVENT-BUTTON"               ;generic function
   "POINTER-EVENT-NATIVE-X"             ;generic function
   "POINTER-EVENT-NATIVE-Y"             ;generic function
   "POINTER-EVENT-POINTER"              ;generic function
   "POINTER-EVENT-X"                    ;generic function
   "POINTER-EVENT-Y"                    ;generic function
   "POINTER-EXIT-EVENT"                 ;class
   "POINTER-MOTION-EVENT"               ;class
   "POINTER-POSITION"                   ;generic function
   "POINTER-SHEET"                      ;generic function
   "POINTERP"                           ;predicate
   "POINTP"                             ;predicate
   "POLYGON"                            ;protocol class
   "POLYGON-POINTS"                     ;generic function
   "POLYGONP"                           ;predicate
   "POLYLINE"                           ;protocol class
   "POLYLINE-CLOSED"                    ;generic function
   "POLYLINEP"                          ;predicate
   "PORT"                               ;protocol class
   "PORT"                               ;generic function
   "PORT-KEYBOARD-INPUT-FOCUS"          ;generic function
   "PORT-NAME"                          ;generic function
   "PORT-PROPERTIES"                    ;generic function
   "PORT-SERVER-PATH"                   ;generic function
   "PORT-TYPE"                          ;generic function
   "PORTP"                              ;predicate
   "PRESENT"                            ;presentation method
   "PRESENT"                            ;function
   "PRESENT-TO-STRING"                  ;function
   "PRESENTATION"                       ;protocol class
   "PRESENTATION-DEFAULT-PREPROCESSOR"  ;presentation method
   "PRESENTATION-MATCHES-CONTEXT-TYPE"  ;function
   "PRESENTATION-MODIFIER"              ;generic function
   "PRESENTATION-OBJECT"                ;generic function
   "PRESENTATION-REFINED-POSITION-TEST" ;presentation method
   "PRESENTATION-REPLACE-INPUT"         ;generic function
   "PRESENTATION-SINGLE-BOX"            ;generic function
   "PRESENTATION-SUBTYPEP"              ;presentation method
   "PRESENTATION-SUBTYPEP"              ;function
   "PRESENTATION-TYPE"                  ;generic function
   "PRESENTATION-TYPE-DIRECT-SUPERTYPES" ;function
   "PRESENTATION-TYPE-HISTORY"          ;presentation method
   "PRESENTATION-TYPE-NAME"             ;function
   "PRESENTATION-TYPE-OF"               ;function
   "PRESENTATION-TYPE-OPTIONS"          ;function
   "PRESENTATION-TYPE-PARAMETERS"       ;function
   "PRESENTATION-TYPE-SPECIFIER-P"      ;presentation method
   "PRESENTATION-TYPE-SPECIFIER-P"      ;function
   "PRESENTATION-TYPEP"                 ;presentation method
   "PRESENTATION-TYPEP"                 ;function
   "PRESENTATIONP"                      ;predicate
   "PRINT-MENU-ITEM"                    ;function
   "PROCESS-NEXT-EVENT"                 ;generic function
   "PROMPT-FOR-ACCEPT"                  ;generic function
   "PROMPT-FOR-ACCEPT-1"                ;function
   "PROPAGATE-OUTPUT-RECORD-CHANGES"    ;generic function
   "PROPAGATE-OUTPUT-RECORD-CHANGES-P"  ;generic function
   "PUSH-BUTTON"                        ;class
   "PUSH-BUTTON-PANE"                   ;class
   "PUSH-BUTTON-SHOW-AS-DEFAULT"        ;generic function
   "QUEUE-EVENT"                        ;generic function
   "QUEUE-REPAINT"                      ;generic function
   "QUEUE-RESCAN"                       ;generic function
   "RADIO-BOX"                          ;class
   "RADIO-BOX-CURRENT-SELECTION"        ;generic function
   "RADIO-BOX-PANE"                     ;class
   "RADIO-BOX-SELECTIONS"               ;generic function
   "RAISE-FRAME"                        ;generic function
   "RAISE-MIRROR"                       ;generic function
   "RAISE-SHEET"                        ;generic function
   "RANGE-GADGET-MIXIN"                 ;class
   "RATIO"                              ;presentation type
   "RATIONAL"                           ;presentation type
   "READ-BITMAP-FILE"                   ;generic function
   "READ-COMMAND"                       ;function
   "READ-COMMAND-USING-KEYSTROKES"      ;function
   "READ-FRAME-COMMAND"                 ;generic function
   "READ-GESTURE"                       ;function
   "READ-TOKEN"                         ;function
   "REAL"                               ;presentation type
   "REALIZE-MIRROR"                     ;generic function
   "RECOMPUTE-CONTENTS-OK"              ;generic function
   "RECOMPUTE-EXTENT-FOR-CHANGED-CHILD" ;generic function
   "RECOMPUTE-EXTENT-FOR-NEW-CHILD"     ;generic function
   "RECTANGLE"                          ;protocol class
   "RECTANGLE-EDGES*"                   ;generic function
   "RECTANGLE-HEIGHT"                   ;generic function
   "RECTANGLE-MAX-POINT"                ;generic function
   "RECTANGLE-MAX-X"                    ;generic function
   "RECTANGLE-MAX-Y"                    ;generic function
   "RECTANGLE-MIN-POINT"                ;generic function
   "RECTANGLE-MIN-X"                    ;generic function
   "RECTANGLE-MIN-Y"                    ;generic function
   "RECTANGLE-SIZE"                     ;generic function
   "RECTANGLE-WIDTH"                    ;generic function
   "RECTANGLEP"                         ;predicate
   "RECTILINEAR-TRANSFORMATION-P"       ;generic function
   "REDISPLAY"                          ;function
   "REDISPLAY-FRAME-PANE"               ;generic function
   "REDISPLAY-FRAME-PANES"              ;generic function
   "REDISPLAY-OUTPUT-RECORD"            ;generic function
   "REDISPLAYABLE-STREAM-P"             ;generic function
   "REDRAW-INPUT-BUFFER"                ;generic function
   "REFLECTION-TRANSFORMATION-P"        ;generic function
   "REFLECTION-UNDERSPECIFIED"          ;error
   "REGION"                             ;protocol class
   "REGION-CONTAINS-POSITION-P"         ;generic function
   "REGION-CONTAINS-REGION-P"           ;generic function
   "REGION-DIFFERENCE"                  ;generic function
   "REGION-EQUAL"                       ;generic function
   "REGION-INTERSECTION"                ;generic function
   "REGION-INTERSECTS-REGION-P"         ;generic function
   "REGION-SET"                         ;protocol class
   "REGION-SET-P"                       ;predicate
   "REGION-SET-REGIONS"                 ;generic function
   "REGION-UNION"                       ;generic function
   "REGIONP"                            ;predicate
   "REMOVE-COMMAND-FROM-COMMAND-TABLE"  ;function
   "REMOVE-KEYSTROKE-FROM-COMMAND-TABLE" ;function
   "REMOVE-MENU-ITEM-FROM-COMMAND-TABLE" ;function
   "REMOVE-PRESENTATION-TRANSLATOR-FROM-COMMAND-TABLE" ;function
   "REORDER-SHEETS"                     ;generic function
   "REPAINT-SHEET"                      ;generic function
   "REPLACE-INPUT"                      ;generic function
   "REPLAY"                             ;function
   "REPLAY-OUTPUT-RECORD"               ;generic function
   "RESCAN-IF-NECESSARY"                ;generic function
   "RESET-FRAME"                        ;generic function
   "RESET-SCAN-POINTER"                 ;generic function
   "RESIZE-SHEET"                       ;generic function
   "RESTART-PORT"                       ;generic function
   "RESTRAINING"                        ;macro
   "RESTRAINING-PANE"                   ;pane
   "RIGID-TRANSFORMATION-P"             ;generic function
   "ROW-OUTPUT-RECORD"                  ;protocol class
   "ROW-OUTPUT-RECORD-P"                ;predicate
   "RUN-FRAME-TOP-LEVEL"                ;generic function
   "SCALING-TRANSFORMATION-P"           ;generic function
   "SCROLL-BAR"                         ;class
   "SCROLL-BAR-DRAG-CALLBACK"           ;generic function
   "SCROLL-BAR-PANE"                    ;class
   "SCROLL-BAR-SCROLL-DOWN-LINE-CALLBACK" ;generic function
   "SCROLL-BAR-SCROLL-DOWN-PAGE-CALLBACK" ;generic function
   "SCROLL-BAR-SCROLL-TO-BOTTOM-CALLBACK" ;generic function
   "SCROLL-BAR-SCROLL-TO-TOP-CALLBACK"  ;generic function
   "SCROLL-BAR-SCROLL-UP-LINE-CALLBACK" ;generic function
   "SCROLL-BAR-SCROLL-UP-PAGE-CALLBACK" ;generic function
   "SCROLL-DOWN-LINE-CALLBACK"          ;callback
   "SCROLL-DOWN-PAGE-CALLBACK"          ;callback
   "SCROLL-EXTENT"                      ;generic function
   "SCROLL-TO-BOTTOM-CALLBACK"          ;callback
   "SCROLL-TO-TOP-CALLBACK"             ;callback
   "SCROLL-UP-LINE-CALLBACK"            ;callback
   "SCROLL-UP-PAGE-CALLBACK"            ;callback
   "SCROLLER-PANE"                      ;pane
   "SCROLLING"                          ;macro
   "SEQUENCE"                           ;presentation type
   "SEQUENCE-ENUMERATED"                ;presentation type
   "SET-HIGHLIGHTED-PRESENTATION"       ;function
   "SHEET"                              ;protocol class
   "SHEET-ADOPT-CHILD"                  ;generic function
   "SHEET-ALLOCATED-REGION"             ;generic function
   "SHEET-ANCESTOR-P"                   ;generic function
   "SHEET-CHILDREN"                     ;generic function
   "SHEET-DELTA-TRANSFORMATION"         ;generic function
   "SHEET-DEVICE-REGION"                ;generic function
   "SHEET-DEVICE-TRANSFORMATION"        ;generic function
   "SHEET-DIRECT-MIRROR"                ;generic function
   "SHEET-DISOWN-CHILD"                 ;generic function
   "SHEET-ENABLED-CHILDREN"             ;generic function
   "SHEET-ENABLED-P"                    ;generic function
   "SHEET-EVENT-QUEUE"                  ;generic function
   "SHEET-GRAFTED-P"                    ;generic function
   "SHEET-IDENTITY-TRANSFORMATION-MIXIN" ;class
   "SHEET-LEAF-MIXIN"                   ;class
   "SHEET-MEDIUM"                       ;generic function
   "SHEET-MIRROR"                       ;generic function
   "SHEET-MIRRORED-ANCESTOR"            ;generic function
   "SHEET-MULTIPLE-CHILD-MIXIN"         ;class
   "SHEET-MUTE-INPUT-MIXIN"             ;class
   "SHEET-MUTE-OUTPUT-MIXIN"            ;class
   "SHEET-MUTE-REPAINTING-MIXIN"        ;class
   "SHEET-NATIVE-REGION"                ;generic function
   "SHEET-NATIVE-TRANSFORMATION"        ;generic function
   "SHEET-OCCLUDING-SHEETS"             ;generic function
   "SHEET-PARENT"                       ;generic function
   "SHEET-PARENT-MIXIN"                 ;class
   "SHEET-REGION"                       ;generic function
   "SHEET-SIBLINGS"                     ;generic function
   "SHEET-SINGLE-CHILD-MIXIN"           ;class
   "SHEET-TRANSFORMATION"               ;generic function
   "SHEET-TRANSFORMATION-MIXIN"         ;class
   "SHEET-TRANSLATION-MIXIN"            ;class
   "SHEET-VIEWABLE-P"                   ;generic function
   "SHEET-WITH-MEDIUM-MIXIN"            ;class
   "SHEET-Y-INVERTING-TRANSFORMATION-MIXIN" ;class
   "SHEETP"                             ;predicate
   "SHRINK-FRAME"                       ;generic function
   "SIMPLE-COMPLETION-ERROR"            ;condition
   "SIMPLE-PARSE-ERROR"                 ;error
   "SIMPLE-PARSE-ERROR"                 ;function
   "SINGULAR-TRANSFORMATION"            ;error
   "SLIDER"                             ;class
   "SLIDER-DRAG-CALLBACK"               ;generic function
   "SLIDER-PANE"                        ;class
   "SPACE-REQUIREMENT"                  ;class
   "SPACE-REQUIREMENT+"                 ;function
   "SPACE-REQUIREMENT+*"                ;function
   "SPACE-REQUIREMENT-COMBINE"          ;function
   "SPACE-REQUIREMENT-COMPONENTS"       ;generic function
   "SPACE-REQUIREMENT-HEIGHT"           ;generic function
   "SPACE-REQUIREMENT-MAX-HEIGHT"       ;generic function
   "SPACE-REQUIREMENT-MAX-WIDTH"        ;generic function
   "SPACE-REQUIREMENT-MIN-HEIGHT"       ;generic function
   "SPACE-REQUIREMENT-MIN-WIDTH"        ;generic function
   "SPACE-REQUIREMENT-WIDTH"            ;generic function
   "SPACING"                            ;macro
   "SPACING-PANE"                       ;pane
   "STANDARD-APPLICATION-FRAME"         ;class
   "STANDARD-BOUNDING-RECTANGLE"        ;class
   "STANDARD-CELL-OUTPUT-RECORD"        ;class
   "STANDARD-COLUMN-OUTPUT-RECORD"      ;class
   "STANDARD-COMMAND-TABLE"             ;class
   "STANDARD-ELLIPSE"                   ;class
   "STANDARD-ELLIPTICAL-ARC"            ;class
   "STANDARD-ENCAPSULATING-STREAM"      ;class
   "STANDARD-EXTENDED-INPUT-STREAM"     ;class
   "STANDARD-EXTENDED-OUTPUT-STREAM"    ;class
   "STANDARD-GRAPH-NODE-OUTPUT-RECORD"  ;class
   "STANDARD-GRAPH-OUTPUT-RECORD"       ;class
   "STANDARD-INPUT-EDITING-STREAM"      ;class
   "STANDARD-INPUT-STREAM"              ;class
   "STANDARD-ITEM-LIST-OUTPUT-RECORD"   ;class
   "STANDARD-LINE"                      ;class
   "STANDARD-LINE-STYLE"                ;class
   "STANDARD-OUTPUT-RECORDING-STREAM"   ;class
   "STANDARD-OUTPUT-STREAM"             ;class
   "STANDARD-POINT"                     ;class
   "STANDARD-POINTER"                   ;class
   "STANDARD-POLYGON"                   ;class
   "STANDARD-POLYLINE"                  ;class
   "STANDARD-PRESENTATION"              ;class
   "STANDARD-RECTANGLE"                 ;class
   "STANDARD-REGION-DIFFERENCE"         ;class
   "STANDARD-REGION-INTERSECTION"       ;class
   "STANDARD-REGION-UNION"              ;class
   "STANDARD-REPAINTING-MIXIN"          ;class
   "STANDARD-ROW-OUTPUT-RECORD"         ;class
   "STANDARD-SEQUENCE-OUTPUT-HISTORY"   ;class
   "STANDARD-SEQUENCE-OUTPUT-RECORD"    ;class
   "STANDARD-SHEET-INPUT-MIXIN"         ;class
   "STANDARD-SHEET-OUTPUT-MIXIN"        ;class
   "STANDARD-TABLE-OUTPUT-RECORD"       ;class
   "STANDARD-TEXT-CURSOR"               ;class
   "STANDARD-TEXT-STYLE"                ;class
   "STANDARD-TREE-OUTPUT-HISTORY"       ;class
   "STANDARD-TREE-OUTPUT-RECORD"        ;class
   "STANDARD-UPDATING-OUTPUT-RECORD"    ;class
   "STREAM-ACCEPT"                      ;generic function
   "STREAM-ADD-CHARACTER-OUTPUT"        ;generic function
   "STREAM-ADD-OUTPUT-RECORD"           ;generic function
   "STREAM-ADD-STRING-OUTPUT"           ;generic function
   "STREAM-ADVANCE-TO-COLUMN"           ;generic function
   "STREAM-ADVANCE-TO-COLUMN"           ;generic function
   "STREAM-BASELINE"                    ;generic function
   "STREAM-CHARACTER-WIDTH"             ;generic function
   "STREAM-CLEAR-INPUT"                 ;generic function
   "STREAM-CLEAR-INPUT"                 ;generic function
   "STREAM-CLEAR-OUTPUT"                ;generic function
   "STREAM-CLEAR-OUTPUT"                ;generic function
   "STREAM-CLOSE-TEXT-OUTPUT-RECORD"    ;generic function
   "STREAM-CURRENT-OUTPUT-RECORD"       ;generic function
   "STREAM-CURSOR-POSITION"             ;generic function
   "STREAM-DEFAULT-VIEW"                ;generic function
   "STREAM-DRAWING-P"                   ;generic function
   "STREAM-ELEMENT-TYPE"                ;generic function
   "STREAM-END-OF-LINE-ACTION"          ;generic function
   "STREAM-END-OF-PAGE-ACTION"          ;generic function
   "STREAM-FINISH-OUTPUT"               ;generic function
   "STREAM-FINISH-OUTPUT"               ;generic function
   "STREAM-FORCE-OUTPUT"                ;generic function
   "STREAM-FORCE-OUTPUT"                ;generic function
   "STREAM-FRESH-LINE"                  ;generic function
   "STREAM-FRESH-LINE"                  ;generic function
   "STREAM-INCREMENT-CURSOR-POSITION"   ;generic function
   "STREAM-INPUT-BUFFER"                ;generic function
   "STREAM-INPUT-WAIT"                  ;generic function
   "STREAM-INSERTION-POINTER"           ;generic function
   "STREAM-LINE-COLUMN"                 ;generic function
   "STREAM-LINE-COLUMN"                 ;generic function
   "STREAM-LINE-HEIGHT"                 ;generic function
   "STREAM-LISTEN"                      ;generic function
   "STREAM-LISTEN"                      ;generic function
   "STREAM-OUTPUT-HISTORY"              ;generic function
   "STREAM-OUTPUT-HISTORY-MIXIN"        ;class
   "STREAM-PATHNAME"                    ;generic function
   "STREAM-PEEK-CHAR"                   ;generic function
   "STREAM-PEEK-CHAR"                   ;generic function
   "STREAM-POINTER-POSITION"            ;generic function
   "STREAM-PRESENT"                     ;generic function
   "STREAM-PROCESS-GESTURE"             ;generic function
   "STREAM-READ-BYTE"                   ;generic function
   "STREAM-READ-CHAR"                   ;generic function
   "STREAM-READ-CHAR"                   ;generic function
   "STREAM-READ-CHAR-NO-HANG"           ;generic function
   "STREAM-READ-CHAR-NO-HANG"           ;generic function
   "STREAM-READ-GESTURE"                ;generic function
   "STREAM-READ-LINE"                   ;generic function
   "STREAM-READ-LINE"                   ;generic function
   "STREAM-RECORDING-P"                 ;generic function
   "STREAM-REDISPLAYING-P"              ;generic function
   "STREAM-REPLAY"                      ;generic function
   "STREAM-RESCANNING-P"                ;generic function
   "STREAM-SCAN-POINTER"                ;generic function
   "STREAM-SET-INPUT-FOCUS"             ;generic function
   "STREAM-START-LINE-P"                ;generic function
   "STREAM-START-LINE-P"                ;generic function
   "STREAM-STRING-WIDTH"                ;generic function
   "STREAM-TERPRI"                      ;generic function
   "STREAM-TERPRI"                      ;generic function
   "STREAM-TEXT-CURSOR"                 ;generic function
   "STREAM-TEXT-MARGIN"                 ;generic function
   "STREAM-TEXT-OUTPUT-RECORD"          ;generic function
   "STREAM-TRUENAME"                    ;generic function
   "STREAM-UNREAD-CHAR"                 ;generic function
   "STREAM-UNREAD-CHAR"                 ;generic function
   "STREAM-UNREAD-GESTURE"              ;generic function
   "STREAM-VERTICAL-SPACING"            ;generic function
   "STREAM-WRITE-BYTE"                  ;generic function
   "STREAM-WRITE-CHAR"                  ;generic function
   "STREAM-WRITE-CHAR"                  ;generic function
   "STREAM-WRITE-STRING"                ;generic function
   "STREAM-WRITE-STRING"                ;generic function
   "STREAMP"                            ;generic function
   "STRING"                             ;presentation type
   "SUBSET"                             ;presentation type abbrev
   "SUBSET-ALIST"                       ;presentation type abbrev
   "SUBSET-COMPLETION"                  ;presentation type
   "SUBSET-SEQUENCE"                    ;presentation type abbrev
   "SUBSTITUTE-NUMERIC-ARGUMENT-MARKER" ;function
   "SUGGEST"                            ;function
   "SURROUNDING-OUTPUT-WITH-BORDER"     ;macro
   "SYMBOL"                             ;presentation type
   "T"                                  ;presentation type
   "TABLE-OUTPUT-RECORD"                ;protocol class
   "TABLE-OUTPUT-RECORD-P"              ;predicate
   "TABLE-PANE"                         ;pane
   "TABLING"                            ;macro
   "TEMPORARY-MEDIUM-SHEET-OUTPUT-MIXIN" ;class
   "TEST-PRESENTATION-TRANSLATOR"       ;function
   "TEXT-DISPLAYED-OUTPUT-RECORD"       ;protocol class
   "TEXT-DISPLAYED-OUTPUT-RECORD-P"     ;predicate
   "TEXT-DISPLAYED-OUTPUT-RECORD-STRING" ;generic function
   "TEXT-EDITOR"                        ;class
   "TEXT-EDITOR-PANE"                   ;class
   "TEXT-FIELD"                         ;class
   "TEXT-FIELD-PANE"                    ;class
   "TEXT-SIZE"                          ;generic function
   "TEXT-STYLE"                         ;protocol class
   "TEXT-STYLE-ASCENT"                  ;generic function
   "TEXT-STYLE-COMPONENTS"              ;generic function
   "TEXT-STYLE-DESCENT"                 ;generic function
   "TEXT-STYLE-FACE"                    ;generic function
   "TEXT-STYLE-FAMILY"                  ;generic function
   "TEXT-STYLE-FIXED-WIDTH-P"           ;generic function
   "TEXT-STYLE-HEIGHT"                  ;generic function
   "TEXT-STYLE-MAPPING"                 ;generic function
   "TEXT-STYLE-P"                       ;predicate
   "TEXT-STYLE-SIZE"                    ;generic function
   "TEXT-STYLE-WIDTH"                   ;generic function
   "TEXTUAL-DIALOG-VIEW"                ;class
   "TEXTUAL-MENU-VIEW"                  ;class
   "TEXTUAL-VIEW"                       ;class
   "THROW-HIGHLIGHTED-PRESENTATION"     ;function
   "TIMER-EVENT"                        ;class
   "TITLE-PANE"                         ;pane
   "TOGGLE-BUTTON"                      ;class
   "TOGGLE-BUTTON-INDICATOR-TYPE"       ;generic function
   "TOGGLE-BUTTON-PANE"                 ;class
   "TOKEN-OR-TYPE"                      ;presentation type abbrev
   "TRACKING-POINTER"                   ;macro
   "TRANSFORM-DISTANCE"                 ;generic function
   "TRANSFORM-POSITION"                 ;generic function
   "TRANSFORM-RECTANGLE*"               ;generic function
   "TRANSFORM-REGION"                   ;generic function
   "TRANSFORMATION"                     ;protocol class
   "TRANSFORMATION-EQUAL"               ;generic function
   "TRANSFORMATION-ERROR"               ;error
   "TRANSFORMATION-UNDERSPECIFIED"      ;error
   "TRANSFORMATIONP"                    ;predicate
   "TRANSLATION-TRANSFORMATION-P"       ;generic function
   "TREE-RECOMPUTE-EXTENT"              ;generic function
   "TYPE-OR-STRING"                     ;presentation type abbrev
   "UNHIGHLIGHT-HIGHLIGHTED-PRESENTATION" ;function
   "UNREAD-GESTURE"                     ;function
   "UNTRANSFORM-DISTANCE"               ;generic function
   "UNTRANSFORM-POSITION"               ;generic function
   "UNTRANSFORM-RECTANGLE*"             ;generic function
   "UNTRANSFORM-REGION"                 ;generic function
   "UPDATING-OUTPUT"                    ;macro
   "UPDATING-OUTPUT-RECORD"             ;protocol class
   "UPDATING-OUTPUT-RECORD-P"           ;predicate
   "USER-COMMAND-TABLE"                 ;command table
   "VALUE-CHANGED-CALLBACK"             ;callback
   "VALUE-GADGET"                       ;class
   "VBOX-PANE"                          ;pane
   "VERTICALLY"                         ;macro
   "VIEW"                               ;protocol class
   "VIEWP"                              ;predicate
   "VRACK-PANE"                         ;pane
   "WINDOW-CLEAR"                       ;generic function
   "WINDOW-CONFIGURATION-EVENT"         ;class
   "WINDOW-ERASE-VIEWPORT"              ;generic function
   "WINDOW-EVENT"                       ;class
   "WINDOW-EVENT-MIRRORED-SHEET"        ;generic function
   "WINDOW-EVENT-NATIVE-REGION"         ;generic function
   "WINDOW-EVENT-REGION"                ;generic function
   "WINDOW-MANAGER-DELETE-EVENT"        ;class
   "WINDOW-MANAGER-EVENT"               ;class
   "WINDOW-REFRESH"                     ;generic function
   "WINDOW-REPAINT-EVENT"               ;class
   "WINDOW-VIEWPORT"                    ;generic function
   "WINDOW-VIEWPORT-POSITION"           ;generic function
   "WITH-ACCEPT-HELP"                   ;macro
   "WITH-ACTIVATION-GESTURES"           ;macro
   "WITH-APPLICATION-FRAME"             ;macro
   "WITH-BOUNDING-RECTANGLE*"           ;macro
   "WITH-COMMAND-TABLE-KEYSTROKES"      ;macro
   "WITH-DELIMITER-GESTURES"            ;macro
   "WITH-DRAWING-OPTIONS"               ;macro
   "WITH-END-OF-LINE-ACTION"            ;macro
   "WITH-END-OF-PAGE-ACTION"            ;macro
   "WITH-FIRST-QUADRANT-COORDINATES"    ;macro
   "WITH-FRAME-MANAGER"                 ;macro
   "WITH-GRAFT-LOCKED"                  ;macro
   "WITH-IDENTITY-TRANSFORMATION"       ;macro
   "WITH-INPUT-CONTEXT"                 ;macro
   "WITH-INPUT-EDITING"                 ;macro
   "WITH-INPUT-EDITOR-TYPEOUT"          ;macro
   "WITH-INPUT-FOCUS"                   ;macro
   "WITH-LOCAL-COORDINATES"             ;macro
   "WITH-LOOK-AND-FEEL-REALIZATION"     ;macro
   "WITH-MENU"                          ;macro
   "WITH-NEW-OUTPUT-RECORD"             ;macro
   "WITH-OUTPUT-AS-GADGET"              ;macro
   "WITH-OUTPUT-AS-PRESENTATION"        ;macro
   "WITH-OUTPUT-BUFFERED"               ;macro
   "WITH-OUTPUT-RECORDING-OPTIONS"      ;macro
   "WITH-OUTPUT-TO-OUTPUT-RECORD"       ;macro
   "WITH-OUTPUT-TO-PIXMAP"              ;macro
   "WITH-OUTPUT-TO-POSTSCRIPT-STREAM"   ;macro
   "WITH-PORT-LOCKED"                   ;macro
   "WITH-PRESENTATION-TYPE-DECODED"     ;macro
   "WITH-PRESENTATION-TYPE-OPTIONS"     ;macro
   "WITH-PRESENTATION-TYPE-PARAMETERS"  ;macro
   "WITH-RADIO-BOX"                     ;macro
   "WITH-ROOM-FOR-GRAPHICS"             ;macro
   "WITH-ROTATION"                      ;macro
   "WITH-SCALING"                       ;macro
   "WITH-SHEET-MEDIUM"                  ;macro
   "WITH-SHEET-MEDIUM-BOUND"            ;macro
   "WITH-TEXT-FACE"                     ;macro
   "WITH-TEXT-FAMILY"                   ;macro
   "WITH-TEXT-SIZE"                     ;macro
   "WITH-TEXT-STYLE"                    ;macro
   "WITH-TRANSLATION"                   ;macro
   "WRITE-TOKEN"                        ;function
   )

  ;;;; symbols, which were exported as of 2002-02-09, but no longer are.

  ;; DISPATCH-REPAINT:
  ;; several mentions in silica.tex.

  ;; INVOKE-ACCEPT-VALUES-COMMAND-BUTTON
  ;; mentioned in dialogs.tex.

  ;; LABELLED

  ;; LABELLED-GADGET, through there is a LABELLED-GADGET-MIXIN
  ;; MUTE-REPAINTING-MIXIN, through there is a SHEET-MUTE-REPAINTING-MIXIN
  ;; ORIENTED-GADGET, through there is a ORIENTED-GADGET-MIXIN

  ;; OUTPUT-RECORD-REFINED-SENSITIVITY-TEST:
  ;; There is mention of this symbol in output-recording.tex. Spelling error?

  ;; POINTER-BUTTON-CLICK-EVENT
  ;; This is mentioned in silica.tex. Spelling error?

  ;;;; absolutly no mention of the following in the spec:
  
  ;; ADD-WATCHER
  ;; BORDERING
  ;; BORDER-PANE
  ;; DELETE-WATCHER
  ;; DISPLAY-CURSOR
  ;; DRAW-TRIANGLE
  ;; DRAW-TRIANGLE*
  ;; FRAME-PANE
  ;; GADGET-LABEL-TEXT-STYLE
  ;; GESTURE-PROCESSING-HANDLER
  ;; KEY-MODIFIER-STATE-MATCH-P
  ;; MUTE-SHEET-INPUT-MIXIN
  ;; MUTE-SHEET-OUTPUT-MIXIN
  ;; NOTE-FRAME-STATE-CHANGED
  ;; PANES-NEED-REDISPLAY
  ;; POINTER-BUTTON-CLICK-AND-HOLD-EVENT
  ;; POINTER-BUTTON-DOUBLE-CLICK-EVENT
  ;; POINTER-BUTTONS
  ;; POINTER-PORT
  ;; PORT-DRAW-CHARACTER*
  ;; PORT-DRAW-ELLIPSE*
  ;; PORT-DRAW-LINE*
  ;; PORT-DRAW-LINES*
  ;; PORT-DRAW-POINT*
  ;; PORT-DRAW-POINTS*
  ;; PORT-DRAW-POLYGON*
  ;; PORT-DRAW-RECTANGLE*
  ;; PORT-DRAW-STRING*
  ;; PUSH-BUTTON-SHOW-AS-DEFAULT-P
  ;; RESET-WATCHER
  ;; SCROLL-BAR-DRAG-DOWN-LINE-CALLBACK
  ;; SCROLL-BAR-DRAG-DOWN-PAGE-CALLBACK
  ;; SCROLL-BAR-DRAG-UP-LINE-CALLBACK
  ;; SCROLL-BAR-DRAG-UP-PAGE-CALLBACK
  ;; SPACER-PANE
  ;; STANDARD-GADGET
  ;; STREAM-POINTERS
  ;; STREAM-PRIMARY-POINTER
  ;; STREAM-REDISPLAYABLE-P
  ;; STREAM-RESTORE-INPUT-FOCUS

  
   ;;; X11 COLOR NAMES - SOME ARE NOT IN THE SPEC - MIKEMAC
  (:export
   "+SNOW+" "+GHOST-WHITE+" "+GHOSTWHITE+" "+WHITE-SMOKE+"
   "+WHITESMOKE+" "+GAINSBORO+" "+FLORAL-WHITE+" "+FLORALWHITE+"
   "+OLD-LACE+" "+OLDLACE+" "+LINEN+" "+ANTIQUE-WHITE+"
   "+ANTIQUEWHITE+" "+PAPAYA-WHIP+" "+PAPAYAWHIP+" "+BLANCHED-ALMOND+"
   "+BLANCHEDALMOND+" "+BISQUE+" "+PEACH-PUFF+" "+PEACHPUFF+"
   "+NAVAJO-WHITE+" "+NAVAJOWHITE+" "+MOCCASIN+" "+CORNSILK+"
   "+IVORY+" "+LEMON-CHIFFON+" "+LEMONCHIFFON+" "+SEASHELL+"
   "+HONEYDEW+" "+MINT-CREAM+" "+MINTCREAM+" "+AZURE+"
   "+ALICE-BLUE+" "+ALICEBLUE+" "+LAVENDER+" "+LAVENDER-BLUSH+"
   "+LAVENDERBLUSH+" "+MISTY-ROSE+" "+MISTYROSE+" "+WHITE+"
   "+BLACK+" "+DARK-SLATE-GRAY+" "+DARKSLATEGRAY+" "+DARK-SLATE-GREY+"
   "+DARKSLATEGREY+" "+DIM-GRAY+" "+DIMGRAY+" "+DIM-GREY+"
   "+DIMGREY+" "+SLATE-GRAY+" "+SLATEGRAY+" "+SLATE-GREY+"
   "+SLATEGREY+" "+LIGHT-SLATE-GRAY+" "+LIGHTSLATEGRAY+" "+LIGHT-SLATE-GREY+"
   "+LIGHTSLATEGREY+" "+GRAY+" "+GREY+" "+LIGHT-GREY+"
   "+LIGHTGREY+" "+LIGHT-GRAY+" "+LIGHTGRAY+" "+MIDNIGHT-BLUE+"
   "+MIDNIGHTBLUE+" "+NAVY+" "+NAVY-BLUE+" "+NAVYBLUE+"
   "+CORNFLOWER-BLUE+" "+CORNFLOWERBLUE+" "+DARK-SLATE-BLUE+" "+DARKSLATEBLUE+"
   "+SLATE-BLUE+" "+SLATEBLUE+" "+MEDIUM-SLATE-BLUE+" "+MEDIUMSLATEBLUE+"
   "+LIGHT-SLATE-BLUE+" "+LIGHTSLATEBLUE+" "+MEDIUM-BLUE+" "+MEDIUMBLUE+"
   "+ROYAL-BLUE+" "+ROYALBLUE+" "+BLUE+" "+DODGER-BLUE+"
   "+DODGERBLUE+" "+DEEP-SKY-BLUE+" "+DEEPSKYBLUE+" "+SKY-BLUE+"
   "+SKYBLUE+" "+LIGHT-SKY-BLUE+" "+LIGHTSKYBLUE+" "+STEEL-BLUE+"
   "+STEELBLUE+" "+LIGHT-STEEL-BLUE+" "+LIGHTSTEELBLUE+" "+LIGHT-BLUE+"
   "+LIGHTBLUE+" "+POWDER-BLUE+" "+POWDERBLUE+" "+PALE-TURQUOISE+"
   "+PALETURQUOISE+" "+DARK-TURQUOISE+" "+DARKTURQUOISE+" "+MEDIUM-TURQUOISE+"
   "+MEDIUMTURQUOISE+" "+TURQUOISE+" "+CYAN+" "+LIGHT-CYAN+"
   "+LIGHTCYAN+" "+CADET-BLUE+" "+CADETBLUE+" "+MEDIUM-AQUAMARINE+"
   "+MEDIUMAQUAMARINE+" "+AQUAMARINE+" "+DARK-GREEN+" "+DARKGREEN+"
   "+DARK-OLIVE-GREEN+" "+DARKOLIVEGREEN+" "+DARK-SEA-GREEN+" "+DARKSEAGREEN+"
   "+SEA-GREEN+" "+SEAGREEN+" "+MEDIUM-SEA-GREEN+" "+MEDIUMSEAGREEN+"
   "+LIGHT-SEA-GREEN+" "+LIGHTSEAGREEN+" "+PALE-GREEN+" "+PALEGREEN+"
   "+SPRING-GREEN+" "+SPRINGGREEN+" "+LAWN-GREEN+" "+LAWNGREEN+"
   "+GREEN+" "+CHARTREUSE+" "+MEDIUM-SPRING-GREEN+" "+MEDIUMSPRINGGREEN+"
   "+GREEN-YELLOW+" "+GREENYELLOW+" "+LIME-GREEN+" "+LIMEGREEN+"
   "+YELLOW-GREEN+" "+YELLOWGREEN+" "+FOREST-GREEN+" "+FORESTGREEN+"
   "+OLIVE-DRAB+" "+OLIVEDRAB+" "+DARK-KHAKI+" "+DARKKHAKI+"
   "+KHAKI+" "+PALE-GOLDENROD+" "+PALEGOLDENROD+" "+LIGHT-GOLDENROD-YELLOW+"
   "+LIGHTGOLDENRODYELLOW+" "+LIGHT-YELLOW+" "+LIGHTYELLOW+" "+YELLOW+"
   "+GOLD+" "+LIGHT-GOLDENROD+" "+LIGHTGOLDENROD+" "+GOLDENROD+"
   "+DARK-GOLDENROD+" "+DARKGOLDENROD+" "+ROSY-BROWN+" "+ROSYBROWN+"
   "+INDIAN-RED+" "+INDIANRED+" "+SADDLE-BROWN+" "+SADDLEBROWN+"
   "+SIENNA+" "+PERU+" "+BURLYWOOD+" "+BEIGE+"
   "+WHEAT+" "+SANDY-BROWN+" "+SANDYBROWN+" "+TAN+"
   "+CHOCOLATE+" "+FIREBRICK+" "+BROWN+" "+DARK-SALMON+"
   "+DARKSALMON+" "+SALMON+" "+LIGHT-SALMON+" "+LIGHTSALMON+"
   "+ORANGE+" "+DARK-ORANGE+" "+DARKORANGE+" "+CORAL+"
   "+LIGHT-CORAL+" "+LIGHTCORAL+" "+TOMATO+" "+ORANGE-RED+"
   "+ORANGERED+" "+RED+" "+HOT-PINK+" "+HOTPINK+"
   "+DEEP-PINK+" "+DEEPPINK+" "+PINK+" "+LIGHT-PINK+"
   "+LIGHTPINK+" "+PALE-VIOLET-RED+" "+PALEVIOLETRED+" "+MAROON+"
   "+MEDIUM-VIOLET-RED+" "+MEDIUMVIOLETRED+" "+VIOLET-RED+" "+VIOLETRED+"
   "+MAGENTA+" "+VIOLET+" "+PLUM+" "+ORCHID+"
   "+MEDIUM-ORCHID+" "+MEDIUMORCHID+" "+DARK-ORCHID+" "+DARKORCHID+"
   "+DARK-VIOLET+" "+DARKVIOLET+" "+BLUE-VIOLET+" "+BLUEVIOLET+"
   "+PURPLE+" "+MEDIUM-PURPLE+" "+MEDIUMPURPLE+" "+THISTLE+"
   "+SNOW1+" "+SNOW2+" "+SNOW3+" "+SNOW4+"
   "+SEASHELL1+" "+SEASHELL2+" "+SEASHELL3+" "+SEASHELL4+"
   "+ANTIQUEWHITE1+" "+ANTIQUEWHITE2+" "+ANTIQUEWHITE3+" "+ANTIQUEWHITE4+"
   "+BISQUE1+" "+BISQUE2+" "+BISQUE3+" "+BISQUE4+"
   "+PEACHPUFF1+" "+PEACHPUFF2+" "+PEACHPUFF3+" "+PEACHPUFF4+"
   "+NAVAJOWHITE1+" "+NAVAJOWHITE2+" "+NAVAJOWHITE3+" "+NAVAJOWHITE4+"
   "+LEMONCHIFFON1+" "+LEMONCHIFFON2+" "+LEMONCHIFFON3+" "+LEMONCHIFFON4+"
   "+CORNSILK1+" "+CORNSILK2+" "+CORNSILK3+" "+CORNSILK4+"
   "+IVORY1+" "+IVORY2+" "+IVORY3+" "+IVORY4+"
   "+HONEYDEW1+" "+HONEYDEW2+" "+HONEYDEW3+" "+HONEYDEW4+"
   "+LAVENDERBLUSH1+" "+LAVENDERBLUSH2+" "+LAVENDERBLUSH3+" "+LAVENDERBLUSH4+"
   "+MISTYROSE1+" "+MISTYROSE2+" "+MISTYROSE3+" "+MISTYROSE4+"
   "+AZURE1+" "+AZURE2+" "+AZURE3+" "+AZURE4+"
   "+SLATEBLUE1+" "+SLATEBLUE2+" "+SLATEBLUE3+" "+SLATEBLUE4+"
   "+ROYALBLUE1+" "+ROYALBLUE2+" "+ROYALBLUE3+" "+ROYALBLUE4+"
   "+BLUE1+" "+BLUE2+" "+BLUE3+" "+BLUE4+"
   "+DODGERBLUE1+" "+DODGERBLUE2+" "+DODGERBLUE3+" "+DODGERBLUE4+"
   "+STEELBLUE1+" "+STEELBLUE2+" "+STEELBLUE3+" "+STEELBLUE4+"
   "+DEEPSKYBLUE1+" "+DEEPSKYBLUE2+" "+DEEPSKYBLUE3+" "+DEEPSKYBLUE4+"
   "+SKYBLUE1+" "+SKYBLUE2+" "+SKYBLUE3+" "+SKYBLUE4+"
   "+LIGHTSKYBLUE1+" "+LIGHTSKYBLUE2+" "+LIGHTSKYBLUE3+" "+LIGHTSKYBLUE4+"
   "+SLATEGRAY1+" "+SLATEGRAY2+" "+SLATEGRAY3+" "+SLATEGRAY4+"
   "+LIGHTSTEELBLUE1+" "+LIGHTSTEELBLUE2+" "+LIGHTSTEELBLUE3+" "+LIGHTSTEELBLUE4+"
   "+LIGHTBLUE1+" "+LIGHTBLUE2+" "+LIGHTBLUE3+" "+LIGHTBLUE4+"
   "+LIGHTCYAN1+" "+LIGHTCYAN2+" "+LIGHTCYAN3+" "+LIGHTCYAN4+"
   "+PALETURQUOISE1+" "+PALETURQUOISE2+" "+PALETURQUOISE3+" "+PALETURQUOISE4+"
   "+CADETBLUE1+" "+CADETBLUE2+" "+CADETBLUE3+" "+CADETBLUE4+"
   "+TURQUOISE1+" "+TURQUOISE2+" "+TURQUOISE3+" "+TURQUOISE4+"
   "+CYAN1+" "+CYAN2+" "+CYAN3+" "+CYAN4+"
   "+DARKSLATEGRAY1+" "+DARKSLATEGRAY2+" "+DARKSLATEGRAY3+" "+DARKSLATEGRAY4+"
   "+AQUAMARINE1+" "+AQUAMARINE2+" "+AQUAMARINE3+" "+AQUAMARINE4+"
   "+DARKSEAGREEN1+" "+DARKSEAGREEN2+" "+DARKSEAGREEN3+" "+DARKSEAGREEN4+"
   "+SEAGREEN1+" "+SEAGREEN2+" "+SEAGREEN3+" "+SEAGREEN4+"
   "+PALEGREEN1+" "+PALEGREEN2+" "+PALEGREEN3+" "+PALEGREEN4+"
   "+SPRINGGREEN1+" "+SPRINGGREEN2+" "+SPRINGGREEN3+" "+SPRINGGREEN4+"
   "+GREEN1+" "+GREEN2+" "+GREEN3+" "+GREEN4+"
   "+CHARTREUSE1+" "+CHARTREUSE2+" "+CHARTREUSE3+" "+CHARTREUSE4+"
   "+OLIVEDRAB1+" "+OLIVEDRAB2+" "+OLIVEDRAB3+" "+OLIVEDRAB4+"
   "+DARKOLIVEGREEN1+" "+DARKOLIVEGREEN2+" "+DARKOLIVEGREEN3+" "+DARKOLIVEGREEN4+"
   "+KHAKI1+" "+KHAKI2+" "+KHAKI3+" "+KHAKI4+"
   "+LIGHTGOLDENROD1+" "+LIGHTGOLDENROD2+" "+LIGHTGOLDENROD3+" "+LIGHTGOLDENROD4+"
   "+LIGHTYELLOW1+" "+LIGHTYELLOW2+" "+LIGHTYELLOW3+" "+LIGHTYELLOW4+"
   "+YELLOW1+" "+YELLOW2+" "+YELLOW3+" "+YELLOW4+"
   "+GOLD1+" "+GOLD2+" "+GOLD3+" "+GOLD4+"
   "+GOLDENROD1+" "+GOLDENROD2+" "+GOLDENROD3+" "+GOLDENROD4+"
   "+DARKGOLDENROD1+" "+DARKGOLDENROD2+" "+DARKGOLDENROD3+" "+DARKGOLDENROD4+"
   "+ROSYBROWN1+" "+ROSYBROWN2+" "+ROSYBROWN3+" "+ROSYBROWN4+"
   "+INDIANRED1+" "+INDIANRED2+" "+INDIANRED3+" "+INDIANRED4+"
   "+SIENNA1+" "+SIENNA2+" "+SIENNA3+" "+SIENNA4+"
   "+BURLYWOOD1+" "+BURLYWOOD2+" "+BURLYWOOD3+" "+BURLYWOOD4+"
   "+WHEAT1+" "+WHEAT2+" "+WHEAT3+" "+WHEAT4+"
   "+TAN1+" "+TAN2+" "+TAN3+" "+TAN4+"
   "+CHOCOLATE1+" "+CHOCOLATE2+" "+CHOCOLATE3+" "+CHOCOLATE4+"
   "+FIREBRICK1+" "+FIREBRICK2+" "+FIREBRICK3+" "+FIREBRICK4+"
   "+BROWN1+" "+BROWN2+" "+BROWN3+" "+BROWN4+"
   "+SALMON1+" "+SALMON2+" "+SALMON3+" "+SALMON4+"
   "+LIGHTSALMON1+" "+LIGHTSALMON2+" "+LIGHTSALMON3+" "+LIGHTSALMON4+"
   "+ORANGE1+" "+ORANGE2+" "+ORANGE3+" "+ORANGE4+"
   "+DARKORANGE1+" "+DARKORANGE2+" "+DARKORANGE3+" "+DARKORANGE4+"
   "+CORAL1+" "+CORAL2+" "+CORAL3+" "+CORAL4+"
   "+TOMATO1+" "+TOMATO2+" "+TOMATO3+" "+TOMATO4+"
   "+ORANGERED1+" "+ORANGERED2+" "+ORANGERED3+" "+ORANGERED4+"
   "+RED1+" "+RED2+" "+RED3+" "+RED4+"
   "+DEEPPINK1+" "+DEEPPINK2+" "+DEEPPINK3+" "+DEEPPINK4+"
   "+HOTPINK1+" "+HOTPINK2+" "+HOTPINK3+" "+HOTPINK4+"
   "+PINK1+" "+PINK2+" "+PINK3+" "+PINK4+"
   "+LIGHTPINK1+" "+LIGHTPINK2+" "+LIGHTPINK3+" "+LIGHTPINK4+"
   "+PALEVIOLETRED1+" "+PALEVIOLETRED2+" "+PALEVIOLETRED3+" "+PALEVIOLETRED4+"
   "+MAROON1+" "+MAROON2+" "+MAROON3+" "+MAROON4+"
   "+VIOLETRED1+" "+VIOLETRED2+" "+VIOLETRED3+" "+VIOLETRED4+"
   "+MAGENTA1+" "+MAGENTA2+" "+MAGENTA3+" "+MAGENTA4+"
   "+ORCHID1+" "+ORCHID2+" "+ORCHID3+" "+ORCHID4+"
   "+PLUM1+" "+PLUM2+" "+PLUM3+" "+PLUM4+"
   "+MEDIUMORCHID1+" "+MEDIUMORCHID2+" "+MEDIUMORCHID3+" "+MEDIUMORCHID4+"
   "+DARKORCHID1+" "+DARKORCHID2+" "+DARKORCHID3+" "+DARKORCHID4+"
   "+PURPLE1+" "+PURPLE2+" "+PURPLE3+" "+PURPLE4+"
   "+MEDIUMPURPLE1+" "+MEDIUMPURPLE2+" "+MEDIUMPURPLE3+" "+MEDIUMPURPLE4+"
   "+THISTLE1+" "+THISTLE2+" "+THISTLE3+" "+THISTLE4+"
   "+GRAY0+" "+GREY0+" "+GRAY1+" "+GREY1+"
   "+GRAY2+" "+GREY2+" "+GRAY3+" "+GREY3+"
   "+GRAY4+" "+GREY4+" "+GRAY5+" "+GREY5+"
   "+GRAY6+" "+GREY6+" "+GRAY7+" "+GREY7+"
   "+GRAY8+" "+GREY8+" "+GRAY9+" "+GREY9+"
   "+GRAY10+" "+GREY10+" "+GRAY11+" "+GREY11+"
   "+GRAY12+" "+GREY12+" "+GRAY13+" "+GREY13+"
   "+GRAY14+" "+GREY14+" "+GRAY15+" "+GREY15+"
   "+GRAY16+" "+GREY16+" "+GRAY17+" "+GREY17+"
   "+GRAY18+" "+GREY18+" "+GRAY19+" "+GREY19+"
   "+GRAY20+" "+GREY20+" "+GRAY21+" "+GREY21+"
   "+GRAY22+" "+GREY22+" "+GRAY23+" "+GREY23+"
   "+GRAY24+" "+GREY24+" "+GRAY25+" "+GREY25+"
   "+GRAY26+" "+GREY26+" "+GRAY27+" "+GREY27+"
   "+GRAY28+" "+GREY28+" "+GRAY29+" "+GREY29+"
   "+GRAY30+" "+GREY30+" "+GRAY31+" "+GREY31+"
   "+GRAY32+" "+GREY32+" "+GRAY33+" "+GREY33+"
   "+GRAY34+" "+GREY34+" "+GRAY35+" "+GREY35+"
   "+GRAY36+" "+GREY36+" "+GRAY37+" "+GREY37+"
   "+GRAY38+" "+GREY38+" "+GRAY39+" "+GREY39+"
   "+GRAY40+" "+GREY40+" "+GRAY41+" "+GREY41+"
   "+GRAY42+" "+GREY42+" "+GRAY43+" "+GREY43+"
   "+GRAY44+" "+GREY44+" "+GRAY45+" "+GREY45+"
   "+GRAY46+" "+GREY46+" "+GRAY47+" "+GREY47+"
   "+GRAY48+" "+GREY48+" "+GRAY49+" "+GREY49+"
   "+GRAY50+" "+GREY50+" "+GRAY51+" "+GREY51+"
   "+GRAY52+" "+GREY52+" "+GRAY53+" "+GREY53+"
   "+GRAY54+" "+GREY54+" "+GRAY55+" "+GREY55+"
   "+GRAY56+" "+GREY56+" "+GRAY57+" "+GREY57+"
   "+GRAY58+" "+GREY58+" "+GRAY59+" "+GREY59+"
   "+GRAY60+" "+GREY60+" "+GRAY61+" "+GREY61+"
   "+GRAY62+" "+GREY62+" "+GRAY63+" "+GREY63+"
   "+GRAY64+" "+GREY64+" "+GRAY65+" "+GREY65+"
   "+GRAY66+" "+GREY66+" "+GRAY67+" "+GREY67+"
   "+GRAY68+" "+GREY68+" "+GRAY69+" "+GREY69+"
   "+GRAY70+" "+GREY70+" "+GRAY71+" "+GREY71+"
   "+GRAY72+" "+GREY72+" "+GRAY73+" "+GREY73+"
   "+GRAY74+" "+GREY74+" "+GRAY75+" "+GREY75+"
   "+GRAY76+" "+GREY76+" "+GRAY77+" "+GREY77+"
   "+GRAY78+" "+GREY78+" "+GRAY79+" "+GREY79+"
   "+GRAY80+" "+GREY80+" "+GRAY81+" "+GREY81+"
   "+GRAY82+" "+GREY82+" "+GRAY83+" "+GREY83+"
   "+GRAY84+" "+GREY84+" "+GRAY85+" "+GREY85+"
   "+GRAY86+" "+GREY86+" "+GRAY87+" "+GREY87+"
   "+GRAY88+" "+GREY88+" "+GRAY89+" "+GREY89+"
   "+GRAY90+" "+GREY90+" "+GRAY91+" "+GREY91+"
   "+GRAY92+" "+GREY92+" "+GRAY93+" "+GREY93+"
   "+GRAY94+" "+GREY94+" "+GRAY95+" "+GREY95+"
   "+GRAY96+" "+GREY96+" "+GRAY97+" "+GREY97+"
   "+GRAY98+" "+GREY98+" "+GRAY99+" "+GREY99+"
   "+GRAY100+" "+GREY100+" "+DARK-GREY+" "+DARKGREY+"
   "+DARK-GRAY+" "+DARKGRAY+" "+DARK-BLUE+" "+DARKBLUE+"
   "+DARK-CYAN+" "+DARKCYAN+" "+DARK-MAGENTA+" "+DARKMAGENTA+"
   "+DARK-RED+" "+DARKRED+" "+LIGHT-GREEN+" "+LIGHTGREEN+" ))

(defpackage :clim-sys
  (:use)
  ;;
  #+CMU
  (:import-from "MP"
   #:make-process
   #:destroy-process
   #:current-process
   #:all-processes
   #:processp
   #:process-name
   #:process-state
   #:process-whostate
   #:process-wait
   #:process-wait-with-timeout
   #:process-yield
   #:process-interrupt
   #:disable-process
   #:enable-process
   #:restart-process
   #:without-scheduling
   #:atomic-incf
   #:atomic-decf)
  ;;
  (:export
   #:defresource
   #:using-resource
   #:allocate-resource
   #:deallocate-resource
   #:clear-resource
   #:map-resource
   ;;
   #:*multiprocessing-p*
   #:make-process
   #:destroy-process
   #:current-process
   #:all-processes
   #:processp
   #:process-name
   #:process-state
   #:process-whostate
   #:process-wait
   #:process-wait-with-timeout
   #:process-yield
   #:process-interrupt
   #:disable-process
   #:enable-process
   #:restart-process
   #:without-scheduling
   #:atomic-incf
   #:atomic-decf
   ;;
   #:make-lock
   #:with-lock-held
   #:make-recursive-lock
   #:with-recursive-lock-held
   ;;
   #:defgeneric*
   #:defmethod* ) )

(defpackage "CLIM-EXTENSIONS"
  (:use)
  (:export
   "RAISED-PANE" "RAISING" 
   "DRAW-GLYPH" "DEVICE-FONT-TEXT-STYLE-P"
   "READ-IMAGE-FILE"
   "IMAGE" "IMAGE-COLOR" "IMAGE-GADGET" "IMAGE-HEIGHT"
   "IMAGE-PIXEL" "IMAGE-PIXELS" "IMAGE-WIDTH"
   "RGB-IMAGE"
   "GRAY-LEVEL-IMAGE" "256-GRAY-LEVEL-IMAGE" "MAKE-256-GRAY-LEVEL-IMAGE"
   "GRAY-IMAGE-MIN-LEVEL" "GRAY-IMAGE-MIN-LEVEL"
   "TRUECOLOR-IMAGE" "MAKE-TRUECOLOR-IMAGE" "MAKE-3X256-COLOR-IMAGE"
   "COLOR-IMAGE-MIN-LEVEL" "COLOR-IMAGE-MAX-LEVEL"
   "BINARY-IMAGE" "MAKE-BINARY-IMAGE" 
   "COLORMAP-IMAGE"
   "SPECTRAL-IMAGE"
   "DRAW-IMAGE"
   "IMAGE-PANE"
   "DRAW-LABEL"
   "COMPOSE-SPACE-AUX"
   "SIMPLE-EVENT-LOOP"))

(defpackage "CLIM-INTERNALS"
  (:use #:clim #:clim-sys #:clim-extensions #:clim-lisp)
  (:nicknames :climi)
  #+excl
  (:import-from :excl compile-system load-system) )

;(defpackage :CLIM
;  (:use #+nil :clim-extensions ; will use it later
;        :clim-internals :common-lisp)
;  )

;(let ((climi-package (find-package :climi))
;      (ext-package   (find-package :clim-extensions)))
;  (do-external-symbols (sym ext-package)
;    (export sym climi-package)))

;(let ((clim-package  (find-package :clim))
;      (climi-package (find-package :climi)))
;  (do-external-symbols (sym climi-package)
;    (export sym clim-package)))

;(use-package :clim-extensions :clim)

(defpackage :CLIM-DEMO
  (:use :clim-extensions :clim :clim-lisp)
  #+excl(:import-from :excl compile-system load-system exit)
  )

(defpackage :CLIM-USER
  (:use :clim :clim-lisp))

(defpackage :GOATEE
  (:use :clim :clim-lisp))

