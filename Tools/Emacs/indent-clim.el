(put 'define-border-type 'lisp-indent-function 2) 
(put 'define-command 'lisp-indent-function 2)  
(put 'formatting-row 'lisp-indent-function 1) 
(put 'do-command-table-inheritance 'lisp-indent-function 1) 
(put 'define-presentation-to-command-translator 'lisp-indent-function 3) 
(put 'with-sheet-medium 'lisp-indent-function 1) 
(put 'with-new-output-record 'lisp-indent-function 1) 
(put 'with-activation-gestures 'lisp-indent-function 1) 
(put 'with-command-table-keystrokes 'lisp-indent-function 1) 
(put 'with-output-to-postscript-stream 'lisp-indent-function 1) 
(put 'with-radio-box 'lisp-indent-function 1) 
(put 'labelling 'lisp-indent-function 1) 
(put 'spacing 'lisp-indent-function 1)  
(put 'with-text-size 'lisp-indent-function 1) 
(put 'with-scaling 'lisp-indent-function 1)  
(put 'horizontally 'lisp-indent-function 1)  
(put 'surrounding-output-with-border 'lisp-indent-function 1) 
(put 'with-rotation 'lisp-indent-function 1) 
(put 'define-presentation-translator 'lisp-indent-function 3) 
(put 'with-bounding-rectangle* 'lisp-indent-function 2)  
(put 'vertically 'lisp-indent-function 1)  
(put 'with-graft-locked 'lisp-indent-function 1) 
(put 'formatting-table 'lisp-indent-function 1) 
(put 'updating-output 'lisp-indent-function 1) 
(put 'with-input-focus 'lisp-indent-function 1) 
(put 'tabling 'lisp-indent-function 1) 
(put 'indenting-output 'lisp-indent-function 1) 
(put 'changing-space-requirements 'lisp-indent-function 1) 
(put 'with-identity-transformation 'lisp-indent-function 1)  
(put 'with-end-of-page-action 'lisp-indent-function 1) 
(put 'with-translation 'lisp-indent-function 1) 
(put 'formatting-column 'lisp-indent-function 1) 
(put 'with-input-editing 'lisp-indent-function 1) 
(put 'with-text-face 'lisp-indent-function 1) 
(put 'with-drawing-options 'lisp-indent-function 1) 
(put 'with-presentation-type-decoded 'lisp-indent-function 2) 
(put 'with-output-recording-options 'lisp-indent-function 1) 
(put 'with-output-buffered 'lisp-indent-function 1) 
(put 'with-input-context 'lisp-indent-function 3) 
(put 'with-output-as-presentation 'lisp-indent-function 1)  
(put 'tracking-pointer 'lisp-indent-function 1)  
(put 'with-presentation-type-options 'lisp-indent-function 1) 
(put 'with-port-locked 'lisp-indent-function 1)  
(put 'accepting-values 'lisp-indent-function 1) 
(put 'with-room-for-graphics 'lisp-indent-function 1) 
(put 'define-presentation-action 'lisp-indent-function 3) 
(put 'with-presentation-type-parameters 'lisp-indent-function 1) 
(put 'with-output-as-gadget 'lisp-indent-function 1) 
(put 'with-frame-manager 'lisp-indent-function 1) 
(put 'with-text-family 'lisp-indent-function 1)  
(put 'with-end-of-line-action 'lisp-indent-function 1) 
(put 'formatting-item-list 'lisp-indent-function 1) 
(put 'with-sheet-medium-bound 'lisp-indent-function 1)  
(put 'with-output-to-output-record 'lisp-indent-function 1) 
(put 'with-output-to-pixmap 'lisp-indent-function 1) 
(put 'restraining 'lisp-indent-function 1) 
(put 'filling-output 'lisp-indent-function 1) 
(put 'scrolling 'lisp-indent-function 1) 
(put 'with-text-style 'lisp-indent-function 1) 
(put 'formatting-cell 'lisp-indent-function 1)  
(put 'with-look-and-feel-realization 'lisp-indent-function 1)  
(put 'with-menu 'lisp-indent-function 1) 
(put 'with-application-frame 'lisp-indent-function 1) 
(put 'outlining 'lisp-indent-function 1) 
(put 'with-delimiter-gestures 'lisp-indent-function 1)  
(put 'completing-from-suggestions 'lisp-indent-function 1)

;;; Some forms indent much better using the
;;common-lisp-indent-function stuff.

(put 'tracking-pointer 'common-lisp-indent-function
     '((&whole 4 &rest 1) &rest (&whole 1 &lambda &body)))
;;
;;(DEFMACRO CLIM:DEFINE-COMMAND-TABLE
;;          (NAME &KEY (INHERIT-FROM '(GLOBAL-COMMAND-TABLE)) MENU))             
;;
;;(DEFMACRO CLIM:DEFINE-PRESENTATION-TYPE-ABBREVIATION
;;          (NAME PARAMETERS EQUIVALENT-TYPE &KEY OPTIONS))    
;;
;;(DEFMACRO CLIM:SLIDER (&REST OPTIONS))   
;;
;;(DEFMACRO CLIM:DEFINE-APPLICATION-FRAME
;;          (NAME SUPERCLASSES SLOTS &REST OPTIONS))      
;;
;;(DEFMACRO CLIM:DEFINE-GRAPH-TYPE (GRAPH-TYPE CLASS))   
;;
;;(DEFMACRO CLIM:PUSH-BUTTON (&REST OPTIONS))          
;;
;;(DEFMACRO CLIM:TOGGLE-BUTTON (&REST OPTIONS))             
;;
;;(DEFMACRO CLIM:DEFINE-PRESENTATION-GENERIC-FUNCTION
;;          (GENERIC-FUNCTION-NAME PRESENTATION-FUNCTION-NAME LAMBDA-LIST
;;           &REST OPTIONS))   
;;
;;(DEFMACRO CLIM:DEFINE-PRESENTATION-METHOD (NAME &REST ARGS))    
;;
;;(DEFMACRO CLIM:APPLY-PRESENTATION-GENERIC-FUNCTION (NAME &REST ARGS))         
;;
;;(DEFMACRO CLIM:TEXT-FIELD (&REST OPTIONS))     
;;
;;(DEFMACRO CLIM:DEFINE-GESTURE-NAME (NAME TYPE GESTURE-SPEC &KEY (UNIQUE T)))         
;;
;;(DEFMACRO CLIM:DEFINE-DEFAULT-PRESENTATION-METHOD (NAME &REST ARGS))   
;;
;;(DEFMACRO CLIM:FUNCALL-PRESENTATION-GENERIC-FUNCTION (NAME &REST ARGS))      
;;
;;(DEFMACRO CLIM:DEFINE-PRESENTATION-TYPE
;;          (NAME PARAMETERS
;;           &KEY OPTIONS INHERIT-FROM
;;           (DESCRIPTION (MAKE-DEFAULT-DESCRIPTION NAME)) (HISTORY T)
;;           PARAMETERS-ARE-TYPES))  
;;

;;; Generated with this humble code:

;; (defun quux ()
;;   (dolist (sym (remove-if-not #'macro-function
;;                               (apropos-list "" :CLIM t)))
;;     (let ((arglist (read-from-string (ilisp::arglist (symbol-name sym)(package-name (symbol-package sym))))))
;;       (let ((n (position '&body arglist)))
;;         (when n
;;           (format t "~&(put '~(~A~) 'lisp-indent-function ~D)"
;;                   sym n))
;;         (unless n
;;           (fresh-line)
;;           (pprint-logical-block (nil nil :per-line-prefix ";;")
;;              (print `(defmacro ,sym ,arglist))))))))
;; 

;; -- 
;; Gilbert Baumann, 2003-05-19
;; <unk6@rz.uni-karlsruhe.de>
