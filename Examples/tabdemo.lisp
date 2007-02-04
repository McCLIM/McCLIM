(in-package :cl-user)

(defpackage :tabdemo
  (:use :clim :clim-lisp :clim-tab-layout)
  (:export :tabdemo))

(in-package :tabdemo)

;;; example and testing code

(define-presentation-type special-page ())

(define-application-frame tabdemo ()
    ()
  (:menu-bar tabdemo-menubar)
  (:panes
   (a :text-editor :value "Hello World from page A")
   (b :text-editor :value "Hello World from page B")
   (c :text-editor :value "This is page C speaking")
   (special-page :text-editor
		 :value "This page has a special presentation type")
   (io :interactor :height 150 :width 600)
   (pointer-doc :pointer-documentation))
  (:layouts
   (default
       (vertically ()
	 (with-tab-layout ('tab-page :name 'tabdemo-layout :height 200)
           ("A" a)
           ("B" b)
           ("C" c)
           ("Special Page" special-page :presentation-type 'special-page))
	 io
	 pointer-doc))))

(define-tabdemo-command (com-remove-tabdemo-page :name t)
    ((page 'tab-page :prompt "Tab page" :gesture :delete))
  (remove-page page))

(make-command-table 'tabdemo-pages-menu
		    :errorp nil
		    :menu '(("Add Extra Pane" :command com-add-extra-pane)
			    ("Randomize" :command com-randomize-tabdemo)
			    ("Quit" :command com-quit-tabdemo)))

(make-command-table 'tabdemo-properties-menu
		    :errorp nil
		    :menu '(("Change Page Title"
			     :command com-change-page-title)
			    ("Paint Page Red"
			     :command com-paint-page-red)
			    ("Paint Page Green"
			     :command com-paint-page-green)))

(make-command-table 'tabdemo-presentation-tests-menu
		    :errorp nil
		    :menu '(("Choose Any Page"
			     :command com-choose-any-page)
			    ("Choose Special Page"
			     :command com-choose-special-page)))

(make-command-table 'tabdemo-menubar
		    :errorp nil
		    :menu '(("Pages" :menu tabdemo-pages-menu)
			    ("Properties" :menu tabdemo-properties-menu)
			    ("Presentation Tests"
			     :menu tabdemo-presentation-tests-menu)))

(defun tabdemo ()
  (run-frame-top-level (make-application-frame 'tabdemo)))

;;;(define-presentation-to-command-translator remove-pane
;;;    (tab-page com-remove-tab-page tabdemo
;;;	      :gesture :describe
;;;	      :documentation "remove this pane"
;;;	      :pointer-documentation "remove this pane")
;;;  (object)
;;;  (list object))


;; FIXME: It only get errors due to bogus frame names with FIND-PANE-NAMED.
;; Ignoring the symbol identity and case works around that.
(defun sane-find-pane-named (frame name)
  (find name
	(climi::frame-named-panes frame)
	:key #'pane-name
	:test #'string-equal))

(defun tabdemo-layout ()
  (sane-find-pane-named *application-frame* 'tabdemo-layout))

(define-tabdemo-command (com-add-extra-pane :name t)
    ()
  (let ((fm (frame-manager *application-frame*)))
    (with-look-and-feel-realization (fm *application-frame*)
      (add-page (make-instance 'tab-page
		  :title "X"
		  :pane (make-pane 'text-editor-pane
				   :value "This is an extra page"))
                (tabdemo-layout)
		t))))

(define-tabdemo-command (com-choose-any-page :name t)
    ()
  (format *standard-input* "You choice: ~A~%" (accept 'tab-page)))

(define-tabdemo-command (com-choose-special-page :name t)
    ()
  (accept 'special-page)
  (write-line "Correct answer!  That's the special page." *standard-input*))

(define-tabdemo-command (com-quit-tabdemo :name t)
    ()
  (frame-exit *application-frame*))

(define-tabdemo-command (com-randomize-tabdemo :name t)
    ()
  (setf (tab-layout-pages (tabdemo-layout))
	(let ((old (tab-layout-pages (tabdemo-layout)))
	      (new '()))
	  (loop
	      while old
	      for i = (random (length old))
	      do
		(push (elt old i) new)
		(setf old (remove-if (constantly t) old :start i :count 1)))
	  new)))

(define-tabdemo-command (com-change-page-title :name t)
    ()
  (let ((page (tab-layout-enabled-page (tabdemo-layout))))
    (when page
      (setf (tab-page-title page)
	    (accept 'string
		    :prompt "New title"
		    :default (tab-page-title page))))))

(define-tabdemo-command (com-paint-page-red :name t)
    ()
  (let ((page (tab-layout-enabled-page (tabdemo-layout))))
    (when page
      (setf (getf (tab-page-drawing-options page) :ink) +red+))))

(define-tabdemo-command (com-paint-page-green :name t)
    ()
  (let ((page (tab-layout-enabled-page (tabdemo-layout))))
    (when page
      (setf (getf (tab-page-drawing-options page) :ink) +green+))))

#+(or)
(tabdemo:tabdemo)
