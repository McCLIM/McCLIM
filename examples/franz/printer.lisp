(in-package :clim-user)

(defun get-printer-options (&key (printer :lw)
				 (copies 1)
				 (orrientation :porta)
				 (multi-page nil)
				 (send-mail nil)
				 (stream *standard-output*))
  (accepting-values (stream :own-window t :label "Printer options"
			    :align-prompts t)
    (setq printer (accept '(member :lw :lw2 :lw3)
			  :default printer 
			  :prompt "Printer" :stream stream))
    (setq copies (accept '(integer 0 10)
			 :default copies
			 :prompt "Copies"
			 :stream stream))
    (setq orrientation (accept '(member :portrait :landscape)
			       :default orrientation
			       :prompt "Orrientation"
			       :stream stream))
    (setq multi-page (accept 'boolean
			     :default multi-page
			     :prompt "Multi-page"
			     :stream stream))
    (setq send-mail (accept 'boolean
			    :default send-mail
			    :prompt "Notify"
			    :stream stream))))

(defmacro with-output-to-printer ((stream &rest options) &body body)
  (invoke-with-output-to-printer #'(lambda (,stream) ,@body) ,@options))

(defun invoke-with-output-to-printer (continuation &rest options)
  (multiple-value-bind (printer copies orrientation multi-page send-mail)
      (apply #'get-printer-options options)
    (with-open-stream 
	(pipe (excl:run-shell-command  (format nil "lpr -P~A ~[~*;-#~D~] ~[-n~]"
					       printer
					       (= 1 copies)
					       copies
					       send-mail)
				       :input :stream :wait nil))
      (with-output-to-postscript-stream (stream pipe 
						:orrientation orrientation
						:multi-page multi-page)
	(funcall continuation stream)))))
