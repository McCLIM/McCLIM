(cl:defvar *output-dir* #P"docstrings/")

(cl:defvar *mcclim-documentee*
  (cl:make-instance 'cl-docextractor:documentee
                    :name "McCLIM"
                    :output-directory *output-dir*
                    :packages '(:clim :drei :drei-buffer :drei-undo :drei-kill-ring
                                :drei-base :drei-abbrev :drei-syntax :drei-motion
                                :drei-editing :drei-core :esa :clim-extensions
				:clim-tab-layout)
                    :ignored-packages '(:clim-internals)
                    :filetype "texi"))

(cl-docextractor:document *mcclim-documentee*)
