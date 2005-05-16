
(in-package :glimpse)

;; 'General' glimpse commands (those that operate on the application, select a mode,
;; or that are always applicable) are placed in here.
(define-command-table glimpse-commands)

;; Commands specific to WINDOW MODE go in here...
(define-command-table glimpse-window-mode-cmds :inherit-from (glimpse-commands))

;; Commands specific to PROCESS MODE go in here...
(define-command-table glimpse-process-mode-cmds :inherit-from (glimpse-commands))
