
(in-package :glimpse)

(define-glimpse-command (com-arrest-process :name t :menu nil)
  ((obj 'ccl::process    ; need to generalize this...
	:prompt "process"
	:gesture t))
  (clim-sys:disable-process obj)
  ;; Redisplay changed state...
  (com-show-processes))

;;; can we disable this for inactive processes?
(define-glimpse-command (com-unarrest-process :name t :menu nil)
  ((obj 'ccl::process    ; need to generalize this...
	:prompt "process"
	:gesture t))
  (clim-sys:enable-process obj)
  (com-show-processes))

;; Should prompt for confirmation... how?
(define-glimpse-command (com-reset-process :name t :menu nil)
  ((obj 'ccl::process    ; need to generalize this...
	:prompt "process"
	:gesture t))
  (clim-sys:restart-process obj)
  (com-show-processes))

;; Should prompt for confirmation... how?
(define-glimpse-command (com-reset-and-enable-process :name t :menu nil)
  ((obj 'ccl::process    ; need to generalize this...
	:prompt "process"
	:gesture t))
  (clim-sys:disable-process obj)
  (clim-sys:restart-process obj)
  (clim-sys:enable-process obj)
  (com-show-processes))

;; Should prompt for confirmation... how?
(define-glimpse-command (com-kill-process :name t :menu nil)
  ((obj 'ccl::process    ; need to generalize this...
	:prompt "process"
	:gesture t))
  (clim-sys:destroy-process obj)
  (com-show-processes))

