;; do (with-drawing-options (pane :ink +firebrick+ :font-family :fix)
;;      (write-string line pane))
;; (terpri pane)
;; finally (setf unread-line line)

(parse-line "9e2: L0:   488FF CMP RDI, #x20100      ; NIL")
(parse-line "   ; NIL")

(parse-disassembly "9e2: L0:   488FF CMP RDI, #x20100      ; NIL")
