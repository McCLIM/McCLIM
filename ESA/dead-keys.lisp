;;; -*- Mode: Lisp; Package: ESA -*-

;;;  (c) copyright 2008 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Elegantly handle dead keys by collapsing into single characters.

(in-package :esa)

(defvar *dead-key-table* (make-hash-table :test 'equal)
  "A hash table mapping keyboard event names and characters to
either a similar hash table or characters.")

(defun set-dead-key-combination (character gestures table)
  "Set `gestures' to result in `character' in the hash table
`table' (see `*dead-key-table*' for the format of the hash
table)."
  (assert (not (null gestures)))
  (if (null (rest gestures))
      ;; Just add it directly to this table.
      (setf (gethash (first gestures) table) character)
      ;; Ensure that the subtable exists.
      (let ((new-table (setf (gethash (first gestures) table)
                             (gethash (first gestures) table
                                      (make-hash-table :test 'equal)))))
        (set-dead-key-combination character (rest gestures) new-table))))

(defmacro define-dead-key-combination (character (&rest gestures))
  "Define a dead key combination that results in `character' when
`gestures' (either characters or key names) is entered."
  (assert (>= (length gestures) 2))
  `(set-dead-key-combination ,character ',gestures *dead-key-table*))

(define-dead-key-combination (code-char 193) (:dead-acute #\a))
(define-dead-key-combination (code-char 201) (:dead-acute #\e))
(define-dead-key-combination (code-char 205) (:dead-acute #\i))
(define-dead-key-combination (code-char 211) (:dead-acute #\o))
(define-dead-key-combination (code-char 218) (:dead-acute #\u))
(define-dead-key-combination (code-char 221) (:dead-acute #\y))
(define-dead-key-combination (code-char 225) (:dead-acute #\a))
(define-dead-key-combination (code-char 233) (:dead-acute #\e))
(define-dead-key-combination (code-char 237) (:dead-acute #\i))
(define-dead-key-combination (code-char 243) (:dead-acute #\o))
(define-dead-key-combination (code-char 250) (:dead-acute #\u))
(define-dead-key-combination (code-char 253) (:dead-acute #\y))
(define-dead-key-combination (code-char 199) (:dead-acute #\c))
(define-dead-key-combination (code-char 231) (:dead-acute #\c))
(define-dead-key-combination (code-char 215) (:dead-acute #\x))
(define-dead-key-combination (code-char 247) (:dead-acute #\-))
(define-dead-key-combination (code-char 222) (:dead-acute #\t))
(define-dead-key-combination (code-char 254) (:dead-acute #\t))
(define-dead-key-combination (code-char 223) (:dead-acute #\s))
(define-dead-key-combination (code-char 39) (:dead-acute #\space))
(define-dead-key-combination (code-char 197) (:dead-acute :dead-acute #\a))
(define-dead-key-combination (code-char 229) (:dead-acute :dead-acute #\a))
(define-dead-key-combination (code-char 192) (:dead-grave #\a))
(define-dead-key-combination (code-char 200) (:dead-grave #\e))
(define-dead-key-combination (code-char 204) (:dead-grave #\i))
(define-dead-key-combination (code-char 210) (:dead-grave #\o))
(define-dead-key-combination (code-char 217) (:dead-grave #\u))
(define-dead-key-combination (code-char 224) (:dead-grave #\a))
(define-dead-key-combination (code-char 232) (:dead-grave #\e))
(define-dead-key-combination (code-char 236) (:dead-grave #\i))
(define-dead-key-combination (code-char 242) (:dead-grave #\o))
(define-dead-key-combination (code-char 249) (:dead-grave #\u))
(define-dead-key-combination (code-char 96) (:dead-grave #\space))
(define-dead-key-combination (code-char 196) (:dead-diaeresis #\a))
(define-dead-key-combination (code-char 203) (:dead-diaeresis #\e))
(define-dead-key-combination (code-char 207) (:dead-diaeresis #\i))
(define-dead-key-combination (code-char 214) (:dead-diaeresis #\o))
(define-dead-key-combination (code-char 220) (:dead-diaeresis #\u))
(define-dead-key-combination (code-char 228) (:dead-diaeresis #\a))
(define-dead-key-combination (code-char 235) (:dead-diaeresis #\e))
(define-dead-key-combination (code-char 239) (:dead-diaeresis #\i))
(define-dead-key-combination (code-char 246) (:dead-diaeresis #\o))
(define-dead-key-combination (code-char 252) (:dead-diaeresis #\u))
(define-dead-key-combination (code-char 255) (:dead-diaeresis #\y))
(define-dead-key-combination (code-char 34) (:dead-diaeresis #\space))
(define-dead-key-combination (code-char 195) (:dead-tilde #\a))
(define-dead-key-combination (code-char 209) (:dead-tilde #\n))
(define-dead-key-combination (code-char 227) (:dead-tilde #\a))
(define-dead-key-combination (code-char 241) (:dead-tilde #\n))
(define-dead-key-combination (code-char 198) (:dead-tilde #\e))
(define-dead-key-combination (code-char 230) (:dead-tilde #\e))
(define-dead-key-combination (code-char 208) (:dead-tilde #\d))
(define-dead-key-combination (code-char 240) (:dead-tilde #\d))
(define-dead-key-combination (code-char 245) (:dead-tilde #\o))
(define-dead-key-combination (code-char 126) (:dead-tilde #\space))
(define-dead-key-combination (code-char 194) (:dead-circumflex #\a))
(define-dead-key-combination (code-char 202) (:dead-circumflex #\e))
(define-dead-key-combination (code-char 206) (:dead-circumflex #\i))
(define-dead-key-combination (code-char 212) (:dead-circumflex #\o))
(define-dead-key-combination (code-char 219) (:dead-circumflex #\u))
(define-dead-key-combination (code-char 226) (:dead-circumflex #\a))
(define-dead-key-combination (code-char 234) (:dead-circumflex #\e))
(define-dead-key-combination (code-char 238) (:dead-circumflex #\i))
(define-dead-key-combination (code-char 244) (:dead-circumflex #\o))
(define-dead-key-combination (code-char 251) (:dead-circumflex #\u))
(define-dead-key-combination (code-char 94) (:dead-circumflex #\space))

(defmacro handling-dead-keys ((gesture &optional restart) &body body)
  "Accumulate dead keys and subsequent characters. `Gesture'
should be a symbol bound to either a gesture or an input
event. When it has been determined that a sequence of `gesture's
either does or doesn't result in a full gesture, `body' will be
evaluated with `gesture' bound to that gesture. If `restart' is
true, start over with a new accumulation. If an `abort-gesture'
condition is signalled in `body', the accumulation will be
cleared."
  (with-gensyms (state-sym)
    `(retaining-value (,state-sym *dead-key-table*)
       (when ,restart
         (setf ,state-sym *dead-key-table*))
       (flet ((invoke-body (,gesture)
                (setf ,state-sym *dead-key-table*)
                (handler-case (progn ,@body)
                  (abort-gesture (c)
                    (setf ,state-sym *dead-key-table*)
                    (signal c)))))
         (if (typep ,gesture '(or keyboard-event character))
             (let ((value (gethash (if (characterp ,gesture)
                                       ,gesture
                                       (keyboard-event-key-name ,gesture))
                                   ,state-sym)))
               (etypecase value
                 (null
                  (if (eq ,state-sym *dead-key-table*)
                      (invoke-body ,gesture)
                      (setf ,state-sym *dead-key-table*)))
                 (character
                  (invoke-body value))
                 (hash-table
                  (setf ,state-sym value))))
             (invoke-body ,gesture))))))
