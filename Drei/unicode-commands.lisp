;;; -*- Mode: Lisp; Package: DREI-COMMANDS -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)

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

;;; Unicode handling for the editing component of the Climacs editor.

(in-package :drei-commands)

(do ((i 160 (+ i 1)))
    ((> i 255))
  (set-key `(com-self-insert ,*numeric-argument-marker*) 
           'self-insert-table (list (code-char i))))

(define-command (com-insert-charcode :name t :command-table self-insert-table)
    ((code 'integer :prompt "Code point") (count 'integer :default 1))
  (let ((char (code-char code)))
    (loop repeat count do (insert-character char))))

(macrolet 
    ((set-charcode-key (code sequence)
       `(set-key 
         `(com-insert-charcode ,',code ,*numeric-argument-marker*)
         'self-insert-table
         ',sequence))
     (set-dead-acute-key (code &rest sequence)
       `(set-charcode-key ,code ((:dead-acute) ,@sequence)))
     (set-dead-grave-key (code &rest sequence)
       `(set-charcode-key ,code ((:dead-grave) ,@sequence)))
     (set-dead-diaresis-key (code &rest sequence)
       `(set-charcode-key ,code ((:dead-diaeresis) ,@sequence)))
     (set-dead-tilde-key (code &rest sequence)
       `(set-charcode-key ,code ((:dead-tilde) ,@sequence)))
     (set-dead-circumflex-key (code &rest sequence)
       `(set-charcode-key ,code ((:dead-circumflex) ,@sequence))))
  (set-dead-acute-key 193 (#\A))
  (set-dead-acute-key 201 (#\E)) 
  (set-dead-acute-key 205 (#\I))
  (set-dead-acute-key 211 (#\O))
  (set-dead-acute-key 218 (#\U))
  (set-dead-acute-key 221 (#\Y))
  (set-dead-acute-key 225 (#\a))
  (set-dead-acute-key 233 (#\e))
  (set-dead-acute-key 237 (#\i))
  (set-dead-acute-key 243 (#\o))
  (set-dead-acute-key 250 (#\u))
  (set-dead-acute-key 253 (#\y))
  (set-dead-acute-key 199 (#\C))
  (set-dead-acute-key 231 (#\c))
  (set-dead-acute-key 215 (#\x))
  (set-dead-acute-key 247 (#\-))
  (set-dead-acute-key 222 (#\T))
  (set-dead-acute-key 254 (#\t))
  (set-dead-acute-key 223 (#\s))
  (set-dead-acute-key 39 (#\Space))

  (set-dead-acute-key 197 (:dead-acute) (#\A))
  (set-dead-acute-key 229 (:dead-acute) (#\a))

  (set-dead-grave-key 192 (#\A))
  (set-dead-grave-key 200 (#\E))
  (set-dead-grave-key 204 (#\I))
  (set-dead-grave-key 210 (#\O))
  (set-dead-grave-key 217 (#\U))
  (set-dead-grave-key 224 (#\a))
  (set-dead-grave-key 232 (#\e))
  (set-dead-grave-key 236 (#\i))
  (set-dead-grave-key 242 (#\o))
  (set-dead-grave-key 249 (#\u))
  (set-dead-grave-key 96 (#\Space))

  (set-dead-diaresis-key 196 (#\A))
  (set-dead-diaresis-key 203 (#\E))
  (set-dead-diaresis-key 207 (#\I))
  (set-dead-diaresis-key 214 (#\O))
  (set-dead-diaresis-key 220 (#\U))
  (set-dead-diaresis-key 228 (#\a))
  (set-dead-diaresis-key 235 (#\e))
  (set-dead-diaresis-key 239 (#\i))
  (set-dead-diaresis-key 246 (#\o))
  (set-dead-diaresis-key 252 (#\u))
  (set-dead-diaresis-key 255 (#\y))
  (set-dead-diaresis-key 34  (#\Space))

  (set-dead-tilde-key 195 (#\A))
  (set-dead-tilde-key 209 (#\N))
  (set-dead-tilde-key 227 (#\a))
  (set-dead-tilde-key 241 (#\n))

  (set-dead-tilde-key 198 (#\E))
  (set-dead-tilde-key 230 (#\e))
  (set-dead-tilde-key 208 (#\D))
  (set-dead-tilde-key 240 (#\d))
  (set-dead-tilde-key 248 (#\o))
  (set-dead-tilde-key 126 (#\Space))

  (set-dead-circumflex-key 194 (#\A))
  (set-dead-circumflex-key 202 (#\E))
  (set-dead-circumflex-key 206 (#\I))
  (set-dead-circumflex-key 212 (#\O))
  (set-dead-circumflex-key 219 (#\U))
  (set-dead-circumflex-key 226 (#\a))
  (set-dead-circumflex-key 234 (#\e))
  (set-dead-circumflex-key 238 (#\i))
  (set-dead-circumflex-key 244 (#\o))
  (set-dead-circumflex-key 251 (#\u))
  (set-dead-circumflex-key 94 (#\Space)))
