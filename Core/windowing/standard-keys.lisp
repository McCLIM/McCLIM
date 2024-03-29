;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2022 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Standard keys
;;;
;;; In this file we define the standard set of key names that must be uniform
;;; between backends - they are denoted by keywords. Key names correspond to
;;; real keys unaffected by the modifier state. The standardized keys are
;;; keywords with the following names:
;;;
;;; letters     A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
;;; digits      0 1 2 3 4 5 6 7 8 9
;;; misc        ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
;;; mod keys    SHIFT-LEFT SHIFT-RIGHT CONTROL-LEFT CONTROL-RIGHT
;;;             META-LEFT META-RIGHT ALT-LEFT ALT-RIGHT
;;;             SUPER-LEFT SUPER-RIGHT HYPER-LEFT HYPER-RIGHT
;;; lock keys   MODE-SWITCH NUM-LOCK CAPS-LOCK SCROLL-LOCK
;;; navigation  UP DOWN LEFT RIGHT PAGE-UP PAGE-DOWN HOME END MENU
;;; editing     RETURN BACKSPACE INSERT DELETE ESCAPE TAB SPACE
;;;
;;; Examples: :a :x :|3| :! :|,| :control-left :caps-lock :backspace
;;;
;;; Other key names should be symbols interned in a backend-specific package.

;;; Implementation details (relevant to backend developers):
;;; 
;;; The keyboard event is intialized with the KEY-NAME and the MODIFIER-STATE.
;;; Backend may optionally provide the initialization argument KEY-CHARACTER
;;; to provide an argument KEY-CHARACTER being a result of combining KEY-NAME
;;; with the MODIFIER-STATE under the current layout. If KEY-CHARACTER is
;;; _not_ supplied then McCLIM will try to perform a minimal translation:
;;;
;;; - [ none] letter, digit or misc - the corresponding character
;;; - [ none] :return #\Newline
;;; - [shift] letter - upcase letter character
;;; - [shift] digit or some of the interpunction
;;;           :|0| #\)    :|'| #\"    :|=| #\+
;;;           :|1| #\!    :|,| #\<
;;;           :|2| #\@    :|-| #\_
;;;           :|3| #\#    :|.| #\>
;;;           :|4| #\$    :|/| #\?
;;;           :|5| #\%    :|;| #\:
;;;           :|6| #\^    :|[| #\{
;;;           :|7| #\&    :|\| #\|
;;;           :|8| #\*    :|]| #\}
;;;           :|9| #\(    :|`| #\~
;;;
;;; Other combinations won't be translated to any character. If the backend
;;; decides to provice its own character value then McCLIM won't override it,
;;; so the programmer should not rely on this translation. If the backend
;;; supports alternative keyboard layouts then for example :y may yield #\z.
;;;
;;; This is an important clue - key names represent a physical layout of the
;;; keyboard while characters represent the logical layout. Depending on the
;;; scenario we want to define a gesture on a physical key or on a character:
;;;
;;;     (define-gesture-name :abort          :keyboard  (:c  :control))
;;;     (define-gesture-name :possibilities  :keyboard  (#\? :control))
;;;

(in-package #:clim-internals)

(defconstant +no-key+         #x0000)
(defconstant +shift-key+      #x0100)
(defconstant +control-key+    #x0200)
(defconstant +meta-key+       #x0400)   ; casually mapped from ALT-L
(defconstant +super-key+      #x0800)   ; "alien" key (or "win", "cmd")
(defconstant +hyper-key+      #x1000)   ; yet have to see it being used
(defconstant +alt-key+        #x2000)   ; not exported (hence not used)

(defparameter *standard-key-names*
  #(:|`| :|1| :|2| :|3| :|4| :|5| :|6| :|7| :|8| :|9| :|0| :|-| :|=|
    :|~| :|!| :|@| :|#| :|$| :|%| :|^| :|&| :|*| :|(| :|)| :|_| :|+|

    :|,| :|.| :|/| :|;| :|'| :|[| :|]| :|\\|
    :|<| :|>| :|?| :|:| :|"| :|{| :|}| :|\||

    :A :B :C :D :E :F :G :H :I :J :K :L :M :N :O :P :Q :R :S :T :U :V :W :X :Y :Z

    :SHIFT-LEFT :SHIFT-RIGHT :CONTROL-LEFT :CONTROL-RIGHT
    :META-LEFT  :META-RIGHT  :ALT-LEFT     :ALT-RIGHT
    :SUPER-LEFT :SUPER-RIGHT :HYPER-LEFT   :HYPER-RIGHT
    
    :MODE-SWITCH :NUM-LOCK :CAPS-LOCK :SCROLL-LOCK

    :UP :DOWN :LEFT :RIGHT :PAGE-UP :PAGE-DOWN :HOME :END :MENU
    :RETURN :BACKSPACE :INSERT :DELETE :ESCAPE :TAB :SPACE))

(defvar *standard-key-characters*
  "`1234567890-=~!@#$%^&*()_+,./;'[]\\<>?:\"{}|abcdefghijklmnopqrstuvwxyz")

(defvar *standard-key-characters/shift*
  "~!@#$%^&*()_+             <>?:\"{}|        ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun compute-keyboard-event-character (key-name modifier-state)
  (when-let* ((map (cond ((= modifier-state +no-key+)
                          (when (eq key-name :return)
                            (return-from compute-keyboard-event-character
                              #\newline))
                          *standard-key-characters*)
                         ((= modifier-state +shift-key+)
                          (when (eq key-name :return)
                            (return-from compute-keyboard-event-character
                              #\newline))
                          *standard-key-characters/shift*)))
              (pos (position key-name *standard-key-names*))
              (chr (and (< pos (length map)) (char map pos))))
    (if (char= chr #\nul)
        nil
        chr)))
