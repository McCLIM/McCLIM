;;; -*- Mode: Lisp; Package: BEAGLE; -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000,2001 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2004 by
;;;           Duncan Rose (duncan@robotcat.demon.co.uk)

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

(in-package :beagle)

;; This seems not to work if we don't have "fixed" in the list
;; (Listener: Apropos graft fails since we need a 'fixed' family).
;; I don't think there should be a fixed family though, it seems not
;; to be in the spec (section 11.1).

;;; These should all be able to be defined by the user, possibly
;;; in some start-up file (implement 'profile' ;-)

(defconstant *beagle-text-families* '(:fix         "Courier"
 				      :serif       "Times New Roman"
				      :sans-serif  "Verdana"))

(defparameter *beagle-text-sizes* '(:normal         12.0
			  	   :tiny            9.0
				   :very-small     10.0
				   :small          11.0
				   :large          14.0
				   :very-large     18.0
				   :huge           24.0))

(defparameter *beagle-native-fonts* (make-hash-table :test #'equal))
(defparameter *beagle-font-metrics* (make-hash-table :test #'equal))

;; This is a hack; for some reason when I run the code I get problems looking up the text-style-*
;; attributes of NIL (which isn't suprising). I'm not sure where the NIL is coming from (which
;; is what I found suprising) so for now hack it so that if text-style is NIL, it gets set to
;; the default text-style.

(defun %text-style->beagle-font (text-style)
  ;;; Need to merge any incoming text styles with *default-text-style* - if provided text-style
  ;;; is NIL, get from *default-text-style*. If text-style is T, but the specific aspect we're
  ;;; interested in is NIL, get from the *default-text-style*. If all else fails, just pick a
  ;;; default (:fix :roman :normal).
  (let ((family (or (text-style-family (or text-style *default-text-style*))
		    (text-style-family *default-text-style*)
		    :fix))
	(face   (or (text-style-face (or text-style *default-text-style*))
		    (text-style-face *default-text-style*)
		    :roman))
	(size   (or (text-style-size (or text-style *default-text-style*))
		    (text-style-size *default-text-style*)
		    :normal)))
    ;; If we have this family-face-size combo cached already, return it. Otherwise, create a new one.
    (or (gethash (list family face size) *beagle-native-fonts*)
		(let* ((beagle-family (%make-nsstring (getf *beagle-text-families* family)))
		       (beagle-size (getf *beagle-text-sizes* size))
		       (beagle-font (send (@class ns-font)
					 :font-with-name beagle-family
					 :size beagle-size))
		       (font-manager (send (@class ns-font-manager) 'shared-font-manager)))
		  (case face
			((:bold)
			 (setf beagle-font 
			       (send font-manager :convert-font beagle-font
				     :to-have-trait #$NSBoldFontMask)))
			((:italic)
			 (setf beagle-font 
			       (send font-manager :convert-font beagle-font
				     :to-have-trait #$NSItalicFontMask)))
			((:bold-italic)
			 (setf beagle-font 
			       (send font-manager :convert-font beagle-font
				     :to-have-trait (logior #$NSBoldFontMask #$NSItalicFontMask))))
			((:italic-bold)
			 (setf beagle-font 
			       (send font-manager :convert-font beagle-font
				     :to-have-trait (logior #$NSBoldFontMask #$NSItalicFontMask)))))
		  (send beagle-font 'retain)
		  (setf (gethash (list family face size) *beagle-native-fonts*) beagle-font)
		  (send beagle-family 'release)
		  beagle-font))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; text-style-ascent text-style medium  [Generic function] 
;;; text-style-descent text-style medium [Generic function] 
;;; text-style-height text-style medium  [Generic function] 
;;; text-style-width text-style medium   [Generic function]

;;; Returns the ascent, descent, height, and width (respectively) of the font corresponding to
;;; the text style text-style as it would be rendered on the medium medium .text-style must be
;;; a fully specified text style. 

;;; The ascent of a font is the distance between the top of the tallest character in that
;;; font and the font's baseline.  The descent of a font is the distance between the baseline
;;; and the bottom of the lowest descending character (usually 'g', 'p', 'q', or 'y').
;;; The height of a font is the sum of the ascent and the descent of the font. The width of a
;;; font is the width of some representative character in the font. 

;;; The methods for these generic functions will typically specialize both the text-style and
;;; medium arguments. Implementations should also provide 'trampoline' for these generic
;;; functions on output sheets; the trampolines will simply call the method for the medium.

;;; Implementation note: caching the font metric information has made no difference to
;;; performance. It's possible that the caching can be improved. Caching doesn't adversly
;;; affect performance though so it's staying in.

#||
origin 0.0
     0.0+------------------------+
        | ^       ^              |
        | |height |ascent        |
        | |       |       width  |
        |<|-------|------------->|
        | |       v              |
   12.0 +-|----------------------+
        | |       ^descent       |
        | v       v              |
   15.0 +------------------------+

Baseline = 12.0
Height   = 15.0 == height
Ascent   = 12.0 == (height - (height - baseline)) == baseline
Descent  =  3.0 == (height - ascent)              == (height - baseline)
Width    = width
||#

(defun beagle-font-metrics (metric text-style medium &optional (char nil))
"Metric is one of :ascent :descent :width :height"
  (declare (special *beagle-font-metrics*))
  (let* ((string (if char
		     (string char)
		   "m"))
	 (key (cons text-style string))  ; possible to avoid consing?
	 (metrics (gethash key *beagle-font-metrics*)))
    (when (null metrics)
      (multiple-value-bind (width height x y baseline)
	  (text-size medium string :text-style text-style)
	(declare (ignore x y))
	(setf metrics (list `(:ascent . ,baseline)
			    `(:descent . ,(- height baseline))
			    `(:width . ,width)
			    `(:height . ,height))))
      (setf (gethash key *beagle-font-metrics*) metrics))
    (cdr (assoc metric metrics))))


(defmethod text-style-ascent (text-style (medium beagle-medium))
  (beagle-font-metrics :ascent text-style medium))

(defmethod text-style-descent (text-style (medium beagle-medium))
  (beagle-font-metrics :descent text-style medium))

(defmethod text-style-height (text-style (medium beagle-medium))
  (beagle-font-metrics :height text-style medium))

(defmethod text-style-width (text-style (medium beagle-medium))
  (beagle-font-metrics :width text-style medium))

(defmethod text-style-character-width (text-style (medium beagle-medium) char)
  (beagle-font-metrics :width text-style medium char))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; text-size medium string &key text-style (start 0) end [Generic function]

;;; Computes the ``cursor motion'' in device units that would take place if string (which
;;; may be either a string or a character) were output to the medium medium starting at the
;;; position (0,0) .  Five values are returned:  the total width of the string in device
;;; units, the total height of the string in device units, the final xcursor position (which
;;; is the same as the width if there are no #\Newline characters in the string), the final
;;; ycursor position (which is 0 if the string has no #\Newline characters in it, and is
;;; incremented by the line height of medium for each #\Newline character in the string), and
;;; the string's baseline. 

;;; text-style specifies what text style is to be used when doing the output, and defaults to
;;; medium-merged-text-style of the medium. text-style must be a fully specified text style.
;;; start and end may be used to specify a substring of string .

;;; If a programmer needs to account for kerning or the ascent or descent of the text style, he
;;; should measure the size of the bounding rectangle of the text rendered on medium .

;;; All mediums and output sheets must implement a method for this generic function.

(defmethod text-size ((medium beagle-medium) string &key text-style (start 0) end)
  (declare (special *default-text-style*))
  
  ;; Method can be passed either a string or a char; make sure for the latter
  ;; that we see only strings.
  (when (characterp string)
    (setf string (string string)))

  ;; Make sure there's an 'end' specified
  (unless end
    (setf end (length string)))

  ;; Make sure there's a text-style
  (unless text-style
    (setf text-style (medium-text-style medium)))

  ;; Check for 'empty string' case
  (if (>= start end)
      (values 0 0 0 0 0)
    (let ((position-newline (position #\newline string :start start))
	  ;; See if there's a better way to do this; is this stack
	  ;; allocation?
	  (objc-str (%make-nsstring (subseq string start end)))
	  (font (%text-style->beagle-font (or text-style
					      *default-text-style*))))
      (slet ((bsize (send objc-str :size-with-attributes
			  (reuse-attribute-dictionary medium font))))
	    ;; Don't use 'text-style-descent' in the following, since that
	    ;; method is defined in terms of this one :-)
        (let* ((descender (abs (send font 'descender)))
	       (fragment-width (pref bsize :<NSS>ize.width))
	       (fragment-height (pref bsize :<NSS>ize.height))
	       (fragment-x (pref bsize :<NSS>ize.width))
	       ;; subtract line height from this later...
	       (fragment-y (pref bsize :<NSS>ize.height))
	       ;; baseline = height - descender
	       (fragment-baseline (- fragment-height descender)))
	  (send objc-str 'release)
	  (if (null position-newline)
	      (values fragment-width
		      fragment-height
		      fragment-x
		      (- fragment-y fragment-height)
		      fragment-baseline)
	    (progn
	      (multiple-value-bind (w h x y b)
		  (text-size medium string :text-style text-style
			     :start position-newline
			     :end end)
		;; Current width, or width of sub-fragment, whichever
		;; is larger
		(let ((largest-width (max fragment-width w))
		      ;; current height + height of sub-fragment
		      (current+fragment-height (+ fragment-height h))
		      ;; new y position; one line height smaller than the
		      ;; total height
		      (y-position (- (+ fragment-y y) fragment-height))
		      ;; baseline of string; total height - baseline size, where
		      ;; baseline 'size' is (line-height - baseline).
		      (baseline (- (+ fragment-height h) (- h b))))
		  (values largest-width
			  current+fragment-height
			  x      ; always use last x calculated...
			  y-position
			  baseline))))))))))


;;; Note: we DO NOT want to draw the fonts in the medium-foreground colour - we want
;;; to draw them in a specific colour (unless McCLIM sets the medium foreground colour
;;; in order to achieve drawing elements in specific colours).

(let ((reusable-dict nil))
  ;; create a mutable dictionary on-demand and reuse it
  ;; CAUTION: this dictionary's fields get changed all the time. don't
  ;;          store it and expect to be able to later get back the values you
  ;;          put into it!
  ;; TODO: implement a WITH-FONT-ATTRIBUTES macro that reuses a lexically-hidden
  ;;       dictionary without returning it, and that is thread-safe so that
  ;;       other threads don;t come along and bash the dictionary in the middle
  ;;       of some operation
  (defun reuse-attribute-dictionary (medium font &key (colour nil))
    (or reusable-dict
	(setf reusable-dict (send (@class ns-mutable-dictionary) :dictionary-with-capacity 3))
	(send reusable-dict 'autorelease))
    (let ((fg-colour (or colour (%beagle-pixel (port medium) (medium-foreground medium))))
	  (bg-colour (%beagle-pixel (port medium) (medium-background medium))))
      (send reusable-dict :set-object font      
	    :for-key (ccl:%get-ptr (ccl::foreign-symbol-address "_NSFontAttributeName")))
      (send reusable-dict :set-object fg-colour 
	    :for-key (ccl:%get-ptr (ccl::foreign-symbol-address "_NSForegroundColorAttributeName")))
      (send  reusable-dict :set-object bg-colour 
	     :for-key (ccl:%get-ptr (ccl::foreign-symbol-address "_NSBackgroundColorAttributeName")))
      reusable-dict)))
