;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

(in-package :CLIM-INTERNALS)

(defclass standard-input-stream (fundamental-character-input-stream)
  ((unread-chars :initform nil
		 :accessor stream-unread-chars)
   )
  )

(defmethod stream-read-char ((pane standard-input-stream))
  (if (stream-unread-chars pane)
      (pop (stream-unread-chars pane))
    (loop for event = (event-read pane)
	if (and (typep event 'key-press-event)
		(characterp (keyboard-event-key-name event)))
	return (let ((char (keyboard-event-key-name event)))
		 (case char
		   (#\Return
		    (setq char #\Newline))
		   (#\Backspace
		    (setq char #\Delete)))
		 (stream-write-char pane char)
		 char)
	  else do (handle-event pane event))))

(defmethod stream-unread-char ((pane standard-input-stream) char)
  (push char (stream-unread-chars pane)))

(defmethod stream-read-char-no-hang ((pane standard-input-stream))
  (if (stream-unread-chars pane)
      (pop (stream-unread-chars pane))
    (loop for event = (event-read-no-hang pane)
	if (null event)
	   return nil
	if (and (typep event 'key-release-event)
		(characterp (keyboard-event-key-name event)))
	return (keyboard-event-key-name event))))
