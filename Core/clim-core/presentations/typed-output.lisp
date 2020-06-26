;;; ---------------------------------------------------------------------------
;;;     Title: Typed output
;;;   Created: 2020-06-26 15:00
;;;    Author: Daniel Kochmański <daniel@turtleware.eu>
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2001-2002 by Tim Moore <moore@bricoworks.com>
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of the typed output.
;;;

(in-package #:clim-internals)

(defmacro with-output-as-presentation ((stream object type
                                        &rest key-args
                                        &key modifier single-box
                                          (allow-sensitive-inferiors t)
                                          parent
                                          (record-type
                                           ''standard-presentation)
                                        &allow-other-keys)
                                       &body body)
  (declare (ignore parent single-box modifier))
  (setq stream (stream-designator-symbol stream '*standard-output*))
  (multiple-value-bind (decls with-body)
      (get-body-declarations body)
    (with-gensyms (record-arg continuation)
      (with-keywords-removed (key-args (:record-type
                                        :allow-sensitive-inferiors))
        `(flet ((,continuation ()
                  ,@decls
                  ,@with-body))
           (declare (dynamic-extent #',continuation))
           (if (and (output-recording-stream-p ,stream)
                    *allow-sensitive-inferiors*)
               (with-new-output-record
                   (,stream ,record-type ,record-arg
                            :object ,object
                            :type (expand-presentation-type-abbreviation
                                   ,type)
                            ,@key-args)
                 (let ((*allow-sensitive-inferiors*
                         ,allow-sensitive-inferiors))
                   (,continuation)))
               (,continuation)))))))
