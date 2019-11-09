;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-
;;;
;;;  (c) copyright 2001 by Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;
;;; See toplevel file 'Copyright' for the copyright details.
;;;

;;; FIXME valid space specification format is described in the section
;;; describing a FORMATTING-TABLE macro. This should be generalized
;;; for other possible space definitions and described in a separate
;;; section. This functionality partially overlaps with a space
;;; specification format described for the layout macros like
;;; VERTICALLY, We should scram a superset of both in PARSE-SPACE and
;;; add a special handling for the non-stream panes. -- jd 2019-11-02

(in-package :clim-internals)

(deftype space-spec ()
  `(or real
       string
       character
       function
       (cons real
             (cons (member :character :line :point :pixel :mm)
                   null))))

(defun parse-space (stream specification direction)
  "Returns the amount of space given by SPECIFICATION relating to the
STREAM in the direction DIRECTION."
  ;; This implementation lives under the assumption that an
  ;; extended-output stream is also a sheet and has a graft.
  ;; --GB 2002-08-14
  (etypecase specification
    (real specification)
    ((or string character) (multiple-value-bind (width height)
                               (text-size stream (string specification))
                             (ecase direction
                               (:horizontal width)
                               (:vertical height))))
    (function (let ((record (with-output-to-output-record (stream)
                              (funcall specification))))
                (ecase direction
                  (:horizontal (bounding-rectangle-width record))
                  (:vertical (bounding-rectangle-height record)))))
    (cons
     (destructuring-bind (value unit)
         specification
       (ecase unit
         (:character
          (ecase direction
            (:horizontal (* value (stream-character-width stream #\M)))
            (:vertical   (* value (stream-line-height stream)))))
         (:line
          (ecase direction
            (:horizontal (* value (stream-line-width stream)))
            (:vertical   (* value (stream-line-height stream)))))
         ((:point :pixel :mm)
          (let* ((graft (graft stream))
                 (gunit (graft-units graft)))
            ;; mungle specification into what grafts talk about
            (case unit
              ((:point) (setf value (/ value 72) unit :inches))
              ((:pixel) (setf unit :device))
              ((:mm)    (setf unit :millimeters)))
            ;;
            (multiple-value-bind (dx dy)
                (multiple-value-call
                    #'transform-distance
                  (compose-transformation-with-scaling
                   (sheet-delta-transformation stream graft)
                   (/ (graft-width graft :units unit)
                      (graft-width graft :units gunit))
                   (/ (graft-height graft :units unit)
                      (graft-height graft :units gunit)))
                  (ecase direction
                    (:horizontal (values 1 0))
                    (:vertical   (values 0 1))))
              (/ value (sqrt (+ (* dx dx) (* dy dy))))))))))))

(defun valid-margin-spec-p (margins)
  (ignore-errors ; destructuring-bind may error; that yields invalid spec
   (destructuring-bind (&key left top right bottom) margins
     (flet ((margin-spec-p (margin)
              (destructuring-bind (anchor value) margin
                (and (member anchor '(:relative :absolute))
                     ;; Value must be a valid argument to PARSE-SPACE,
                     ;; not necessarily a number. -- jd 2019-10-31
                     (typep value 'space-spec)))))
       (every #'margin-spec-p (list left top right bottom))))))

(deftype margin-spec ()
  `(satisfies valid-margin-spec-p))

(defun normalize-margin-spec (plist defaults)
  (loop with plist = (copy-list plist)
        for edge in '(:left :top :right :bottom)
        for value = (getf plist edge)
        do
           (typecase value
             (null (setf (getf plist edge) (getf defaults edge)))
             (atom (setf (getf plist edge) `(:relative ,value)))
             (list #| do nothing |#))
        finally
           (check-type plist margin-spec)
           (return plist)))

