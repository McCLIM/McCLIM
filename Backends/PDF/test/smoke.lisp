;;;  (c) copyright 2019 Jan Moringen

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

(cl:in-package #:clim-pdf.test)

(in-suite :clim-pdf)

(test smoke
  "Smoke test for the PDF backend."

  (flet ((invoke-with-pdf-stream (continuation
                                  &key filename title scale-to-fit)
           (with-open-file (stream filename :direction :output
                                            :if-does-not-exist :create
                                            :if-exists :supersede
                                            :element-type '(unsigned-byte 8))
             (clim-pdf:with-output-to-pdf-stream
                 (stream stream :header-comments `(:title ,title)
                                :scale-to-fit scale-to-fit)
               (funcall continuation stream)))))
    (loop for i from 1
          for filename = (format nil "pdf-test-~D.pdf" i)
          for title = (format nil "Test Page ~D" i)
          for page in clim-test-util:*all-test-pages*
          do (finishes
               (invoke-with-pdf-stream page :filename filename :title title)))
    (finishes
      (clim-pdf:with-output-to-pdf-stream
          (stream "pdf-test-all.pdf" :header-comments `(:title "All test pages in one document"))
        (loop for page in clim-test-util:*all-test-pages* do
             (funcall page stream)
             (clim:new-page stream))))))
