
;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000, 2014 by 
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2005 by
;;;           Andreas Fuchs (asf@boinkor.net)
;;;
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

;;; The actual McCLIM system that people should to use in their ASDF
;;; package dependency lists.
(defsystem :mcclim
  :author "Alexey Dejneka
Andreas Fuchs
Andy Hefner
Arnaud Rouanet
Brian Mastenbrook
Brian Spilsbury
Christophe Rhodes
Clemens Fruhwirth
Daniel Barlow
Duncan Rose
Edena Pixel
Frank Buss
Gilbert Baumann
Iban Hatchondo
Julien Boninfan
Lionel Salabartan
Max-Gerd Retzlaff
Mike McDonald
Peter Mechleborg
Rainer Joswig
Robert Goldman
Robert Strandh
Rudi Schlatte
Timothy Moore
Daniel Kochmański"
  :license "LGPL-2.1+"
  :version "0.9.7"
  :description "McCLIM is an implementation of the CLIM 2.0 specification."
  :long-description "McCLIM is an implementation of the CLIM 2.0 specification.

CLIM (Common Lisp Interface Manager) is an advanced graphical user
interface management system."
  :depends-on (#:mcclim/looks #:mcclim/extensions))

;;; A system that loads the appropriate backend for the current
;;; platform.
(defsystem #:mcclim/looks
  :depends-on (#:clim
               #-(or mcclim-beagle mcclim-ugly)
                                #:mcclim-clx/pretty  #| adds truetype        |#
               #+mcclim-ugly    #:mcclim-clx         #| raw clim-clx backend |#
               #+mcclim-beagle  #:mcclim-beagle      #| OSX native (clozure) |#

               ;; null backend
               #:mcclim-null))

(defsystem #:mcclim/extensions
  :depends-on (#:mcclim-bitmaps
               #:conditional-commands
               #:mcclim-layouts/tab
               #:mcclim-bezier))

(defmethod perform :after ((op load-op) (c (eql (find-system :mcclim))))
  (pushnew :clim *features*)) ;; The fact that CLIM itself is available is true when all is loaded.

;; The fact that our CLIM implementation is McCLIM is already true now.
;; This feature is notably used by ESA and DREI, in cases where they need to
;; know whether they are compiled with McCLIM or another CLIM implementation.
(pushnew :mcclim *features*) 
