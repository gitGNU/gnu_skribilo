;;; location.scm -- Skribilo source location.
;;;
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;; Copyright 2005, 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
;;;
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;;; USA.

(define-module (skribilo location)
  :use-module (oop goops)
  :use-module ((skribilo utils syntax) :select (%skribilo-module-reader))
  :export (<location> location? ast-location
	   location-file location-line location-column
           invocation-location))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; An abstract data type to keep track of source locations.
;;;
;;; Code:

(fluid-set! current-reader %skribilo-module-reader)


;;;
;;; Class definition.
;;;

(define-class <location> ()
  (file   :init-keyword :file   :getter location-file)
  (column :init-keyword :column :getter location-column)
  (line   :init-keyword :line   :getter location-line))

(define (location? obj)
  (is-a? obj <location>))

(define (ast-location obj)
  (let ((loc (slot-ref obj 'loc)))
    (if (location? loc)
	(let* ((fname (location-file loc))
	       (line  (location-line loc))
	       (pwd   (getcwd))
	       (len   (string-length pwd))
	       (lenf  (string-length fname))
	       (file  (if (and (substring=? pwd fname len)
			       (> lenf len))
			  (substring fname len (+ 1 (string-length fname)))
			  fname)))
	  (format #f "~a, line ~a" file line))
	"no source location")))

(define-method (write (loc <location>) port)
  (format port "#<<location> ~a \"~a\":~a:~a>"
          (object-address loc)
          (location-file loc)
          (location-line loc)
          (location-column loc)))



;;;
;;; Getting an invocation's location.
;;;

(define (invocation-location . depth)
  ;; Return a location object denoting the place of invocation of this
  ;; function's caller.
  (let ((depth (if (null? depth) 4 (car depth))))
    (let* ((stack  (make-stack #t))
           (frame  (stack-ref stack depth))
           (source (frame-source frame)))
      (and source
           (let ((file (source-property source 'filename))
                 (line (source-property source 'line))
                 (col  (source-property source 'column)))
             (and file
                  (make <location> :file file
                        :line (and line (+ line 1))
                        :column col)))))))

;;; arch-tag: d68fa45d-a200-465e-a3c2-eb2861907f83

;;; location.scm ends here.
