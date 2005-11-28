;;; location.scm -- Skribilo source location.
;;;
;;; Copyright 2003-2004  Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;; Copyright 2005  Ludovic Courtès <ludovic.courtes@laas.fr>
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;; USA.

(define-module (skribilo location)
  :use-module (oop goops)
  :use-module ((skribilo utils syntax) :select (%skribilo-module-reader))
  :export (<location> location? ast-location
	   location-file location-line location-pos))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; An abstract data type to keep track of source locations.
;;;
;;; Code:

(set-current-reader %skribilo-module-reader)


;;;
;;; Class definition.
;;;

(define-class <location> ()
  (file :init-keyword :file :getter location-file)
  (pos  :init-keyword :pos  :getter location-pos)
  (line :init-keyword :line :getter location-line))

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


;;; arch-tag: d68fa45d-a200-465e-a3c2-eb2861907f83

;;; location.scm ends here.
