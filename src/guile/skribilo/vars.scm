;;;
;;; vars.scm	-- Skribe Globals
;;;
;;; Copyright © 2003-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;; Copyright 2005  Ludovic Courtès  <ludovic.courtes@laas.fr>
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


(define-module (skribilo vars))

;;;
;;; Switches
;;;
(define-public *skribe-verbose*	0)
(define-public *skribe-warning*	5)
(define-public *load-rc*		#t)

;;;
;;; PATH variables
;;;
(define-public *skribe-path*		#f)
(define-public *skribe-bib-path*	'("."))
(define-public *skribe-source-path*	'("."))
(define-public *skribe-image-path*	'("."))


(define-public *skribe-rc-directory*
  (string-append (getenv "HOME") "/" ".skribilo"))


;;;
;;; In and out ports
;;;
(define-public *skribe-src*		'())
(define-public *skribe-dest*		#f)

;;;
;;; Engine
;;;
(define-public *skribe-engine*	'html)	;; Use HTML by default

;;;
;;; Misc
;;;
(define-public *skribe-chapter-split*	'())
(define-public *skribe-ref-base*	#f)
(define-public *skribe-convert-image*  #f)	;; i.e. use the Skribe standard converter
(define-public *skribe-variants*	'())


